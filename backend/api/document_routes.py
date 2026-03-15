from fastapi import APIRouter, UploadFile, File, Form, HTTPException, status, Depends
import os
import shutil
import tempfile
import pdfplumber
import json
import logging
import threading
from typing import Optional
from db.connection import get_cursor
from auth.utils import get_current_user

logger = logging.getLogger("ledgerai.document_routes")

router = APIRouter()

UPLOAD_DIR = os.path.join(os.path.dirname(os.path.dirname(__file__)), "uploads")
os.makedirs(UPLOAD_DIR, exist_ok=True)


def _is_password_exception(exc: Exception) -> bool:
    """Check if an exception is password/encryption related.
    Handles PdfminerException which wraps PDFPasswordIncorrect with empty str().
    """
    # Check the string representation
    check_str = f"{str(exc)} {repr(exc)} {type(exc).__name__}".lower()

    # Also check the inner exception (PdfminerException wraps inner exceptions)
    if hasattr(exc, 'args') and exc.args:
        for arg in exc.args:
            check_str += f" {str(arg)} {type(arg).__name__}".lower()
    if hasattr(exc, '__cause__') and exc.__cause__:
        check_str += f" {str(exc.__cause__)} {type(exc.__cause__).__name__}".lower()

    keywords = ["password", "encrypt", "authenticate", "pdfminer", "pdfdocument",
                "pdfminerexception", "pdfpassword"]
    return any(kw in check_str for kw in keywords)


def _check_encryption_pypdf(tmp_path: str, password: str = None) -> str:
    """
    Use pypdf to reliably check if a PDF is encrypted.
    Returns 'PASSWORD_TEXT_PDF' if encrypted and no valid password given,
    or None if not encrypted / successfully decrypted.
    """
    try:
        from pypdf import PdfReader
    except ImportError:
        try:
            from PyPDF2 import PdfReader
        except ImportError:
            return None  # Can't check, skip

    try:
        reader = PdfReader(tmp_path)
        if reader.is_encrypted:
            if password:
                try:
                    result = reader.decrypt(password)
                    if result == 0:
                        return "PASSWORD_TEXT_PDF"
                    # Verify pdfplumber also works with this password
                    try:
                        with pdfplumber.open(tmp_path, password=password) as pdf:
                            _ = pdf.pages[0].extract_text() if pdf.pages else None
                    except Exception:
                        return "PASSWORD_TEXT_PDF"
                    return None  # Both libraries agree: decrypted OK
                except Exception:
                    return "PASSWORD_TEXT_PDF"
            else:
                # Encrypted PDF, no password provided → always ask for password
                return "PASSWORD_TEXT_PDF"
    except Exception:
        pass

    return None  # Not encrypted


def _detect_pdf_type_pdfplumber(tmp_path: str, password: str = None) -> str:
    """Primary content detection using pdfplumber. Returns pdf_type or raises on failure."""
    with pdfplumber.open(tmp_path, password=password) as pdf:
        has_text = False
        has_images = False

        for page in pdf.pages:
            try:
                text = page.extract_text()
                if text and text.strip():
                    has_text = True
            except Exception as e:
                # If text extraction fails with encryption/password errors, re-raise
                err_msg = str(e).lower()
                if any(kw in err_msg for kw in ["password", "encrypt", "pdfminer", "pdfdocument"]):
                    raise
                # Otherwise ignore this page's extraction failure
            try:
                if page.images or page.figures:
                    has_images = True
            except Exception:
                pass  # Some PDFs have issues with image extraction but are otherwise valid

        if not has_text and has_images:
            return "IMAGE_CONVERTED_PDF"
        elif not has_text and not has_images:
            return "SCANNED_PDF"
        elif has_text and has_images:
            return "HYBRID_PDF"
        else:
            return "TEXT_PDF"


def _detect_pdf_type_pypdf_content(tmp_path: str, password: str = None) -> str:
    """Fallback content detection using pypdf."""
    try:
        from pypdf import PdfReader
    except ImportError:
        try:
            from PyPDF2 import PdfReader
        except ImportError:
            raise ImportError("No fallback PDF reader available")

    reader = PdfReader(tmp_path)

    if reader.is_encrypted:
        if password:
            try:
                reader.decrypt(password)
            except Exception:
                return "PASSWORD_TEXT_PDF"
        else:
            return "PASSWORD_TEXT_PDF"

    has_text = False
    for page in reader.pages:
        text = page.extract_text()
        if text and text.strip():
            has_text = True
            break

    return "TEXT_PDF" if has_text else "SCANNED_PDF"


def _is_valid_pdf_binary(tmp_path: str) -> bool:
    """Basic binary header check — a valid PDF starts with %PDF."""
    try:
        with open(tmp_path, "rb") as f:
            header = f.read(1024)
            return b"%PDF" in header
    except Exception:
        return False


@router.post("/verify-type")
async def verify_pdf_type(file: UploadFile = File(...), password: Optional[str] = Form(None)):
    if not file.filename.lower().endswith('.pdf'):
        raise HTTPException(status_code=400, detail="Only PDF files are allowed.")

    logger.info("")
    logger.info("─" * 50)
    logger.info("📋 PDF TYPE DETECTION: %s", file.filename)
    logger.info("─" * 50)

    with tempfile.NamedTemporaryFile(delete=False, suffix=".pdf") as tmp:
        shutil.copyfileobj(file.file, tmp)
        tmp_path = tmp.name

    file_size = os.path.getsize(tmp_path)
    logger.info("   ├─ File size   : %s bytes", f"{file_size:,}")

    pdf_type = "TEXT_PDF"
    try:
        # ── STEP 0: Check encryption FIRST (most reliable) ──
        logger.info("   ├─ Step 0      : Checking encryption (pypdf)...")
        encryption_result = _check_encryption_pypdf(tmp_path, password)
        if encryption_result == "PASSWORD_TEXT_PDF":
            logger.info("   ├─ Detected    : Password-protected PDF ⚠️")
            logger.info("   └─ Final type  : PASSWORD_TEXT_PDF")
            logger.info("─" * 50)
            return {"filename": file.filename, "pdf_type": "PASSWORD_TEXT_PDF"}
        elif encryption_result is None:
            logger.info("   ├─ Encryption  : Not encrypted (or decrypted OK)")

        # ── STEP 1: Content analysis with pdfplumber ──
        try:
            logger.info("   ├─ Step 1      : Content analysis (pdfplumber)...")
            pdf_type = _detect_pdf_type_pdfplumber(tmp_path, password)
            logger.info("   ├─ Result      : %s ✅", pdf_type)
        except Exception as e1:
            logger.warning("   ├─ pdfplumber failed: %s (type: %s)", str(e1)[:120], type(e1).__name__)

            # Check if it's a password/encryption issue (handles empty str() exceptions)
            if _is_password_exception(e1):
                logger.info("   ├─ Detected    : Password-protected PDF (extraction-level)")
                pdf_type = "PASSWORD_TEXT_PDF"
            else:
                # ── STEP 2: Fallback content analysis with pypdf ──
                try:
                    logger.info("   ├─ Step 2      : Fallback content analysis (pypdf)...")
                    pdf_type = _detect_pdf_type_pypdf_content(tmp_path, password)
                    logger.info("   ├─ Result      : %s ✅", pdf_type)
                except ImportError:
                    logger.warning("   ├─ pypdf not installed, skipping fallback")
                    logger.info("   ├─ Step 3      : Binary header check...")
                    if _is_valid_pdf_binary(tmp_path):
                        logger.info("   ├─ Valid PDF header — treating as TEXT_PDF")
                        pdf_type = "TEXT_PDF"
                    else:
                        pdf_type = "CORRUPTED_PDF"
                except Exception as e2:
                    err_str_2 = str(e2).lower()
                    if _is_password_exception(e2):
                        pdf_type = "PASSWORD_TEXT_PDF"
                        logger.info("   ├─ Detected    : Password-protected PDF (via fallback)")
                    else:
                        logger.info("   ├─ Step 3      : Binary header check...")
                        if _is_valid_pdf_binary(tmp_path):
                            pdf_type = "TEXT_PDF"
                        else:
                            pdf_type = "CORRUPTED_PDF"

        logger.info("   └─ Final type  : %s", pdf_type)
        logger.info("─" * 50)
    finally:
        if os.path.exists(tmp_path):
            os.remove(tmp_path)

    return {"filename": file.filename, "pdf_type": pdf_type}


@router.post("/upload")
async def upload_and_process(
    file: UploadFile = File(...),
    password: Optional[str] = Form(None),
    user=Depends(get_current_user)
):
    """
    Full pipeline: Upload PDF → Insert into DB → Trigger processing engine.
    Returns the document_id immediately; processing runs in background.
    """
    import sys
    # Project root is 3 levels up from backend/api/document_routes.py
    project_root = os.path.dirname(os.path.dirname(os.path.dirname(__file__)))
    if project_root not in sys.path:
        sys.path.insert(0, project_root)

    user_id = user["user_id"]

    if not file.filename.lower().endswith('.pdf'):
        raise HTTPException(status_code=400, detail="Only PDF files are allowed.")

    logger.info("")
    logger.info("═" * 50)
    logger.info("📤 DOCUMENT UPLOAD: %s", file.filename)
    logger.info("═" * 50)
    logger.info("   ├─ user_id     : %s", user_id)
    logger.info("   ├─ password    : %s", "YES" if password else "NO")

    # Save file to disk
    file_path = os.path.join(UPLOAD_DIR, f"{user_id}_{file.filename}")
    with open(file_path, "wb") as f:
        shutil.copyfileobj(file.file, f)

    file_size = os.path.getsize(file_path)
    logger.info("   ├─ Saved to    : %s", file_path)
    logger.info("   ├─ File size   : %s bytes", f"{file_size:,}")

    # Insert into documents table
    with get_cursor(commit=True, dictionary=True) as (_, cursor):
        is_pw = password is not None and len(password) > 0
        cursor.execute("""
            INSERT INTO documents (user_id, file_name, file_path, is_password_protected, status)
            VALUES (%s, %s, %s, %s, 'UPLOADED')
        """, (user_id, file.filename, file_path, is_pw))
        document_id = cursor.lastrowid

        # Store password if provided
        if is_pw:
            cursor.execute("""
                INSERT INTO document_password (document_id, encrypted_password)
                VALUES (%s, %s)
            """, (document_id, password))

    logger.info("   ├─ document_id : %s", document_id)
    logger.info("   ├─ DB status   : UPLOADED")
    logger.info("   ├─ 🚀 Starting background processing thread...")

    # Trigger processing engine in background thread
    def run_processing():
        try:
            from services.processing_engine import process_document
            process_document(document_id)
        except Exception as e:
            logger.error("[ERROR] Processing failed for doc %s: %s", document_id, e)

    thread = threading.Thread(target=run_processing, daemon=True)
    thread.start()

    logger.info("   └─ ✅ Upload complete, processing started in background")
    logger.info("═" * 50)

    return {"document_id": document_id, "status": "PROCESSING", "message": "Document uploaded. Processing started."}


@router.get("/status/{document_id}")
async def get_document_status(document_id: int, user=Depends(get_current_user)):
    """Poll this endpoint to check processing progress."""
    user_id = user["user_id"]
    with get_cursor(dictionary=True) as (_, cursor):
        cursor.execute("""
            SELECT document_id, status, transaction_parsed_type, file_name
            FROM documents
            WHERE document_id = %s AND user_id = %s
        """, (document_id, user_id))
        doc = cursor.fetchone()
        if not doc:
            raise HTTPException(status_code=404, detail="Document not found")
        return doc


@router.get("/stats")
async def get_document_stats(user=Depends(get_current_user)):
    user_id = user["user_id"]
    with get_cursor(dictionary=True) as (_, cursor):
        cursor.execute("""
            SELECT 
                COUNT(*) as total,
                SUM(CASE WHEN status = 'APPROVE' THEN 1 ELSE 0 END) as parsed,
                SUM(CASE WHEN status = 'FAILED' THEN 1 ELSE 0 END) as failed,
                SUM(CASE WHEN status = 'AWAITING_REVIEW' THEN 1 ELSE 0 END) as pending_review
            FROM documents 
            WHERE user_id = %s
        """, (user_id,))
        stats = cursor.fetchone()
        return {
            "total": stats["total"] or 0,
            "parsed": int(stats["parsed"] or 0),
            "failed": int(stats["failed"] or 0),
            "pending_review": int(stats["pending_review"] or 0)
        }


@router.get("/recent")
async def get_recent_documents(user=Depends(get_current_user)):
    user_id = user["user_id"]
    with get_cursor(dictionary=True) as (_, cursor):
        cursor.execute("""
            SELECT 
                d.document_id,
                d.file_name,
                d.status,
                d.transaction_parsed_type,
                d.created_at,
                s.institution_name
            FROM documents d
            LEFT JOIN statement_categories s ON d.statement_id = s.statement_id
            WHERE d.user_id = %s
            ORDER BY d.created_at DESC
            LIMIT 20
        """, (user_id,))
        rows = cursor.fetchall()
        return rows


@router.get("/{document_id}/review")
async def get_document_review(document_id: int, user=Depends(get_current_user)):
    user_id = user["user_id"]
    with get_cursor(dictionary=True) as (_, cursor):
        cursor.execute("""
            SELECT d.*, s.institution_name as bank_name, s.statement_identifier
            FROM documents d
            LEFT JOIN statement_categories s ON d.statement_id = s.statement_id
            WHERE d.document_id = %s AND d.user_id = %s
        """, (document_id, user_id))
        doc = cursor.fetchone()

        if not doc:
            raise HTTPException(status_code=404, detail="Document not found")

        # Get ALL staging transactions for this document (both CODE and LLM)
        cursor.execute("""
            SELECT staging_transaction_id, transaction_json, parser_type, overall_confidence
            FROM ai_transactions_staging 
            WHERE document_id = %s
        """, (document_id,))
        staging_rows = cursor.fetchall()

        code_txns = []
        llm_txns = []
        for row in staging_rows:
            txn_data = row["transaction_json"]
            if isinstance(txn_data, str):
                txn_data = json.loads(txn_data)
            if row["parser_type"] == "CODE":
                code_txns = txn_data
            else:
                llm_txns = txn_data

        # Parse identifier JSON
        ident_json = doc.get("statement_identifier")
        if isinstance(ident_json, str):
            try:
                ident_json = json.loads(ident_json)
            except:
                pass

        return {
            "bank_name": doc.get("bank_name") or "Pending Identification",
            "identifier_json": ident_json,
            "code_transactions": code_txns,
            "llm_transactions": llm_txns,
            "status": doc["status"]
        }


@router.post("/{document_id}/approve")
async def approve_document(document_id: int, user=Depends(get_current_user)):
    """
    Approve a document:
      1. Update document status  AWAITING_REVIEW → APPROVE
      2. Copy parsed transactions into uncategorized_transactions
    """
    user_id = user["user_id"]

    with get_cursor(commit=True, dictionary=True) as (_, cursor):
        # ── Verify ownership & current status ──
        cursor.execute(
            "SELECT document_id, status, statement_id FROM documents WHERE document_id = %s AND user_id = %s",
            (document_id, user_id),
        )
        doc = cursor.fetchone()
        if not doc:
            raise HTTPException(status_code=404, detail="Document not found")

        if doc["status"] == "APPROVE":
            return {"message": "Already approved", "inserted": 0}

        statement_id = doc.get("statement_id")

        # ── Fetch staging rows ──
        cursor.execute(
            "SELECT staging_transaction_id, transaction_json, parser_type "
            "FROM ai_transactions_staging WHERE document_id = %s",
            (document_id,),
        )
        staging_rows = cursor.fetchall()
        if not staging_rows:
            raise HTTPException(status_code=400, detail="No staging transactions to approve")

        # ── Pick best source: prefer CODE, fall back to LLM ──
        chosen_row = None
        for row in staging_rows:
            if row["parser_type"] == "CODE":
                chosen_row = row
                break
        if chosen_row is None:
            chosen_row = staging_rows[0]

        staging_id = chosen_row["staging_transaction_id"]
        txn_data = chosen_row["transaction_json"]
        if isinstance(txn_data, str):
            txn_data = json.loads(txn_data)

        # ── Insert each transaction into uncategorized_transactions ──
        inserted = 0
        for txn in txn_data:
            cursor.execute(
                """
                INSERT INTO uncategorized_transactions
                    (user_id, document_id, statement_id, staging_transaction_id,
                     txn_date, debit, credit, balance, description, confidence)
                VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
                """,
                (
                    user_id,
                    document_id,
                    statement_id,
                    staging_id,
                    txn.get("date"),
                    txn.get("debit"),
                    txn.get("credit"),
                    txn.get("balance"),
                    txn.get("details", "")[:500],
                    txn.get("confidence", 0.0),
                ),
            )
            inserted += 1

        # ── Update document status ──
        cursor.execute(
            "UPDATE documents SET status = 'APPROVE' WHERE document_id = %s",
            (document_id,),
        )

        logger.info(
            "✅ Document %s approved by user %s — %d transactions saved to uncategorized_transactions",
            document_id, user_id, inserted,
        )

    return {"message": "Document approved", "inserted": inserted}
