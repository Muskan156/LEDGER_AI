from fastapi import APIRouter, UploadFile, File, Form, HTTPException, status, Depends
from fastapi.responses import JSONResponse
import os
import shutil
import tempfile
import pdfplumber
import json
import logging
import threading
import uuid
import hashlib
from typing import Optional
from db.connection import get_client
from auth.utils import get_current_user

logger = logging.getLogger("ledgerai.document_routes")

router = APIRouter()

UPLOAD_DIR = os.path.join(os.path.dirname(os.path.dirname(__file__)), "uploads")
os.makedirs(UPLOAD_DIR, exist_ok=True)


def _is_password_exception(exc: Exception) -> bool:
    check_str = f"{str(exc)} {repr(exc)} {type(exc).__name__}".lower()
    if hasattr(exc, 'args') and exc.args:
        for arg in exc.args:
            check_str += f" {str(arg)} {type(arg).__name__}".lower()
    if hasattr(exc, '__cause__') and exc.__cause__:
        check_str += f" {str(exc.__cause__)} {type(exc.__cause__).__name__}".lower()
    keywords = ["password", "encrypt", "authenticate", "pdfminer", "pdfdocument",
                "pdfminerexception", "pdfpassword"]
    return any(kw in check_str for kw in keywords)


def _check_encryption_pypdf(tmp_path: str, password: str = None) -> str:
    try:
        from pypdf import PdfReader
    except ImportError:
        try:
            from PyPDF2 import PdfReader
        except ImportError:
            return None
    try:
        reader = PdfReader(tmp_path)
        if reader.is_encrypted:
            if password:
                try:
                    result = reader.decrypt(password)
                    if result == 0:
                        return "PASSWORD_TEXT_PDF"
                    try:
                        with pdfplumber.open(tmp_path, password=password) as pdf:
                            _ = pdf.pages[0].extract_text() if pdf.pages else None
                    except Exception:
                        return "PASSWORD_TEXT_PDF"
                    return None
                except Exception:
                    return "PASSWORD_TEXT_PDF"
            else:
                return "PASSWORD_TEXT_PDF"
    except Exception:
        pass
    return None


def _detect_pdf_type_pdfplumber(tmp_path: str, password: str = None) -> str:
    with pdfplumber.open(tmp_path, password=password) as pdf:
        has_text = False
        has_images = False
        for page in pdf.pages:
            try:
                text = page.extract_text()
                if text and text.strip():
                    has_text = True
            except Exception as e:
                err_msg = str(e).lower()
                if any(kw in err_msg for kw in ["password", "encrypt", "pdfminer", "pdfdocument"]):
                    raise
            try:
                if page.images or page.figures:
                    has_images = True
            except Exception:
                pass
        if not has_text and has_images:
            return "IMAGE_CONVERTED_PDF"
        elif not has_text and not has_images:
            return "SCANNED_PDF"
        elif has_text and has_images:
            return "HYBRID_PDF"
        else:
            return "TEXT_PDF"


def _detect_pdf_type_pypdf_content(tmp_path: str, password: str = None) -> str:
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
        logger.info("   ├─ Step 0      : Checking encryption (pypdf)...")
        encryption_result = _check_encryption_pypdf(tmp_path, password)
        if encryption_result == "PASSWORD_TEXT_PDF":
            logger.info("   ├─ Detected    : Password-protected PDF ⚠️")
            logger.info("   └─ Final type  : PASSWORD_TEXT_PDF")
            return {"filename": file.filename, "pdf_type": "PASSWORD_TEXT_PDF"}
        elif encryption_result is None:
            logger.info("   ├─ Encryption  : Not encrypted (or decrypted OK)")

        try:
            logger.info("   ├─ Step 1      : Content analysis (pdfplumber)...")
            pdf_type = _detect_pdf_type_pdfplumber(tmp_path, password)
            logger.info("   ├─ Result      : %s ✅", pdf_type)
        except Exception as e1:
            logger.warning("   ├─ pdfplumber failed: %s (type: %s)", str(e1)[:120], type(e1).__name__)
            if _is_password_exception(e1):
                pdf_type = "PASSWORD_TEXT_PDF"
            else:
                try:
                    pdf_type = _detect_pdf_type_pypdf_content(tmp_path, password)
                    logger.info("   ├─ Result      : %s ✅", pdf_type)
                except ImportError:
                    pdf_type = "TEXT_PDF" if _is_valid_pdf_binary(tmp_path) else "CORRUPTED_PDF"
                except Exception as e2:
                    if _is_password_exception(e2):
                        pdf_type = "PASSWORD_TEXT_PDF"
                    else:
                        pdf_type = "TEXT_PDF" if _is_valid_pdf_binary(tmp_path) else "CORRUPTED_PDF"

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
    backend_root = os.path.dirname(os.path.dirname(__file__))
    if backend_root not in sys.path:
        sys.path.insert(0, backend_root)

    user_id = user["user_id"]

    if not file.filename.lower().endswith('.pdf'):
        raise HTTPException(status_code=400, detail="Only PDF files are allowed.")

    logger.info("")
    logger.info("═" * 50)
    logger.info("📤 DOCUMENT UPLOAD: %s", file.filename)
    logger.info("═" * 50)
    logger.info("   ├─ user_id     : %s", user_id)
    logger.info("   ├─ password    : %s", "YES" if password else "NO")

    unique_token = uuid.uuid4().hex
    file_hash = hashlib.sha256(f"{user_id}_{file.filename}_{unique_token}".encode()).hexdigest()[:24]
    safe_filename = f"{file_hash}.pdf"
    file_path = os.path.join(UPLOAD_DIR, safe_filename)
    with open(file_path, "wb") as f:
        shutil.copyfileobj(file.file, f)

    file_size = os.path.getsize(file_path)
    logger.info("   ├─ Saved to    : %s", file_path)
    logger.info("   ├─ File size   : %s bytes", f"{file_size:,}")

    # Insert into documents table via Supabase client
    sb = get_client()
    is_pw = bool(password)

    doc_result = sb.table("documents").insert({
        "user_id": user_id,
        "file_name": file.filename,
        "file_path": file_path,
        "is_password_protected": is_pw,
        "status": "UPLOADED",
    }).execute()
    document_id = doc_result.data[0]["document_id"]

    if is_pw:
        sb.table("document_password").insert({
            "document_id": document_id,
            "encrypted_password": password,
        }).execute()

    logger.info("   ├─ document_id : %s", document_id)
    logger.info("   ├─ DB status   : UPLOADED")
    logger.info("   ├─ 🚀 Starting background processing thread...")

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
    user_id = user["user_id"]
    sb = get_client()
    result = (
        sb.table("documents")
        .select("document_id, status, transaction_parsed_type, file_name")
        .eq("document_id", document_id)
        .eq("user_id", user_id)
        .maybe_single()
        .execute()
    )
    if not result.data:
        raise HTTPException(status_code=404, detail="Document not found")
    return result.data


@router.get("/stats")
async def get_document_stats(user=Depends(get_current_user)):
    user_id = user["user_id"]
    sb = get_client()
    result = (
        sb.table("documents")
        .select("status")
        .eq("user_id", user_id)
        .execute()
    )
    rows = result.data or []
    total = len(rows)
    parsed = sum(1 for r in rows if r["status"] == "APPROVE")
    failed = sum(1 for r in rows if r["status"] == "FAILED")
    pending_review = sum(1 for r in rows if r["status"] == "AWAITING_REVIEW")
    return {"total": total, "parsed": parsed, "failed": failed, "pending_review": pending_review}


@router.get("/recent")
async def get_recent_documents(user=Depends(get_current_user)):
    user_id = user["user_id"]
    sb = get_client()
    result = (
        sb.table("documents")
        .select(
            "document_id, file_name, status, transaction_parsed_type, created_at, "
            "statement_categories(institution_name)"
        )
        .eq("user_id", user_id)
        .order("created_at", desc=True)
        .limit(20)
        .execute()
    )
    # Flatten the nested statement_categories join
    rows = []
    for r in (result.data or []):
        cat = r.pop("statement_categories", None) or {}
        r["institution_name"] = cat.get("institution_name")
        rows.append(r)
    return rows


@router.get("/{document_id}/review")
async def get_document_review(document_id: int, user=Depends(get_current_user)):
    user_id = user["user_id"]
    sb = get_client()

    doc_result = (
        sb.table("documents")
        .select("*, statement_categories(institution_name, statement_identifier)")
        .eq("document_id", document_id)
        .eq("user_id", user_id)
        .maybe_single()
        .execute()
    )
    if not doc_result.data:
        raise HTTPException(status_code=404, detail="Document not found")
    doc = doc_result.data

    # Get staging transactions
    staging_result = (
        sb.table("ai_transactions_staging")
        .select("staging_transaction_id, transaction_json, parser_type, overall_confidence")
        .eq("document_id", document_id)
        .execute()
    )
    staging_rows = staging_result.data or []

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

    cat = doc.pop("statement_categories", None) or {}
    bank_name = cat.get("institution_name") or "Pending Identification"
    ident_json = cat.get("statement_identifier")

    return {
        "bank_name": bank_name,
        "identifier_json": ident_json,
        "code_transactions": code_txns,
        "llm_transactions": llm_txns,
        "status": doc["status"],
    }


@router.post("/{document_id}/approve")
async def approve_document(document_id: int, user=Depends(get_current_user)):
    user_id = user["user_id"]
    sb = get_client()

    doc_result = (
        sb.table("documents")
        .select("document_id, status, statement_id")
        .eq("document_id", document_id)
        .eq("user_id", user_id)
        .maybe_single()
        .execute()
    )
    if not doc_result.data:
        raise HTTPException(status_code=404, detail="Document not found")
    doc = doc_result.data

    if doc["status"] == "APPROVE":
        return {"message": "Already approved", "inserted": 0}

    # Fetch staging rows
    staging_result = (
        sb.table("ai_transactions_staging")
        .select("staging_transaction_id, transaction_json, parser_type")
        .eq("document_id", document_id)
        .execute()
    )
    staging_rows = staging_result.data or []
    if not staging_rows:
        raise HTTPException(status_code=400, detail="No staging transactions to approve")

    # Prefer CODE over LLM
    chosen_row = next((r for r in staging_rows if r["parser_type"] == "CODE"), staging_rows[0])
    staging_id = chosen_row["staging_transaction_id"]
    txn_data = chosen_row["transaction_json"]
    if isinstance(txn_data, str):
        txn_data = json.loads(txn_data)

    # Insert into uncategorized_transactions
    rows = [
        {
            "user_id": user_id,
            "account_id": None,
            "document_id": document_id,
            "staging_transaction_id": staging_id,
            "txn_date": txn.get("date"),
            "debit": txn.get("debit"),
            "credit": txn.get("credit"),
            "balance": txn.get("balance"),
            "details": (txn.get("details") or "")[:500],
        }
        for txn in txn_data
    ]
    if rows:
        sb.table("uncategorized_transactions").insert(rows).execute()

    sb.table("documents").update({"status": "APPROVE"}).eq("document_id", document_id).execute()

    inserted = len(rows)
    logger.info("✅ Document %s approved by user %s — %d transactions saved", document_id, user_id, inserted)
    return {"message": "Document approved", "inserted": inserted}


@router.delete("/{document_id}")
async def delete_document(document_id: int, user=Depends(get_current_user)):
    user_id = user["user_id"]
    sb = get_client()

    doc_result = (
        sb.table("documents")
        .select("document_id, file_path, status")
        .eq("document_id", document_id)
        .eq("user_id", user_id)
        .maybe_single()
        .execute()
    )
    if not doc_result.data:
        raise HTTPException(status_code=404, detail="Document not found")

    file_path = doc_result.data.get("file_path")
    if file_path and os.path.exists(file_path):
        try:
            os.remove(file_path)
            logger.info("🗑️ Deleted file: %s", file_path)
        except Exception as e:
            logger.warning("Could not delete file %s: %s", file_path, e)

    # Delete child records then the document
    sb.table("ai_transactions_staging").delete().eq("document_id", document_id).execute()
    sb.table("uncategorized_transactions").delete().eq("document_id", document_id).execute()
    sb.table("documents").delete().eq("document_id", document_id).eq("user_id", user_id).execute()

    logger.info("🗑️ Document %s deleted by user %s", document_id, user_id)
    return {"message": "Document deleted successfully"}


@router.get("/{document_id}/download-json")
async def download_transactions_json(document_id: int, user=Depends(get_current_user)):
    user_id = user["user_id"]
    sb = get_client()

    doc_result = (
        sb.table("documents")
        .select("document_id, file_name, transaction_parsed_type")
        .eq("document_id", document_id)
        .eq("user_id", user_id)
        .maybe_single()
        .execute()
    )
    if not doc_result.data:
        raise HTTPException(status_code=404, detail="Document not found")
    doc = doc_result.data

    preferred_parser = doc.get("transaction_parsed_type") or "CODE"

    staging_result = (
        sb.table("ai_transactions_staging")
        .select("transaction_json, parser_type")
        .eq("document_id", document_id)
        .execute()
    )
    staging_rows = staging_result.data or []
    if not staging_rows:
        raise HTTPException(status_code=404, detail="No extracted transactions found for this document")

    chosen_json = None
    fallback_json = None
    for row in staging_rows:
        txn_data = row["transaction_json"]
        if isinstance(txn_data, str):
            txn_data = json.loads(txn_data)
        if row["parser_type"] == preferred_parser:
            chosen_json = txn_data
        else:
            fallback_json = txn_data

    result_json = chosen_json if chosen_json is not None else fallback_json
    if result_json is None:
        raise HTTPException(status_code=404, detail="No transaction JSON available")

    safe_name = doc["file_name"].replace(".pdf", "").replace(" ", "_")
    return JSONResponse(
        content={
            "document_id": document_id,
            "file_name": doc["file_name"],
            "parser_type": preferred_parser,
            "transaction_count": len(result_json),
            "transactions": result_json,
        },
        headers={
            "Content-Disposition": f'attachment; filename="{safe_name}_transactions.json"'
        }
    )
