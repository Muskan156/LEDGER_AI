import json
import logging
import re
from concurrent.futures import ThreadPoolExecutor

from services.pdf_service import extract_pdf_text
from services.identifier_service import (
    classify_document_llm,
    check_format_exists,
    save_new_statement_format,
)
from services.extraction_service import (
    generate_extraction_logic_llm,
    extract_transactions_using_logic,
)
from services.llm_parser import parse_with_llm
from services.validation_service import (
    validate_transactions,
    extract_json_from_response,
    validate_extraction_propriety,
    validate_code_quality_strict,       
)
from repository.document_repo import (
    get_document,
    get_document_password,
    update_processing_start,
    update_document_status,
    insert_audit,
    insert_text_extraction,
    update_document_statement,
    update_processing_complete,
    insert_staging_transactions,
    insert_staging_code_only,
)
from repository.statement_category_repo import (
    activate_statement_category,
    update_statement_status,
    update_success_rate,
)

logger = logging.getLogger("ledgerai.processing_engine")


# ═══════════════════════════════════════════════════════════
# MAIN PIPELINE
# ═══════════════════════════════════════════════════════════

def process_document(document_id: int, override_file_path: str = None):
    """
    Main entry point. Called after document is inserted into DB.
    Runs the complete pipeline: extract → identify → parse → validate → stage.

    override_file_path: if provided, use this local path for PDF extraction
    instead of the file_path stored in the DB. This keeps the DB file_path
    pointing to the permanent Supabase Storage path at all times.
    """

    try:
        # ─────────────────────────────────────────────────────
        # STEP 1 — FETCH DOCUMENT
        # ─────────────────────────────────────────────────────
        logger.info("")
        logger.info("═" * 70)
        logger.info("  PIPELINE START — document_id=%s", document_id)
        logger.info("═" * 70)

        doc = get_document(document_id)
        if not doc:
            raise ValueError(f"Document {document_id} not found.")

        # Use override_file_path (local temp file) if provided,
        # otherwise fall back to the DB path (Supabase Storage path).
        # Never patch the DB file_path — that caused file_path to go NULL.
        file_path = override_file_path or doc["file_path"]
        user_id   = doc["user_id"]
        password  = get_document_password(document_id)

        logger.info("[STEP 1/5] Document fetched")
        logger.info("file      : %s", doc["file_name"])
        logger.info("path      : %s", file_path)
        logger.info("user_id   : %s", user_id)
        logger.info("password  : %s", "YES" if password else "NO")
        logger.info("═" * 70)

        update_processing_start(document_id)
        insert_audit(document_id, "PROCESSING")

        # ─────────────────────────────────────────────────────
        # STEP 2 — EXTRACT TEXT FROM PDF
        # ─────────────────────────────────────────────────────
        logger.info("")
        logger.info("[STEP 2/5] Extracting text from PDF...")
        update_document_status(document_id, "EXTRACTING_TEXT")

        full_text = extract_pdf_text(file_path, password)
        if not full_text:
            raise ValueError("PDF extraction returned empty text.")
        sample_text = full_text[:20000]
        # Split full_text into per-page list using the === PAGE N === separators
        # that EnhancedFinancialPDFExtractor produces
        pages = [
            block.strip()
            for block in re.split(r'={80}', full_text)
            if block.strip() and not re.fullmatch(r'\s*PAGE\s+\d+\s*', block.strip(), re.IGNORECASE)
        ]
        if not pages:
            pages = [full_text]


        logger.info("pages     : %d", len(pages))
        logger.info("chars     : %d", len(full_text))
        logger.info("Text extracted successfully")
        logger.info("═" * 70)

        insert_text_extraction(document_id, full_text)
        update_document_status(document_id, "IDENTIFYING_FORMAT")

        # ─────────────────────────────────────────────────────
        # STEP 3 — CLASSIFY via LLM, then check DB for existing match
        # check_format_exists needs the identifier_json (institution +
        # format_name + table columns), so we always classify first.
        # ─────────────────────────────────────────────────────
        logger.info("")
        logger.info("[STEP 3/5] Generating identification markers via LLM...")

        identity_json = classify_document_llm(pages)

        logger.info("Family      : %s", identity_json.get("document_family", "?"))
        logger.info("Institution : %s", identity_json.get("institution_name", "?"))
        logger.info("Layout      : %s", identity_json.get("parsing_hints", {}).get("layout_type", "?"))
        logger.info("Boundaries  : %s", identity_json.get("parsing_hints", {}).get("transaction_boundary_signals"))
        logger.info("ID          : %s", identity_json.get("id"))
        logger.info("Identification markers generated")
        logger.info("═" * 70)

        logger.info("")
        logger.info("[STEP 3b/5] Checking if format exists in database...")
        existing = check_format_exists(identity_json)
        matched  = existing is not None

        if matched:
            logger.info("EXISTING FORMAT DETECTED")
            logger.info("format    : %s", existing.get("format_name", "?"))
            logger.info("statement : %s", existing.get("statement_id"))
            logger.info("status    : %s", existing.get("status"))
        else:
            logger.info("NO MATCHING FORMAT — new format will be saved")
        logger.info("═" * 70)

        update_document_status(document_id, "PARSING_TRANSACTIONS")

        # ═══════════════════════════════════════════════════
        # CASE A — FORMAT EXISTS IN DB
        # ═══════════════════════════════════════════════════
        if matched:
            # Use the stored identifier_json and extraction code from DB
            identity_json    = existing.get("statement_identifier", {})
            extraction_code  = existing["extraction_logic"]
            statement_id     = existing["statement_id"]
            statement_status = existing.get("status")

            update_document_statement(document_id, statement_id)

            # ── CASE A1 — ACTIVE → Fast path (CODE ONLY, no LLM) ──
            if statement_status == "ACTIVE":
                logger.info("")
                logger.info("[STEP 4/5] ACTIVE format — running stored extraction code (fast path)...")
                logger.info("Skipping LLM (format is trusted)")

                code_txns = extract_transactions_using_logic(full_text, extraction_code)
                logger.info("Code extracted %d transactions", len(code_txns))

                propriety_ok = validate_extraction_propriety(code_txns)
                strict_ok    = validate_code_quality_strict(code_txns)   # BUG-02 fix

                if propriety_ok and strict_ok:
                    logger.info("[STEP 5/5] PIPELINE COMPLETE — CODE (ACTIVE fast-path)")
                    update_processing_complete(document_id, "CODE")
                    insert_staging_code_only(document_id, user_id, code_txns, 100.0)
                    update_document_status(document_id, "AWAITING_REVIEW")
                    insert_audit(document_id, "COMPLETED")
                    logger.info("═" * 70)
                    return  # ← EXIT fast path

                else:
                    logger.warning(
                        "ACTIVE code produced improper transactions "
                        "(propriety=%s strict=%s) — downgrading to UNDER_REVIEW",
                        propriety_ok, strict_ok
                    )
                    update_statement_status(statement_id, "UNDER_REVIEW")
                    # Fall through to dual pipeline below

            # ── CASE A2 — UNDER_REVIEW / EXPERIMENTAL → Fall through to dual pipeline ──
            logger.info("Format status is %s — continuing to dual pipeline...", statement_status)

        # ═══════════════════════════════════════════════════
        # CASE B — NEW FORMAT → GENERATE EXTRACTION CODE + SAVE
        # ═══════════════════════════════════════════════════
        else:
            logger.info("")
            logger.info("[STEP 3c/5] Generating extraction code via LLM...")
            extraction_code = generate_extraction_logic_llm(
                identifier_json=identity_json,
                text_sample=sample_text,
            )
            logger.info("Extraction code generated (%d chars)", len(extraction_code))

            logger.info("")
            logger.info("[STEP 3d/5] Saving new format to database...")
            statement_id = save_new_statement_format(
                format_name=identity_json.get("id", "AUTO_FORMAT"),
                identifier_json=identity_json,
                extraction_logic=extraction_code,
            )
            update_document_statement(document_id, statement_id)
            statement_status = "UNDER_REVIEW"
            logger.info("Saved as statement_id=%s (UNDER_REVIEW)", statement_id)

        # ═══════════════════════════════════════════════════
        # STEP 4 — DUAL EXTRACTION (CODE + LLM in parallel)
        # Reached for: NEW formats + UNDER_REVIEW formats
        # ═══════════════════════════════════════════════════
        logger.info("")
        logger.info("[STEP 4/5] Running DUAL PIPELINE (CODE + LLM in parallel)...")

        code_txns = []
        llm_txns  = []

        with ThreadPoolExecutor(max_workers=2) as executor:
            future_code = executor.submit(
                extract_transactions_using_logic, full_text, extraction_code
            )
            future_llm = executor.submit(
                parse_with_llm, full_text, identity_json
            )

            # LLM — critical path (user always gets transactions even if CODE fails)
            try:
                llm_response = future_llm.result()
                llm_txns     = extract_json_from_response(llm_response)
                logger.info("LLM extraction complete: %d transactions", len(llm_txns))
            except Exception as e:
                logger.error("LLM extraction FAILED: %s", e)

            # CODE — best-effort (failure falls back to LLM)
            try:
                code_txns = future_code.result()
                logger.info("CODE extraction complete: %d transactions", len(code_txns))
            except Exception as e:
                logger.warning("CODE extraction FAILED (will use LLM): %s", e)

        logger.info("Results: CODE=%d txns | LLM=%d txns", len(code_txns), len(llm_txns))

        # ═══════════════════════════════════════════════════
        # STEP 5 — VALIDATION & DECISION
        # ═══════════════════════════════════════════════════
        logger.info("")
        logger.info("[STEP 5/5] VALIDATION & ACCURACY CHECK...")

        metrics           = validate_transactions(code_txns, llm_txns)
        comparison_score  = metrics.get("overall_accuracy", 0) if metrics else 0

        code_confidence = round(
            sum(t.get("confidence", 0) for t in code_txns) / len(code_txns), 2
        ) if code_txns else 0

        llm_confidence = round(
            sum(t.get("confidence", 0) for t in llm_txns) / len(llm_txns), 2
        ) if llm_txns else 0

        # BUG-02 fix: require BOTH gates to pass for CODE to win
        code_is_proper = validate_extraction_propriety(code_txns)
        code_is_strict = validate_code_quality_strict(code_txns)
        code_passes_quality = code_is_proper and code_is_strict

        logger.info("Code accuracy    : %.2f%%", comparison_score)
        logger.info("Code confidence  : %.2f",   code_confidence)
        logger.info("LLM confidence   : %.2f",   llm_confidence)
        logger.info("Code propriety   : %s",      "PASS" if code_is_proper else "FAIL")
        logger.info("Code strict gate : %s",      "PASS" if code_is_strict else "FAIL")

        # Decision: 90% weighted accuracy + both quality gates → CODE wins
        if comparison_score >= 90 and code_passes_quality:
            final_parser_type    = "CODE"
            new_statement_status = "ACTIVE"
            logger.info("DECISION: CODE WINS (accuracy=%.2f%% ≥ 90%% & both quality gates pass)", comparison_score)
            logger.info("Format status → ACTIVE")
        else:
            final_parser_type    = "LLM"
            new_statement_status = "EXPERIMENTAL"
            if not code_is_proper:
                reason = "code propriety check failed"
            elif not code_is_strict:
                reason = "code strict quality gate failed"
            else:
                reason = f"code accuracy {comparison_score:.2f}% < 90%"
            logger.info("DECISION: LLM WINS (%s)", reason)
            logger.info("Format status → EXPERIMENTAL")

        # Persist DB state
        update_statement_status(statement_id, new_statement_status)
        update_success_rate(statement_id, comparison_score)
        update_processing_complete(document_id, final_parser_type)

        insert_staging_transactions(
            document_id=document_id,
            user_id=user_id,
            code_txns=code_txns,
            llm_txns=llm_txns,
            code_confidence=code_confidence,
            llm_confidence=llm_confidence,
        )

        update_document_status(document_id, "AWAITING_REVIEW")
        insert_audit(document_id, "COMPLETED")

        logger.info("")
        logger.info("PIPELINE COMPLETE for document_id=%s", document_id)
        logger.info("Winner       : %s", final_parser_type)
        logger.info("CODE txns    : %d", len(code_txns))
        logger.info("LLM txns     : %d", len(llm_txns))
        logger.info("Accuracy     : %.2f%%", comparison_score)
        logger.info("New status   : %s", new_statement_status)
        logger.info("═" * 70)

    except Exception as e:
        logger.error("")
        logger.error("PIPELINE FAILED for document_id=%s", document_id)
        logger.error("Error: %s", e, exc_info=True)
        logger.error("═" * 70)
        try:
            update_document_status(document_id, "FAILED")
            insert_audit(document_id, "FAILED", str(e))
        except Exception:
            logger.error("Failed to update failure status for document %s",
                         document_id, exc_info=True)
        raise