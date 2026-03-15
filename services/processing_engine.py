"""
services/processing_engine.py
─────────────────────────────
CENTRAL ORCHESTRATOR — implements the complete document processing
pipeline as defined in the specification.

Flow:
  STEP 1 — Document upload (handled by app.py / caller)
  STEP 2 — Format check in DB
  STEP 3 — Generate identification markers (if new)
  STEP 4 — Dual extraction (CODE + LLM)
  STEP 5 — Validation engine
  DECISION — Route to correct outcome

Key invariants:
  • User ALWAYS receives transactions.
  • User NEVER sees inconsistent/partial data.
  • ACTIVE formats skip LLM completely.
  • Only validated code becomes ACTIVE.
"""

import json
import logging
from concurrent.futures import ThreadPoolExecutor

from services.pdf_service import extract_pages
from services.identifier_service import (
    reduce_text,
    find_existing_identifier,
    classify_document_llm,
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
    validate_extraction_propriety
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


def process_document(document_id: int):
    """
    Main entry point. Called after document is inserted into DB.
    Runs the complete pipeline: extract → identify → parse → validate → stage.
    """

    try:
        # ═══════════════════════════════════════════════════
        # STEP 1 — FETCH DOCUMENT
        # ═══════════════════════════════════════════════════
        logger.info("")
        logger.info("═" * 70)
        logger.info("  PIPELINE START — document_id=%s", document_id)
        logger.info("═" * 70)

        doc = get_document(document_id)
        if not doc:
            raise ValueError(f"Document {document_id} not found.")

        file_path = doc["file_path"]
        user_id = doc["user_id"]
        password = get_document_password(document_id)
        logger.info("═" * 70)
        logger.info("[STEP 1/5] Document fetched")
        logger.info("file      : %s", doc["file_name"])
        logger.info("path      : %s", file_path)
        logger.info("user_id   : %s", user_id)
        logger.info("password  : %s", "YES" if password else "NO")
        logger.info("═" * 70)
        # Mark processing started
        update_processing_start(document_id)
        insert_audit(document_id, "PROCESSING")

        # ═══════════════════════════════════════════════════
        # TEXT EXTRACTION
        # ═══════════════════════════════════════════════════
        logger.info("")
        logger.info("[STEP 2/5] Extracting text from PDF...")
        pages = extract_pages(file_path, password)
        if not pages:
            raise ValueError("PDF extraction returned no pages.")

        full_text = "\n".join(pages)
        reduced = reduce_text(pages)

        logger.info("pages     : %d", len(pages))
        logger.info("chars     : %d", len(full_text))
        logger.info("Text extracted successfully")
        logger.info("full_text : %s", full_text)
        logger.info("═" * 70)
        update_document_status(document_id, "IDENTIFYING_FORMAT")
        insert_text_extraction(document_id, full_text)

        # ═══════════════════════════════════════════════════
        # STEP 2 — FORMAT CHECK IN DATABASE
        # ═══════════════════════════════════════════════════
        logger.info("")
        logger.info("[STEP 3/5] Checking if format exists in database...")
        matched, existing = find_existing_identifier(full_text)

        if matched:
            logger.info("EXISTING FORMAT DETECTED")
            logger.info("format    : %s", existing.get("format_name", "?"))
            logger.info("statement : %s", existing.get("statement_id"))
            logger.info("status    : %s", existing.get("status"))
        else:
            logger.info("NO MATCHING FORMAT — new format detected")
        logger.info("═" * 70)
        update_document_status(document_id, "PARSING_TRANSACTIONS")

        # ═══════════════════════════════════════════════════
        # CASE A — FORMAT EXISTS
        # ═══════════════════════════════════════════════════
        if matched:
            identity_json = existing.get("statement_identifier", {})
            extraction_code = existing["extraction_logic"]
            statement_id = existing["statement_id"]
            statement_status = existing.get("status")

            update_document_statement(document_id, statement_id)

            # ─────────────────────────────────────────────
            # CASE A1 — STATUS = ACTIVE → Fast path (CODE ONLY)
            # ─────────────────────────────────────────────
            if statement_status == "ACTIVE":
                logger.info("")
                logger.info("[STEP 4/5] ACTIVE format — running stored extraction code (fast path)...")
                logger.info("Skipping LLM (format is trusted)")
                code_txns = extract_transactions_using_logic(full_text, extraction_code)
                logger.info("Code extracted %d transactions", len(code_txns))

                if validate_extraction_propriety(code_txns):
                    logger.info("Code transactions are valid")
                    logger.info("")
                    logger.info("[STEP 5/5] PIPELINE COMPLETE")
                    logger.info("Winner    : CODE (ACTIVE fast-path)")
                    logger.info("Txns      : %d", len(code_txns))
                    logger.info("Status    : AWAITING_REVIEW")
                    update_processing_complete(document_id, "CODE")
                    insert_staging_code_only(document_id, user_id, code_txns, 100.0)
                    update_document_status(document_id, "AWAITING_REVIEW")
                    insert_audit(document_id, "COMPLETED")
                    logger.info("═" * 70)
                    return  # ← EXIT
                else:
                    logger.warning("ACTIVE code produced improper transactions!")
                    logger.warning("Downgrading statement_id=%s to UNDER_REVIEW", statement_id)
                    update_statement_status(statement_id, "UNDER_REVIEW")
                    # No return here, fallthrough to dual pipeline

            # ─────────────────────────────────────────────
            # CASE A2 — STATUS = EXPERIMENTAL → LLM ONLY (as requested)
            # ─────────────────────────────────────────────
            elif statement_status == "EXPERIMENTAL":
                logger.info("")
                logger.info("[STEP 4/5] EXPERIMENTAL format — using LLM extraction only...")
                logger.info("Generating transactions using LLM...")
                llm_response = parse_with_llm(full_text, identity_json)
                llm_txns = extract_json_from_response(llm_response)
                logger.info("LLM extracted %d transactions", len(llm_txns))

                logger.info("")
                logger.info("[STEP 5/5] PIPELINE COMPLETE")
                logger.info("Winner    : LLM (EXPERIMENTAL path)")
                logger.info("Txns      : %d", len(llm_txns))
                logger.info("Status    : AWAITING_REVIEW")
                logger.info("═" * 70)
                update_processing_complete(document_id, "LLM")
                # Store in staging (code set is empty)
                insert_staging_transactions(
                    document_id=document_id, user_id=user_id,
                    code_txns=[], llm_txns=llm_txns,
                    code_confidence=0.0, llm_confidence=0.85
                )
                update_document_status(document_id, "AWAITING_REVIEW")
                insert_audit(document_id, "COMPLETED")
                logger.info("═" * 70)
                return  # ← EXIT

            # ─────────────────────────────────────────────
            # CASE A3 — STATUS = UNDER_REVIEW → Dual Pipeline
            # ─────────────────────────────────────────────
            logger.info("")
            logger.info("UNDER_REVIEW format — continuing to dual pipeline...")

        # ═══════════════════════════════════════════════════
        # CASE B — NEW FORMAT → CLASSIFY & GENERATE
        # ═══════════════════════════════════════════════════
        else:
            logger.info("")
            logger.info("[STEP 3b/5] NEW FORMAT — generating identification markers via LLM...")

            # STEP 3 — Generate identification markers
            identity_json = classify_document_llm(reduced)
            logger.info("Family    : %s", identity_json.get("document_family", "?"))
            logger.info("Institution: %s", identity_json.get("institution_name", "?"))
            logger.info("Identifiers generated")
            logger.info("═" * 70)
            # STEP 4 — Generate extraction code
            logger.info("")
            logger.info("[STEP 3c/5] Generating extraction code via LLM...")
            extraction_code = generate_extraction_logic_llm(
                identifier_json=identity_json,
                text_sample=full_text,
            )
            logger.info("Extraction code generated (%d chars)", len(extraction_code))

            # Save new format to DB
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
        # Used for NEW formats or formats UNDER_REVIEW
        # ═══════════════════════════════════════════════════
        logger.info("")
        logger.info("[STEP 4/5] Running DUAL PIPELINE (CODE + LLM in parallel)...")

        code_txns = []
        llm_txns = []

        with ThreadPoolExecutor(max_workers=2) as executor:
            logger.info("Submitting CODE extraction task...")
            future_code = executor.submit(
                extract_transactions_using_logic, full_text, extraction_code
            )
            logger.info("Submitting LLM extraction task...")
            future_llm = executor.submit(
                parse_with_llm, full_text, identity_json
            )

            # LLM result — critical path (user always gets transactions)
            try:
                llm_response = future_llm.result()
                llm_txns = extract_json_from_response(llm_response)
                logger.info("LLM extraction complete: %d transactions", len(llm_txns))
            except Exception as e:
                logger.error("LLM extraction FAILED: %s", e)

            # CODE result — best-effort (failure falls back to LLM)
            try:
                code_txns = future_code.result()
                logger.info("CODE extraction complete: %d transactions", len(code_txns))
            except Exception as e:
                logger.warning("CODE extraction FAILED (will use LLM): %s", e)

        logger.info("Results: CODE=%d txns | LLM=%d txns",
                     len(code_txns), len(llm_txns))

        # ═══════════════════════════════════════════════════
        # STEP 5 — VALIDATION & DECISION (Dual Only)
        # ═══════════════════════════════════════════════════
        logger.info("")
        logger.info("[STEP 5/5] VALIDATION & ACCURACY CHECK...")

        metrics = validate_transactions(code_txns, llm_txns)
        comparison_score = metrics.get("overall_accuracy", 0) if metrics else 0

        code_confidence = round(
            sum(t.get("confidence", 0) for t in code_txns) / len(code_txns), 2
        ) if code_txns else 0

        llm_confidence = round(
            sum(t.get("confidence", 0) for t in llm_txns) / len(llm_txns), 2
        ) if llm_txns else 0

        code_is_proper = validate_extraction_propriety(code_txns)

        logger.info("Code accuracy    : %.2f%%", comparison_score)
        logger.info("Code confidence  : %.2f", code_confidence)
        logger.info("LLM confidence   : %.2f", llm_confidence)
        logger.info("Code is proper   : %s", "YES" if code_is_proper else "NO")

        if comparison_score >= 90 and code_is_proper:
            final_parser_type = "CODE"
            new_statement_status = "ACTIVE"
            logger.info("DECISION: CODE WINS (accuracy ≥ 90%% & proper)")
            logger.info("Format status → ACTIVE")
        else:
            final_parser_type = "LLM"
            new_statement_status = "EXPERIMENTAL"
            if not code_is_proper:
                logger.info("DECISION: LLM WINS (code produced improper results)")
            else:
                logger.info("DECISION: LLM WINS (code accuracy %.2f%% < 90%%)", comparison_score)
            logger.info("Format status → EXPERIMENTAL")

        # Update DB state
        update_statement_status(statement_id, new_statement_status)
        update_success_rate(statement_id, comparison_score)
        update_processing_complete(document_id, final_parser_type)

        # Store transactions
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
        logger.info("Status       : AWAITING_REVIEW")
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