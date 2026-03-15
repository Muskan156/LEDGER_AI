"""
repository/document_repo.py
────────────────────────────
All database operations for the documents table.
Status enums from schema:
  UPLOADED | PASSWORD_REQUIRED | EXTRACTING_TEXT | IDENTIFYING_FORMAT
  PARSING_TRANSACTIONS | AWAITING_REVIEW | CATEGORIZING | POSTED | APPROVE | FAILED
"""

import json
import logging
from db.connection import get_cursor

logger = logging.getLogger("ledgerai.document_repo")


# ── READ ─────────────────────────────────────────────────

def get_document(document_id: int):
    with get_cursor(dictionary=True) as (conn, cursor):
        cursor.execute("SELECT * FROM documents WHERE document_id=%s", (document_id,))
        return cursor.fetchone()


def get_document_password(document_id: int):
    with get_cursor(dictionary=True) as (conn, cursor):
        cursor.execute(
            "SELECT encrypted_password FROM document_password WHERE document_id=%s",
            (document_id,),
        )
        row = cursor.fetchone()
        return row["encrypted_password"] if row else None


# ── STATUS UPDATES ───────────────────────────────────────

def update_document_status(document_id: int, status: str):
    with get_cursor(commit=True) as (conn, cursor):
        cursor.execute(
            "UPDATE documents SET status=%s WHERE document_id=%s",
            (status, document_id),
        )


def update_processing_start(document_id: int):
    with get_cursor(commit=True) as (conn, cursor):
        cursor.execute("""
            UPDATE documents
            SET status='EXTRACTING_TEXT', processing_started_at=NOW()
            WHERE document_id=%s
        """, (document_id,))


def update_processing_complete(document_id: int, parser_type: str):
    with get_cursor(commit=True) as (conn, cursor):
        cursor.execute("""
            UPDATE documents
            SET transaction_parsed_type=%s, processing_completed_at=NOW()
            WHERE document_id=%s
        """, (parser_type, document_id))


# ── LINKING ──────────────────────────────────────────────

def update_document_statement(document_id: int, statement_id: int):
    with get_cursor(commit=True) as (conn, cursor):
        cursor.execute(
            "UPDATE documents SET statement_id=%s WHERE document_id=%s",
            (statement_id, document_id),
        )


# ── AUDIT ────────────────────────────────────────────────

def insert_audit(document_id: int, status: str, error_message: str = None):
    with get_cursor(commit=True) as (conn, cursor):
        # Truncate to fit VARCHAR(500) column
        if error_message and len(error_message) > 490:
            error_message = error_message[:490] + "..."
        cursor.execute("""
            INSERT INTO document_upload_audit (document_id, status, error_message)
            VALUES (%s, %s, %s)
        """, (document_id, status, error_message))


# ── TEXT EXTRACTION ──────────────────────────────────────

def insert_text_extraction(document_id: int, extracted_text: str):
    with get_cursor(commit=True, prepared=True) as (conn, cursor):
        cursor.execute("""
            INSERT INTO document_text_extractions
            (document_id, extraction_method, extracted_text, extraction_status)
            VALUES (%s, 'PDF_TEXT', %s, 'SUCCESS')
        """, (document_id, extracted_text))


# ── STAGING TRANSACTIONS ────────────────────────────────
# Schema: ai_transactions_staging
#   staging_transaction_id | document_id | user_id
#   transaction_json (JSON) | parser_type ENUM('LLM','CODE')
#   overall_confidence DECIMAL(5,2) | created_at

def insert_staging_transactions(
    document_id: int,
    user_id: int,
    code_txns: list,
    llm_txns: list,
    code_confidence: float,
    llm_confidence: float,
):
    with get_cursor(commit=True, prepared=True) as (conn, cursor):
        # Insert CODE transactions
        cursor.execute("""
            INSERT INTO ai_transactions_staging
            (document_id, user_id, transaction_json, parser_type, overall_confidence)
            VALUES (%s, %s, %s, 'CODE', %s)
        """, (document_id, user_id, json.dumps(code_txns), code_confidence))

        # Insert LLM transactions
        cursor.execute("""
            INSERT INTO ai_transactions_staging
            (document_id, user_id, transaction_json, parser_type, overall_confidence)
            VALUES (%s, %s, %s, 'LLM', %s)
        """, (document_id, user_id, json.dumps(llm_txns), llm_confidence))


def insert_staging_code_only(
    document_id: int,
    user_id: int,
    code_txns: list,
    confidence: float,
):
    """For ACTIVE formats — only CODE transactions, no LLM."""
    with get_cursor(commit=True, prepared=True) as (conn, cursor):
        cursor.execute("""
            INSERT INTO ai_transactions_staging
            (document_id, user_id, transaction_json, parser_type, overall_confidence)
            VALUES (%s, %s, %s, 'CODE', %s)
        """, (document_id, user_id, json.dumps(code_txns), confidence))


# ── REVIEW PAGE ─────────────────────────────────────────

def get_review_transactions(document_id: int):
    """Get the winning parser's staging row for review screen."""
    with get_cursor(dictionary=True) as (conn, cursor):
        cursor.execute("""
            SELECT s.*, d.statement_id
            FROM ai_transactions_staging s
            JOIN documents d ON s.document_id = d.document_id
            WHERE s.document_id=%s
              AND s.parser_type = d.transaction_parsed_type
            LIMIT 1
        """, (document_id,))
        return cursor.fetchone()


# ── FINAL APPROVAL ──────────────────────────────────────

def insert_uncategorized_transactions(
    document_id: int,
    user_id: int,
    statement_id: int,
    staging_transaction_id: int,
    transactions: list,
):
    with get_cursor(commit=True) as (conn, cursor):
        for txn in transactions:
            cursor.execute("""
                INSERT INTO uncategorized_transactions
                (user_id, document_id, statement_id, staging_transaction_id,
                 txn_date, debit, credit, balance, description, confidence)
                VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
            """, (
                user_id, document_id, statement_id, staging_transaction_id,
                txn.get("date"), txn.get("debit"), txn.get("credit"),
                txn.get("balance"), txn.get("details"), txn.get("confidence"),
            ))

        cursor.execute(
            "UPDATE documents SET status='APPROVE' WHERE document_id=%s",
            (document_id,),
        )