"""
services/review_service.py
──────────────────────────
Handles the human review screen data layer.
The review engine logic is in processing_engine.py now.
"""

import json
import logging
from db.connection import get_cursor
from repository.document_repo import (
    get_review_transactions,
    insert_uncategorized_transactions,
    update_document_status,
    insert_audit,
)

logger = logging.getLogger("ledgerai.review_service")


def get_document_for_review(document_id: int) -> dict:
    """
    Fetch all data needed for the review screen.
    Returns dict with document info + transactions.
    """
    with get_cursor(dictionary=True) as (conn, cursor):
        # Get document
        cursor.execute("SELECT * FROM documents WHERE document_id=%s", (document_id,))
        doc = cursor.fetchone()
        if not doc:
            return None

        # Get both CODE and LLM staging rows
        cursor.execute("""
            SELECT staging_transaction_id, parser_type,
                   transaction_json, overall_confidence
            FROM ai_transactions_staging
            WHERE document_id=%s
            ORDER BY parser_type
        """, (document_id,))
        staging_rows = cursor.fetchall()

    # Parse JSON
    code_txns = []
    llm_txns = []
    code_staging_id = None
    llm_staging_id = None

    for row in staging_rows:
        txn_data = row["transaction_json"]
        if isinstance(txn_data, str):
            txn_data = json.loads(txn_data)

        if row["parser_type"] == "CODE":
            code_txns = txn_data
            code_staging_id = row["staging_transaction_id"]
        elif row["parser_type"] == "LLM":
            llm_txns = txn_data
            llm_staging_id = row["staging_transaction_id"]

    return {
        "document": doc,
        "code_transactions": code_txns,
        "llm_transactions": llm_txns,
        "code_staging_id": code_staging_id,
        "llm_staging_id": llm_staging_id,
        "final_parser": doc.get("transaction_parsed_type"),
    }


def approve_transactions(
    document_id: int,
    user_id: int,
    statement_id: int,
    staging_transaction_id: int,
    transactions: list,
):
    """Move approved transactions to uncategorized_transactions."""
    insert_uncategorized_transactions(
        document_id=document_id,
        user_id=user_id,
        statement_id=statement_id,
        staging_transaction_id=staging_transaction_id,
        transactions=transactions,
    )
    insert_audit(document_id, "APPROVE")
    logger.info("Approved %d transactions for document_id=%s.",
                len(transactions), document_id)