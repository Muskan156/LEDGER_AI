"""
services/document_service.py
────────────────────────────
Thin service layer used by the Streamlit app for document-related UI operations.
Processing pipeline is in processing_engine.py.
"""

import os
import logging
from db.connection import get_cursor

logger = logging.getLogger("ledgerai.document_service")


def create_document(
    user_id: int,
    file_name: str,
    file_path: str,
    is_password_protected: bool,
    password: str = None,
) -> int:
    """Insert document + password + audit in a single transaction."""
    with get_cursor(commit=True) as (conn, cursor):
        cursor.execute("""
            INSERT INTO documents
            (user_id, file_name, file_path, is_password_protected, status)
            VALUES (%s, %s, %s, %s, 'UPLOADED')
        """, (user_id, file_name, file_path, is_password_protected))

        document_id = cursor.lastrowid

        if password:
            cursor.execute("""
                INSERT INTO document_password (document_id, encrypted_password)
                VALUES (%s, %s)
            """, (document_id, password))

        cursor.execute("""
            INSERT INTO document_upload_audit (document_id, status)
            VALUES (%s, 'UPLOADED')
        """, (document_id,))

    logger.info("Created document_id=%s file=%s user_id=%s",
                document_id, file_name, user_id)
    return document_id


def get_user_documents(user_id: int) -> list:
    """Get all documents for a user, most recent first."""
    with get_cursor(dictionary=True) as (conn, cursor):
        cursor.execute("""
            SELECT document_id, file_name, status, created_at,
                   transaction_parsed_type, processing_started_at,
                   processing_completed_at
            FROM documents
            WHERE user_id=%s AND is_active=TRUE
            ORDER BY created_at DESC
        """, (user_id,))
        return cursor.fetchall()
