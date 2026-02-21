# document_repo.py

from db import get_connection
from typing import List, Dict, Optional


# ==========================================================
# DOCUMENT TABLE OPERATIONS
# ==========================================================

def create_document(
    user_id: int,
    file_name: str,
    file_path: str,
    is_password_protected: bool
) -> int:

    conn = get_connection()
    cursor = conn.cursor()

    cursor.execute("""
        INSERT INTO documents
        (user_id, file_name, file_path, is_password_protected, status)
        VALUES (%s, %s, %s, %s, 'UPLOADED')
    """, (user_id, file_name, file_path, is_password_protected))

    document_id = cursor.lastrowid
    conn.commit()
    cursor.close()
    conn.close()

    return document_id


def update_document_status(document_id: int, status: str):
    conn = get_connection()
    cursor = conn.cursor()

    cursor.execute("""
        UPDATE documents
        SET status = %s
        WHERE document_id = %s
    """, (status, document_id))

    conn.commit()
    cursor.close()
    conn.close()


def link_statement_to_document(document_id: int, statement_id: int):
    conn = get_connection()
    cursor = conn.cursor()

    cursor.execute("""
        UPDATE documents
        SET statement_id = %s
        WHERE document_id = %s
    """, (statement_id, document_id))

    conn.commit()
    cursor.close()
    conn.close()


# ==========================================================
# PASSWORD STORAGE
# ==========================================================

def save_document_password(document_id: int, encrypted_password: str):
    conn = get_connection()
    cursor = conn.cursor()

    cursor.execute("""
        INSERT INTO document_password (document_id, encrypted_password)
        VALUES (%s, %s)
    """, (document_id, encrypted_password))

    conn.commit()
    cursor.close()
    conn.close()


# ==========================================================
# AUDIT LOG
# ==========================================================

def insert_upload_audit(document_id: int, status: str, error_message: Optional[str] = None):
    conn = get_connection()
    cursor = conn.cursor()

    cursor.execute("""
        INSERT INTO document_upload_audit (document_id, status, error_message)
        VALUES (%s, %s, %s)
    """, (document_id, status, error_message))

    conn.commit()
    cursor.close()
    conn.close()


# ==========================================================
# TEXT EXTRACTION STORAGE
# ==========================================================

def save_extracted_text(
    document_id: int,
    extracted_text: str,
    method: str = "PDF_TEXT",
    status: str = "SUCCESS",
    error_message: Optional[str] = None
):
    conn = get_connection()
    cursor = conn.cursor()

    cursor.execute("""
        INSERT INTO document_text_extractions
        (document_id, extraction_method, extracted_text, extraction_status, error_message)
        VALUES (%s, %s, %s, %s, %s)
    """, (document_id, method, extracted_text, status, error_message))

    conn.commit()
    cursor.close()
    conn.close()


# ==========================================================
# STAGING TRANSACTIONS STORAGE
# ==========================================================

def insert_statement_transactions(
    document_id: int,
    statement_id: int,
    transactions: List[Dict]
):

    conn = get_connection()
    cursor = conn.cursor()

    for txn in transactions:
        cursor.execute("""
            INSERT INTO statement_transactions
            (document_id, statement_id, txn_date, debit, credit, balance,
             description, confidence)
            VALUES (%s, %s, %s, %s, %s, %s, %s, %s)
        """, (
            document_id,
            statement_id,
            txn.get("date"),
            txn.get("debit"),
            txn.get("credit"),
            txn.get("balance"),
            txn.get("details"),
            txn.get("confidence")
        ))

    conn.commit()
    cursor.close()
    conn.close()