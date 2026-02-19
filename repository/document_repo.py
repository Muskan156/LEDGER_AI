import json
from db.connection import get_connection


# ---------------------------------------------------
# INSERT DOCUMENT
# ---------------------------------------------------
def insert_document(user_id: int,
                    document_type_id: int,
                    file_name: str,
                    file_path: str,
                    is_password_protected: bool):

    conn = get_connection()
    cursor = conn.cursor()

    query = """
        INSERT INTO documents
        (user_id, document_type_id, file_name, file_path, is_password_protected, status)
        VALUES (%s, %s, %s, %s, %s, 'UPLOADED')
    """

    cursor.execute(query, (
        user_id,
        document_type_id,
        file_name,
        file_path,
        is_password_protected
    ))

    document_id = cursor.lastrowid
    conn.commit()

    cursor.close()
    conn.close()

    return document_id

def get_document_type_id(type_code: str):
    conn = get_connection()
    cursor = conn.cursor(dictionary=True)

    cursor.execute(
        "SELECT document_type_id FROM document_types WHERE type_code = %s",
        (type_code,)
    )

    row = cursor.fetchone()

    cursor.close()
    conn.close()

    if not row:
        raise ValueError(f"Document type {type_code} not found")

    return row["document_type_id"]

# ---------------------------------------------------
# INSERT PASSWORD
# ---------------------------------------------------
def insert_document_password(document_id: int, encrypted_password: str):

    conn = get_connection()
    cursor = conn.cursor()

    query = """
        INSERT INTO document_password (document_id, encrypted_password)
        VALUES (%s, %s)
    """

    cursor.execute(query, (document_id, encrypted_password))
    conn.commit()

    cursor.close()
    conn.close()


# ---------------------------------------------------
# UPDATE DOCUMENT STATUS
# ---------------------------------------------------
def update_document_status(document_id: int, status: str):

    conn = get_connection()
    cursor = conn.cursor()

    query = """
        UPDATE documents
        SET status = %s
        WHERE document_id = %s
    """

    cursor.execute(query, (status, document_id))
    conn.commit()

    cursor.close()
    conn.close()


# ---------------------------------------------------
# INSERT AUDIT
# ---------------------------------------------------
def insert_upload_audit(document_id: int, status: str, error_message=None):

    conn = get_connection()
    cursor = conn.cursor()

    query = """
        INSERT INTO document_upload_audit
        (document_id, status, error_message)
        VALUES (%s, %s, %s)
    """

    cursor.execute(query, (document_id, status, error_message))
    conn.commit()

    cursor.close()
    conn.close()


# ---------------------------------------------------
# INSERT TEXT EXTRACTION
# ---------------------------------------------------
def insert_text_extraction(document_id: int,
                           extracted_text: str,
                           extraction_status="SUCCESS",
                           error_message=None):

    conn = get_connection()
    cursor = conn.cursor()

    query = """
        INSERT INTO document_text_extractions
        (document_id, extraction_method, extracted_text, extraction_status, error_message)
        VALUES (%s, 'PDF_TEXT', %s, %s, %s)
    """

    cursor.execute(query, (
        document_id,
        extracted_text,
        extraction_status,
        error_message
    ))

    conn.commit()
    cursor.close()
    conn.close()
