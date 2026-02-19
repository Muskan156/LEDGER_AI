import base64
from repository.document_repo import (
    insert_document,
    insert_document_password,
    update_document_status,
    insert_upload_audit,
    insert_text_extraction
)
from db.connection import get_connection

# ---------------------------------------------------
# HANDLE COMPLETE DOCUMENT LIFECYCLE
# ---------------------------------------------------
def create_document_entry(user_id: int,
                          document_type_id: int,
                          file_name: str,
                          file_path: str,
                          password: str | None):

    is_protected = bool(password)

    # 1️⃣ Insert Document
    document_id = insert_document(
        user_id=user_id,
        document_type_id=document_type_id,
        file_name=file_name,
        file_path=file_path,
        is_password_protected=is_protected
    )

    # 2️⃣ Insert Password (Base64 demo encryption)
    if password:
        encrypted = base64.b64encode(password.encode()).decode()
        insert_document_password(document_id, encrypted)

    # 3️⃣ Insert Audit Entry
    insert_upload_audit(document_id, "UPLOADED")

    return document_id



# -------------------------------------------
# Store Extracted Text
# -------------------------------------------
def store_extracted_text(document_id: int, extracted_text: str):

    conn = get_connection()
    cursor = conn.cursor()

    query = """
        INSERT INTO document_text_extractions (
            document_id,
            extraction_method,
            extracted_text,
            extraction_status
        )
        VALUES (%s, 'PDF_TEXT', %s, 'SUCCESS')
    """

    cursor.execute(query, (document_id, extracted_text))
    conn.commit()
    cursor.close()
    conn.close()


# -------------------------------------------
# Store Password
# -------------------------------------------
def store_document_password(document_id: int, password: str):

    conn = get_connection()
    cursor = conn.cursor()

    query = """
        INSERT INTO document_password (
            document_id,
            encrypted_password
        )
        VALUES (%s, %s)
    """

    # ⚠ For demo only. Later encrypt properly.
    cursor.execute(query, (document_id, password))

    conn.commit()
    cursor.close()
    conn.close()


def mark_processing(document_id: int):
    update_document_status(document_id, "PROCESSING")
    insert_upload_audit(document_id, "PROCESSING")


def mark_completed(document_id: int):
    update_document_status(document_id, "COMPLETED")
    insert_upload_audit(document_id, "COMPLETED")


def mark_failed(document_id: int, error_message: str):
    update_document_status(document_id, "FAILED")
    insert_upload_audit(document_id, "FAILED", error_message)


def save_extracted_text(document_id: int, text: str):
    insert_text_extraction(document_id, text)
