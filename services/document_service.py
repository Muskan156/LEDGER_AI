# document_service.py

import os
from typing import Dict, Any, List

from repository.document_repo import (
    create_document,
    update_document_status,
    link_statement_to_document,
    insert_upload_audit,
    save_extracted_text,
    insert_statement_transactions,
    save_document_password
)

from services.review_service import run_review_engine


# ==========================================================
# MAIN DOCUMENT PROCESSOR
# ==========================================================

def process_document(
    user_id: int,
    file_path: str,
    extracted_text: str,
    statement_id: int,
    extracted_transactions: List[Dict],
    password: str = None
) -> Dict[str, Any]:

    file_name = os.path.basename(file_path)

    # ------------------------------------------------------
    # 1️⃣ Create Document Entry
    # ------------------------------------------------------
    document_id = create_document(
        user_id=user_id,
        file_name=file_name,
        file_path=file_path,
        is_password_protected=bool(password)
    )

    insert_upload_audit(document_id, "UPLOADED")

    try:
        # ------------------------------------------------------
        # 2️⃣ Mark Processing
        # ------------------------------------------------------
        update_document_status(document_id, "PROCESSING")
        insert_upload_audit(document_id, "PROCESSING")

        # ------------------------------------------------------
        # 3️⃣ Save Password (if any)
        # ------------------------------------------------------
        if password:
            # You should encrypt before storing
            save_document_password(document_id, password)

        # ------------------------------------------------------
        # 4️⃣ Save Extracted Text
        # ------------------------------------------------------
        save_extracted_text(document_id, extracted_text)

        # ------------------------------------------------------
        # 5️⃣ Link Statement Format
        # ------------------------------------------------------
        link_statement_to_document(document_id, statement_id)

        # ------------------------------------------------------
        # 6️⃣ Store Extracted Transactions (Staging)
        # ------------------------------------------------------
        insert_statement_transactions(
            document_id,
            statement_id,
            extracted_transactions
        )

        # ------------------------------------------------------
        # 7️⃣ Run Review Engine
        # ------------------------------------------------------
        review_result = run_review_engine(
            statement_id,
            file_path,
            extracted_text
        )

        # ------------------------------------------------------
        # 8️⃣ Mark Completed
        # ------------------------------------------------------
        update_document_status(document_id, "COMPLETED")
        insert_upload_audit(document_id, "COMPLETED")

        return {
            "document_id": document_id,
            "review_result": review_result
        }

    except Exception as e:

        update_document_status(document_id, "FAILED")
        insert_upload_audit(document_id, "FAILED", str(e))

        raise e
