# import streamlit as st
# import os
# import tempfile
# import json
# import base64

# # ================= SERVICES =================
# from services.auth_service import login_user, register_user, validate_session
# from services.pdf_service import extract_pages
# from services.identifier_service import (
#     generate_identifier_llm,
#     reduce_text_for_llm,
#     derive_bank_name_from_ifsc
# )
# from services.extraction_service import (
#     generate_extraction_logic_llm,
#     extract_transactions_using_logic
# )
# from services.review_service import run_review_engine

# from repository.statement_category_repo import (
#     get_formats_by_bank_code,
#     insert_statement_category,
#     activate_statement_category
# )

# from services.document_service import (
#     create_document_entry,
#     mark_processing,
#     mark_completed,
#     mark_failed,
#     save_extracted_text
# )
# from services.validation_service import (
#     extract_json_from_response,
# )
# from services.llm_parser import parse_with_llm
# from repository.document_repo import get_document_type_id
# # =========================================================
# # SESSION INIT
# # =========================================================

# if "user_id" not in st.session_state:
#     st.session_state.user_id = None

# if "token" not in st.session_state:
#     st.session_state.token = None

# # =========================================================
# # LOGIN SCREEN
# # =========================================================

# def login_screen():
#     st.title("LedgerAI Login")

#     tab1, tab2 = st.tabs(["Login", "Register"])

#     with tab1:
#         email = st.text_input("Email")
#         password = st.text_input("Password", type="password")

#         if st.button("Login"):
#             result = login_user(email, password)
#             if result:
#                 st.session_state.user_id = result["user_id"]
#                 st.session_state.token = result["token"]
#                 st.success("Login Successful")
#                 st.rerun()
#             else:
#                 st.error("Invalid Credentials")

#     with tab2:
#         reg_email = st.text_input("Register Email")
#         reg_pass = st.text_input("Register Password", type="password")

#         if st.button("Register"):
#             user_id = register_user(reg_email, reg_pass)
#             st.success(f"User created: {user_id}")


# # =========================================================
# # ACCURACY CALCULATION
# # =========================================================

# def calculate_accuracy(logic_txns, llm_txns):
#     if not llm_txns:
#         return 0.0

#     matched = 0

#     for l in logic_txns:
#         for m in llm_txns:
#             if (
#                 l.get("amount") == m.get("amount") and
#                 l.get("date") == m.get("date")
#             ):
#                 matched += 1
#                 break

#     return round((matched / len(llm_txns)) * 100, 2)


# # =========================================================
# # MAIN PROCESSING PIPELINE
# # =========================================================

# def process_document(file, password):

#     user_id = st.session_state.user_id

#     # Save uploaded file temporarily
#     with tempfile.NamedTemporaryFile(delete=False, suffix=".pdf") as tmp:
#         tmp.write(file.read())
#         file_path = tmp.name

#     # ---------------------------------------------
#     # 1️⃣ Create Document Entry
#     # ---------------------------------------------
    
#     doc_type_id = get_document_type_id("BANK_STATEMENT")

#     document_id = create_document_entry(
#     user_id=user_id,
#     document_type_id=doc_type_id,
#     file_name=file.name,
#     file_path=file_path,
#     password=password
#     )

#     mark_processing(document_id)

#     # ---------------------------------------------
#     # 2️⃣ Extract PDF Text
#     # ---------------------------------------------
#     pages = extract_pages(file_path, password)
#     full_text = "\n".join(pages)

#     save_extracted_text(document_id, full_text)

#     # ---------------------------------------------
#     # 3️⃣ Derive Bank via IFSC
#     # ---------------------------------------------
#     bank_name, bank_code = derive_bank_name_from_ifsc(full_text)

#     if not bank_code:
#         st.error("IFSC not detected.")
#         mark_failed(document_id, "IFSC not found")
#         return

#     st.success(f"Bank Detected: {bank_name} ({bank_code})")

#     # ---------------------------------------------
#     # 4️⃣ Check Existing Format
#     # ---------------------------------------------
#     formats = get_formats_by_bank_code(bank_code)

#     # =========================================================
#     # CASE 1️⃣ FORMAT EXISTS
#     # =========================================================
#     if formats:
#         fmt = formats[0]

#         st.info("Existing Format Found")
#         st.write(f"Format: {fmt['format_name']}")
#         st.write(f"Status: {fmt['status']}")
#         # 🔥 Run Review Engine and Capture Result
#         review_result = run_review_engine(
#         fmt["statement_id"],
#         file_path,
#         full_text
#         )

#         if not review_result:
#           st.warning("No transactions extracted for comparison.")
#           mark_completed(document_id)
#           return

#         code_txns = review_result["code_transactions"]
#         lm_txns = review_result["llm_transactions"]
#         metrics = review_result["metrics"]

#         # ===============================
#         # 🔍 SHOW SIDE-BY-SIDE
#         # ===============================
#         st.subheader("Transaction Comparison")

#         col1, col2 = st.columns(2)

#         with col1:
#           st.write("Code Extraction")
#           st.dataframe(code_txns)

#         with col2:
#           st.write("LLM Extraction")
#           st.dataframe(lm_txns)

#         # ===============================
#         # 📊 SHOW METRICS
#         # ===============================
#         st.subheader("Accuracy Report")

#         st.metric("Code Transactions", len(code_txns))
#         st.metric("LLM Transactions", len(lm_txns))
#         st.metric("Overall Accuracy (%)", metrics["overall_accuracy"])

#         threshold = float(fmt["match_threshold"])
#         st.metric("Threshold (%)", threshold)

#         # ===============================
#          # 🎯 PASS / FAIL
#         # ===============================
#         if metrics["overall_accuracy"] >= threshold:
#            st.success("PASS — Meets threshold")

#            if fmt["status"] == "UNDER_REVIEW":
#               if st.button("Activate Format"):
#                 activate_statement_category(fmt["statement_id"])
#                 st.success("Format Activated")

#         else:
#           st.error("FAIL — Below threshold")
#           st.info("Format remains UNDER_REVIEW")

#         mark_completed(document_id)
#         return


#     # =========================================================
#     #  CASE 2️⃣ NEW FORMAT
#     # =========================================================
#     st.warning("No format found. Generating new format...")

#     # ---------------------------------------------
#     # 5️⃣ Generate Identifier
#     # ---------------------------------------------
#     reduced = reduce_text_for_llm(pages)
#     identifier_json = generate_identifier_llm(reduced)

#     identifier_json["bank_identification"] = {
#     "bank_name": {"patterns": [bank_name]}}

#     st.subheader("Generated Identifier")
#     st.json(identifier_json)

#     # ---------------------------------------------
#     # 6️⃣ Generate Extraction Logic
#     # ---------------------------------------------
#     extraction_logic = generate_extraction_logic_llm(identifier_json)

#     # ---------------------------------------------
#     # 7️⃣ Save Format (UNDER_REVIEW by default)
#     # ---------------------------------------------
#     format_name = bank_name.replace(" ", "_").upper() + "_V1"

#     statement_id = insert_statement_category(
#     statement_type="BANK_STATEMENT",
#     format_name=format_name,
#     institution_name=bank_name,
#     ifsc_code=bank_code,
#     identifier_json=identifier_json,
#     extraction_logic_json=extraction_logic,
#     threshold=65.0
#     )

#     st.success(f"New Format Saved (ID: {statement_id})")
#     st.info("Status: UNDER_REVIEW")

#     # ---------------------------------------------
#     # 8️⃣ Run Logic Extraction
#     # ---------------------------------------------
#     logic_txns = extract_transactions_using_logic(
#     full_text,
#     identifier_json,
#     extraction_logic
#     )

#     # ---------------------------------------------
#     # 9️⃣ Run TRUE LLM Extraction (Ground Truth)
#     # ---------------------------------------------
#     try:
#        llm_response = parse_with_llm(full_text)
#        llm_txns = extract_json_from_response(llm_response)
#     except Exception as e:
#        st.error(f"LLM Extraction Failed: {str(e)}")
#        llm_txns = []

#     # ---------------------------------------------
#     # 🔟 Show Comparison
#     # ---------------------------------------------
#     st.subheader("Side by Side Comparison")

#     col1, col2 = st.columns(2)

#     with col1:
#       st.write("Code Extraction")
#       st.dataframe(logic_txns)

#     with col2:
#       st.write("LLM Extraction (Ground Truth)")
#       st.dataframe(llm_txns)

#     # ---------------------------------------------
#     # 1️⃣1️⃣ Calculate Accuracy
#     # ---------------------------------------------
#     if logic_txns and llm_txns:
#         accuracy = calculate_accuracy(logic_txns, llm_txns)
#     else:
#         accuracy = 0.0

#     threshold = 65.0

#     st.subheader("Accuracy Report")

#     st.metric("Code Transactions", len(logic_txns))
#     st.metric("LLM Transactions", len(llm_txns))
#     st.metric("Accuracy (%)", accuracy)
#     st.metric("Threshold (%)", threshold)

#     # ---------------------------------------------
#     # 1️⃣2️⃣ Activation Decision
#     # ---------------------------------------------
#     if accuracy >= threshold:
#         st.success("Meets threshold. You can activate format.")

#         if st.button("Activate Format"):
#             activate_statement_category(statement_id)
#             st.success("Format Activated")
#     else:
#        st.warning("Below threshold. Keep UNDER_REVIEW.")

#        if st.button("Keep Under Review"):
#            st.info("Format remains UNDER_REVIEW")

#     mark_completed(document_id)

# # =========================================================
# # UPLOAD SCREEN
# # =========================================================

# def upload_screen():
#     st.title("Upload Bank Statement")

#     uploaded_file = st.file_uploader("Upload PDF", type=["pdf"])
#     password = st.text_input("PDF Password (optional)", type="password")

#     if uploaded_file and st.button("Process Document"):
#         process_document(uploaded_file, password)


# # =========================================================
# # ROUTER
# # =========================================================

# if not st.session_state.user_id:
#     login_screen()
# else:
#     upload_screen()
import streamlit as st
import tempfile
import json

from services.pdf_service import extract_pages
from services.identifier_service import (
    generate_identifier_llm,
    reduce_text_for_llm,
    derive_bank_name_from_ifsc
)
from services.extraction_service import generate_extraction_logic_llm
from services.review_service import run_review_engine
from repository.statement_category_repo import insert_statement_category
from db.connection import get_connection


# ---------------------------------------------------
# Fetch formats by BANK CODE (same as main.py)
# ---------------------------------------------------
def get_formats_by_bank_code(bank_code: str):
    conn = get_connection()
    cursor = conn.cursor(dictionary=True)

    query = """
        SELECT *
        FROM statement_categories
        WHERE statement_type = 'BANK_STATEMENT'
        AND ifsc_code = %s
    """

    cursor.execute(query, (bank_code,))
    rows = cursor.fetchall()

    cursor.close()
    conn.close()

    for row in rows:
        if isinstance(row["statement_identifier"], str):
            row["statement_identifier"] = json.loads(row["statement_identifier"])

    return rows


# ---------------------------------------------------
# UI
# ---------------------------------------------------
st.set_page_config(layout="wide")
st.title("LedgerAI - Bank Statement Engine")

uploaded_file = st.file_uploader("Upload Bank Statement PDF", type=["pdf"])

if uploaded_file:

    password = st.text_input("Enter PDF Password (if any)", type="password")

    with tempfile.NamedTemporaryFile(delete=False, suffix=".pdf") as tmp:
        tmp.write(uploaded_file.read())
        file_path = tmp.name

    if st.button("Run Processing"):

        # ---------------------------------------------------
        # 1️⃣ Extract PDF
        # ---------------------------------------------------
        st.subheader("Extracting PDF Content")
        pages = extract_pages(file_path, password)
        full_text = "\n".join(pages)
        st.success("PDF extracted successfully")

        # ---------------------------------------------------
        # 2️⃣ Detect Bank via IFSC
        # ---------------------------------------------------
        st.subheader("Detecting Bank via IFSC")
        bank_name, bank_code = derive_bank_name_from_ifsc(full_text)

        if not bank_name:
            bank_name = bank_code or "UNKNOWN_BANK"

        if not bank_code:
            st.error("IFSC not found.")
            st.stop()

        st.write(f"**Bank Name:** {bank_name}")
        st.write(f"**Bank Code:** {bank_code}")

        # ---------------------------------------------------
        # 3️⃣ Check Existing Formats
        # ---------------------------------------------------
        st.subheader("Checking Existing Formats")
        formats = get_formats_by_bank_code(bank_code)

        if formats:

            st.warning("⚠ Format already exists for this bank")

            fmt = formats[0]

            st.write(f"**Format Name:** {fmt['format_name']}")
            st.write(f"**Status:** {fmt['status']}")

            st.info("Running Review Engine on existing format...")

            review_result = run_review_engine(
                fmt["statement_id"],
                file_path,
                full_text
            )

        else:

            st.info("No format found. Generating new format...")

            # ---------------------------------------------------
            # 4️⃣ Generate Identifier
            # ---------------------------------------------------
            st.subheader("Generating Identifier")
            reduced = reduce_text_for_llm(pages)
            identifier_json = generate_identifier_llm(reduced)

            if bank_name:
                identifier_json["bank_identification"] = {
                    "bank_name": {
                        "patterns": [bank_name]
                    }
                }

            st.json(identifier_json)

            # ---------------------------------------------------
            # 5️⃣ Generate Extraction Logic
            # ---------------------------------------------------
            st.subheader("Generating Extraction Logic")

            headers = [
                line.strip()
                for line in full_text.splitlines()
                if any(k in line.lower() for k in ["date", "debit", "credit", "balance"])
            ][:5]

            footer = [
                line.strip()
                for line in full_text.splitlines()
                if any(k in line.lower() for k in ["summary", "end of statement"])
            ][-5:]

            sample_text = full_text[:5000]

            extraction_code = generate_extraction_logic_llm(
                identifier_json,
                headers,
                sample_text,
                footer
            )

            with st.expander("View Generated Extraction Code"):
                st.code(extraction_code, language="python")

            # ---------------------------------------------------
            # 6️ Save Format
            # ---------------------------------------------------
            formatted_bank = bank_name.replace(" ", "_").upper()
            format_name = f"{formatted_bank}_V1"

            statement_id = insert_statement_category(
                statement_type="BANK_STATEMENT",
                format_name=format_name,
                institution_name=bank_name,
                ifsc_code=bank_code,
                identifier_json=identifier_json,
                extraction_logic_json=extraction_code,
                threshold=65.0
            )

            st.success("Format saved successfully")
            st.write(f"Statement ID: {statement_id}")
            st.write("Status: UNDER_REVIEW")

            st.info("Running Review Engine...")

            review_result = run_review_engine(
                statement_id,
                file_path,
                full_text
            )

        # ---------------------------------------------------
        # 7️⃣ Show Review Results
        # ---------------------------------------------------
        if not review_result:
            st.error("Review engine did not return data")
            st.stop()

        code_txns = review_result.get("code_transactions", [])
        llm_txns = review_result.get("llm_transactions", [])
        metrics = review_result.get("metrics", {})

        st.subheader("Validation Metrics")
        st.json(metrics)

        # ---------------------------------------------------
        # 8️⃣ Side-by-Side Comparison
        # ---------------------------------------------------
        st.subheader("Side-by-Side Transaction Comparison")

        min_len = min(len(code_txns), len(llm_txns))

        for i in range(min_len):

            col1, col2 = st.columns(2)

            with col1:
                st.markdown(f"### Code Transaction {i+1}")
                st.json(code_txns[i])

            with col2:
                st.markdown(f"### LLM Transaction {i+1}")
                st.json(llm_txns[i])

            st.divider()

        # ---------------------------------------------------
        # 🔢 Overall Similarity + Decision Engine
        # ---------------------------------------------------

        overall_similarity = metrics.get("overall_accuracy", 0)

        st.subheader("Overall Similarity")
        st.success(f"Similarity Score: {overall_similarity}%")

        # ==================================================
        # DECISION ENGINE (Aligned with DB Architecture)
        # ==================================================

        # CASE 1: ≥ 90% → Auto Activate
        if overall_similarity >= 90:
            st.success("ACCEPTED")
            st.success("Parser Verified & ACTIVATED")

            from repository.statement_category_repo import activate_statement_category
            activate_statement_category(statement_id)

       # CASE 2: 75–90% → Human Loop
        elif 75 <= overall_similarity < 90:

          st.warning("PARTIAL MATCH (75–90%)")
          st.info("Human Intervention Required")

          manual_code = st.text_area(
             "Edit / Replace Extraction Logic Below:",
             height=400,
             value=extraction_code,
           )

          if st.button("Validate Human Logic"):

               if manual_code.strip() == "":
                   st.error("Code cannot be empty.")
               elif "def extract_transactions" not in manual_code:
                   st.error("Code must contain: def extract_transactions(text: str)")
               else:
                try:
                    from services.extraction_service import extract_transactions_using_logic
                    st.info("Running Human Extraction Logic...")
                    human_transactions = extract_transactions_using_logic(
                    full_text,
                    manual_code
                    )

                    # Re-run validation using your production validator
                    from services.validation_service import validate_transactions

                    new_metrics = validate_transactions(
                    human_transactions,
                    llm_txns
                   )

                    new_score = new_metrics.get("overall_accuracy", 0)

                    st.success(f"Human Logic Similarity: {new_score}%")

                    if new_score >= 90:
                        from repository.statement_category_repo import update_extraction_logic
                        update_extraction_logic(statement_id, manual_code)

                        from repository.statement_category_repo import activate_statement_category
                        activate_statement_category(statement_id)

                        st.success("HUMAN LOGIC ACCEPTED")
                        st.success("Format Activated")

                    else:
                        st.error("HUMAN LOGIC REJECTED (Below 90%)")

                except Exception as e:
                   st.error(f"Execution Error: {e}")

        # CASE 3: < 75% → Reject
        else:
            st.error("REJECTED (Below 75%)")
