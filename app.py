"""
app.py
──────
LedgerAI — Streamlit Application

Screens:
  1. LOGIN       — Email + password authentication
  2. REGISTER    — New user registration
  3. UPLOAD      — Upload financial document PDF
  4. PROCESSING  — Live status polling during background processing
  5. REVIEW      — View extracted transactions + approve
  6. DASHBOARD   — List of user's documents
"""

# ── Logging must be configured BEFORE any other imports ──
import logging
import sys

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s  %(name)-30s  %(levelname)-7s  %(message)s",
    handlers=[logging.StreamHandler(sys.stdout)],
)

import streamlit as st
import tempfile
import json
import time
import pandas as pd
import hashlib
import uuid
import datetime

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
from services.validation_service import validate_transactions, extract_json_from_response
from services.review_service import get_document_for_review, approve_transactions
from services.document_service import create_document, get_user_documents
from services.task_queue import submit_document, get_task_status, TaskStatus
from repository.document_repo import get_document
from repository.statement_category_repo import (
    activate_statement_category,
    update_extraction_logic,
    update_success_rate,
)
from db.connection import get_cursor

logger = logging.getLogger("ledgerai.app")

# ═══════════════════════════════════════════════════════════
# SESSION STATE DEFAULTS
# ═══════════════════════════════════════════════════════════

if "user_id" not in st.session_state:
    st.session_state.user_id = None
if "screen" not in st.session_state:
    st.session_state.screen = "login"
if "current_document" not in st.session_state:
    st.session_state.current_document = None


# ═══════════════════════════════════════════════════════════
# AUTH HELPERS
# ═══════════════════════════════════════════════════════════

def hash_password(password: str):
    return hashlib.sha256(password.encode()).hexdigest()

def create_session(user_id):
    with get_cursor(commit=True) as (conn, cursor):
        token = str(uuid.uuid4())
        expires_at = datetime.datetime.now() + datetime.timedelta(hours=12)
        cursor.execute("""
            INSERT INTO user_sessions (user_id, token, expires_at)
            VALUES (%s, %s, %s)
        """, (user_id, token, expires_at))
    st.session_state.user_id = user_id

def login_user(email, password):
    with get_cursor(dictionary=True) as (conn, cursor):
        cursor.execute(
            "SELECT * FROM users WHERE email=%s AND status='ACTIVE'",
            (email,)
        )
        user = cursor.fetchone()
    if not user:
        return None
    if user["password_hash"] != hash_password(password):
        return None
    create_session(user["user_id"])
    return user["user_id"]


# ═══════════════════════════════════════════════════════════
# SCREEN: LOGIN
# ═══════════════════════════════════════════════════════════

def show_login():
    st.title("Login")

    tab_login, tab_register = st.tabs(["Login", "Register"])

    with tab_login:
        email = st.text_input("Email", key="login_email")
        password = st.text_input("Password", type="password", key="login_pw")

        if st.button("Login", key="btn_login"):
            result = login_user(email, password)
            if result:
                st.session_state.screen = "dashboard"
                st.rerun()
            else:
                st.error("Invalid email or password.")

    with tab_register:
        new_email = st.text_input("Email", key="reg_email")
        new_pw = st.text_input("Password", type="password", key="reg_pw")

        if st.button("Register", key="btn_register"):
            try:
                pw_hash = hash_password(new_pw)
                with get_cursor(commit=True) as (conn, cursor):
                    cursor.execute(
                        "INSERT INTO users (email, password_hash) VALUES (%s, %s)",
                        (new_email, pw_hash),
                    )
                    user_id = cursor.lastrowid
                st.success(f"Registered! User ID: {user_id}. Please login.")
            except Exception as e:
                st.error(f"Registration failed: {e}")


# ═══════════════════════════════════════════════════════════
# SCREEN: DASHBOARD
# ═══════════════════════════════════════════════════════════

def show_dashboard():
    st.title("LedgerAI — Dashboard")

    col1, col2, col3 = st.columns([2, 1, 1])
    with col1:
        st.write(f"**User ID:** {st.session_state.user_id}")
    with col2:
        if st.button("Upload New Document", key="dash_upload"):
            st.session_state.screen = "upload"
            st.rerun()
    with col3:
        if st.button("Logout", key="dash_logout"):
            st.session_state.user_id = None
            st.session_state.screen = "login"
            st.rerun()

    st.divider()

    docs = get_user_documents(st.session_state.user_id)

    if not docs:
        st.info("No documents uploaded yet. Click 'Upload New Document' to start.")
        return

    for doc in docs:
        with st.container(border=True):
            c1, c2, c3 = st.columns([3, 1, 1])
            with c1:
                st.write(f"**{doc['file_name']}**")
                st.caption(f"Uploaded: {doc['created_at']}")
            with c2:
                status = doc["status"]
                if status in ("POSTED", "APPROVE"):
                    st.success(status)
                elif status == "FAILED":
                    st.error(status)
                elif status == "AWAITING_REVIEW":
                    st.warning(status)
                else:
                    st.info(status)
            with c3:
                if status == "AWAITING_REVIEW":
                    if st.button("Review", key=f"rev_{doc['document_id']}"):
                        st.session_state.current_document = doc["document_id"]
                        st.session_state.screen = "review"
                        st.rerun()
                elif status in ("EXTRACTING_TEXT", "IDENTIFYING_FORMAT",
                                "PARSING_TRANSACTIONS"):
                    if st.button("Status", key=f"stat_{doc['document_id']}"):
                        st.session_state.current_document = doc["document_id"]
                        st.session_state.screen = "processing"
                        st.rerun()


# ═══════════════════════════════════════════════════════════
# SCREEN: UPLOAD
# ═══════════════════════════════════════════════════════════

def show_upload():
    st.title("Upload Financial Statement")

    if st.button("← Back to Dashboard", key="upload_back"):
        st.session_state.screen = "dashboard"
        st.rerun()

    uploaded_file = st.file_uploader(
        "Upload PDF document",
        type=["pdf"],
        help="Bank statement, credit card statement, loan statement, etc.",
    )
    password = st.text_input("PDF Password (if any)", type="password")

    if uploaded_file and st.button("Start Processing", type="primary", key="upload_start"):

        # Save to temp file
        with tempfile.NamedTemporaryFile(delete=False, suffix=".pdf") as tmp:
            tmp.write(uploaded_file.read())
            file_path = tmp.name

        # Create document record
        document_id = create_document(
            user_id=st.session_state.user_id,
            file_name=uploaded_file.name,
            file_path=file_path,
            is_password_protected=bool(password),
            password=password if password else None,
        )

        # Submit to background queue
        submit_document(document_id)

        st.session_state.current_document = document_id
        st.session_state.screen = "processing"
        st.rerun()


# ═══════════════════════════════════════════════════════════
# SCREEN: PROCESSING (live status polling)
# ═══════════════════════════════════════════════════════════

def show_processing():
    st.title("Processing Document")

    doc_id = st.session_state.current_document
    if not doc_id:
        st.error("No document selected.")
        return

    if st.button("← Back to Dashboard", key="proc_back"):
        st.session_state.screen = "dashboard"
        st.rerun()

    status_placeholder = st.empty()
    progress_bar = st.progress(0)

    status_map = {
        "UPLOADED": (0.05, "Document uploaded..."),
        "EXTRACTING_TEXT": (0.20, "Extracting text from PDF..."),
        "IDENTIFYING_FORMAT": (0.40, "Identifying document format..."),
        "PARSING_TRANSACTIONS": (0.65, "Parsing transactions (dual pipeline)..."),
        "AWAITING_REVIEW": (1.00, "Processing complete! Ready for review."),
        "FAILED": (1.00, "Processing failed."),
    }

    MAX_POLLS = 120  # 2 minutes max
    for _ in range(MAX_POLLS):
        doc = get_document(doc_id)
        if not doc:
            st.error("Document not found.")
            return

        current_status = doc["status"]
        progress, message = status_map.get(current_status, (0.50, current_status))

        progress_bar.progress(progress)
        status_placeholder.info(message)

        # Terminal states
        if current_status == "AWAITING_REVIEW":
            st.success("Document processed successfully!")
            if st.button("Review Transactions", type="primary", key="proc_review"):
                st.session_state.screen = "review"
                st.rerun()
            return

        if current_status == "FAILED":
            st.error("Processing failed. Check logs for details.")
            if st.button("← Back to Dashboard", key="proc_fail_back"):
                st.session_state.screen = "dashboard"
                st.rerun()
            return

        time.sleep(1)

    st.warning("Processing is taking longer than expected. Check back later.")


# ═══════════════════════════════════════════════════════════
# SCREEN: REVIEW
# ═══════════════════════════════════════════════════════════

def show_review():
    st.title("Review Transactions")

    doc_id = st.session_state.current_document
    if not doc_id:
        st.error("No document selected.")
        return

    if st.button("← Back to Dashboard", key="review_back"):
        st.session_state.screen = "dashboard"
        st.rerun()

    review_data = get_document_for_review(doc_id)
    if not review_data:
        st.error("No review data found.")
        return

    doc = review_data["document"]
    code_txns = review_data["code_transactions"]
    llm_txns = review_data["llm_transactions"]
    final_parser = review_data["final_parser"]

    # ── Document Info ──
    with st.container(border=True):
        c1, c2 = st.columns(2)
        c1.metric("File", doc["file_name"])
        c2.metric("Status", doc["status"])

    # ── Winning Transactions ──
    st.subheader("Extracted Transactions")

    winning_txns = code_txns if final_parser == "CODE" else llm_txns
    winning_staging_id = (
        review_data["code_staging_id"] if final_parser == "CODE"
        else review_data["llm_staging_id"]
    )

    if winning_txns:
        df = pd.DataFrame(winning_txns)
        st.dataframe(df, use_container_width=True)
        st.info(f"Total: **{len(winning_txns)}** transactions")
    else:
        st.warning("No transactions extracted.")

    # ── Side-by-side Comparison (expandable) ──
    if code_txns and llm_txns:
        with st.expander("Compare CODE vs LLM Transactions"):
            col1, col2 = st.columns(2)
            with col1:
                st.write(f"**CODE** ({len(code_txns)} txns)")
                st.dataframe(pd.DataFrame(code_txns), use_container_width=True)
            with col2:
                st.write(f"**LLM** ({len(llm_txns)} txns)")
                st.dataframe(pd.DataFrame(llm_txns), use_container_width=True)

    # ── Approve Button ──
    st.divider()

    if doc["status"] == "AWAITING_REVIEW" and winning_txns:
        if st.button("Approve Transactions", type="primary", key="review_approve"):
            approve_transactions(
                document_id=doc_id,
                user_id=st.session_state.user_id,
                statement_id=doc.get("statement_id"),
                staging_transaction_id=winning_staging_id,
                transactions=winning_txns,
            )
            st.success("Transactions approved and moved to uncategorized!")
            st.session_state.screen = "dashboard"
            time.sleep(1)
            st.rerun()
    elif doc["status"] == "APPROVE":
        st.success("Already approved.")


# ═══════════════════════════════════════════════════════════
# ROUTER
# ═══════════════════════════════════════════════════════════

st.set_page_config(page_title="LedgerAI", layout="wide")

if st.session_state.user_id is None:
    show_login()
else:
    screen = st.session_state.screen
    if screen == "dashboard":
        show_dashboard()
    elif screen == "upload":
        show_upload()
    elif screen == "processing":
        show_processing()
    elif screen == "review":
        show_review()
    else:
        show_dashboard()