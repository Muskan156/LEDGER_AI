"""
repository/statement_category_repo.py
──────────────────────────────────────
CRUD for statement_categories table.
Status enums: ACTIVE | UNDER_REVIEW | DISABLED | EXPERIMENTAL
"""

import json
import logging
from db.connection import get_cursor

logger = logging.getLogger("ledgerai.statement_category_repo")


# ── FETCH ────────────────────────────────────────────────

def get_all_matchable_formats():
    """Fetch ACTIVE + UNDER_REVIEW + EXPERIMENTAL formats for matching."""
    with get_cursor(dictionary=True) as (conn, cursor):
        cursor.execute("""
            SELECT * FROM statement_categories
            WHERE status IN ('ACTIVE','UNDER_REVIEW','EXPERIMENTAL')
        """)
        rows = cursor.fetchall()

    for row in rows:
        if isinstance(row.get("statement_identifier"), str):
            row["statement_identifier"] = json.loads(row["statement_identifier"])
    return rows


def get_active_formats():
    with get_cursor(dictionary=True) as (conn, cursor):
        cursor.execute("SELECT * FROM statement_categories WHERE status='ACTIVE'")
        rows = cursor.fetchall()

    for row in rows:
        if isinstance(row.get("statement_identifier"), str):
            row["statement_identifier"] = json.loads(row["statement_identifier"])
    return rows


def get_statement_by_id(statement_id: int):
    with get_cursor(dictionary=True) as (conn, cursor):
        cursor.execute(
            "SELECT * FROM statement_categories WHERE statement_id=%s",
            (statement_id,),
        )
        row = cursor.fetchone()
    if row and isinstance(row.get("statement_identifier"), str):
        row["statement_identifier"] = json.loads(row["statement_identifier"])
    return row


# ── INSERT ───────────────────────────────────────────────

def insert_statement_category(
    statement_type: str,
    format_name: str,
    institution_name: str,
    identifier_json: dict,
    extraction_logic: str,
    ifsc_code: str = None,
    threshold: float = 65.0,
):
    sql = """
        INSERT INTO statement_categories
        (statement_type, format_name, institution_name, ifsc_code,
         statement_identifier, extraction_logic,
         match_threshold, logic_version, status)
        VALUES (%s, %s, %s, %s, %s, %s, %s, 1, 'UNDER_REVIEW')
    """
    params = (
        statement_type,
        format_name,
        institution_name,
        ifsc_code,
        json.dumps(identifier_json),
        extraction_logic,
        threshold,
    )
    
    with get_cursor(commit=True, prepared=True) as (conn, cursor):
        # Use prepared statement for safer handling of large code blocks
        # Some MySQL drivers struggle with %s substitution in multi-line strings
        cursor.execute(sql, params)
        return cursor.lastrowid


# ── STATUS ───────────────────────────────────────────────

def activate_statement_category(statement_id: int):
    with get_cursor(commit=True) as (conn, cursor):
        cursor.execute("""
            UPDATE statement_categories
            SET status='ACTIVE', last_verified_at=NOW()
            WHERE statement_id=%s
        """, (statement_id,))
    logger.info("Statement %s → ACTIVE.", statement_id)


def update_statement_status(statement_id: int, status: str):
    with get_cursor(commit=True) as (conn, cursor):
        cursor.execute(
            "UPDATE statement_categories SET status=%s WHERE statement_id=%s",
            (status, statement_id),
        )


def update_extraction_logic(statement_id: int, new_logic: str):
    with get_cursor(commit=True, prepared=True) as (conn, cursor):
        cursor.execute("""
            UPDATE statement_categories
            SET extraction_logic=%s,
                logic_version = logic_version + 1
            WHERE statement_id=%s
        """, (new_logic, statement_id))


def update_success_rate(statement_id: int, rate: float):
    with get_cursor(commit=True) as (conn, cursor):
        cursor.execute("""
            UPDATE statement_categories
            SET success_rate=%s, last_verified_at=NOW()
            WHERE statement_id=%s
        """, (rate, statement_id))
