# from db.connection import get_connection
# import json

# #=================================Fetch All Active Categories=====================================#
# def get_active_statement_categories():
#     conn = get_connection()
#     cursor = conn.cursor(dictionary=True)

#     query = """
#         SELECT * FROM statement_categories
#         WHERE status = 'ACTIVE'
#     """

#     cursor.execute(query)
#     rows = cursor.fetchall()

#     cursor.close()
#     conn.close()

#     for row in rows:
#         row["statement_identifier"] = json.loads(row["statement_identifier"])
#         row["extraction_logic"] = json.loads(row["extraction_logic"])

#     return rows

# #==================================Insert New Category (LLM Generated)========================================#
# def insert_statement_category(
#     statement_type,
#     format_name,
#     institution_name,
#     identifier_json,
#     extraction_logic_json,
#     threshold=65.00
# ):
#     conn = get_connection()
#     cursor = conn.cursor()

#     query = """
#         INSERT INTO statement_categories
#         (statement_type, format_name, institution_name,
#          statement_identifier, extraction_logic,
#          match_threshold, logic_version, status)
#         VALUES (%s, %s, %s, %s, %s, %s, 1, 'UNDER_REVIEW')
#     """

#     cursor.execute(
#         query,
#         (
#             statement_type,
#             format_name,
#             institution_name,
#             json.dumps(identifier_json),
#             json.dumps(extraction_logic_json),
#             threshold
#         )
#     )

#     conn.commit()

#     inserted_id = cursor.lastrowid

#     cursor.close()
#     conn.close()

#     return inserted_id

# #==============================Activate Category After Validation====================================#
# def activate_statement_category(statement_id):
#     conn = get_connection()
#     cursor = conn.cursor()

#     query = """
#         UPDATE statement_categories
#         SET status = 'ACTIVE'
#         WHERE statement_id = %s
#     """

#     cursor.execute(query, (statement_id,))
#     conn.commit()

#     cursor.close()
#     conn.close()

import json
from db.connection import get_connection


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

# ================================= Fetch All Active Categories =================================
def get_active_statement_categories():
    conn = get_connection()
    cursor = conn.cursor(dictionary=True)

    query = """
        SELECT *
        FROM statement_categories
        WHERE status = 'ACTIVE'
    """

    cursor.execute(query)
    rows = cursor.fetchall()

    cursor.close()
    conn.close()

    for row in rows:
        row["statement_identifier"] = json.loads(row["statement_identifier"])
        row["extraction_logic"] = json.loads(row["extraction_logic"])

    return rows


# ================================= Insert New Category =================================
def insert_statement_category(
    statement_type,
    format_name,
    institution_name,
    ifsc_code,                 
    identifier_json,
    extraction_logic_json,
    threshold=65.00
):
    conn = get_connection()
    cursor = conn.cursor()

    query = """
        INSERT INTO statement_categories
        (
            statement_type,
            format_name,
            institution_name,
            ifsc_code,                      
            statement_identifier,
            extraction_logic,
            match_threshold,
            logic_version,
            status
        )
        VALUES (%s, %s, %s, %s, %s, %s, %s, 1, 'UNDER_REVIEW')
    """

    cursor.execute(
        query,
        (
            statement_type,
            format_name,
            institution_name,
            ifsc_code,                      
            json.dumps(identifier_json),
            extraction_logic_json,
            threshold
        )
    )

    conn.commit()

    inserted_id = cursor.lastrowid

    cursor.close()
    conn.close()

    return inserted_id


# ============================== Activate Category After Validation ==============================
def activate_statement_category(statement_id):
    conn = get_connection()
    cursor = conn.cursor()

    query = """
        UPDATE statement_categories
        SET status = 'ACTIVE'
        WHERE statement_id = %s
    """

    cursor.execute(query, (statement_id,))
    conn.commit()

    cursor.close()
    conn.close()
#============================== Fetch All Under Review Categories ==============================
def get_under_review_formats():
    conn = get_connection()
    cursor = conn.cursor(dictionary=True)

    query = """
        SELECT *
        FROM statement_categories
        WHERE status = 'UNDER_REVIEW'
    """

    cursor.execute(query)
    rows = cursor.fetchall()

    cursor.close()
    conn.close()

    for row in rows:
        row["statement_identifier"] = json.loads(row["statement_identifier"])
        row["extraction_logic"] = json.loads(row["extraction_logic"])

    return rows


def get_statement_by_id(statement_id):
    conn = get_connection()
    cursor = conn.cursor(dictionary=True)

    query = """
        SELECT * FROM statement_categories
        WHERE statement_id = %s
    """

    cursor.execute(query, (statement_id,))
    result = cursor.fetchone()

    cursor.close()
    conn.close()

    return result


def update_extraction_logic(statement_id, new_logic):
    conn = get_connection()
    cursor = conn.cursor()

    query = """
        UPDATE statement_categories
        SET extraction_logic = %s
        WHERE statement_id = %s
    """

    cursor.execute(query, (new_logic, statement_id))
    conn.commit()
    cursor.close()
    conn.close()
