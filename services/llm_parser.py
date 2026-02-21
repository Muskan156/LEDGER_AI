# from groq import Groq
# import os
# import json

# client = Groq(api_key=os.getenv("GROQ_API_KEY"))
# MODEL_NAME = "llama-3.1-8b-instant"


# # ==========================================================
# # LLM PARSER (Uses full_text directly – NO pdfplumber here)
# # ==========================================================
# def parse_with_llm(full_text: str):

#     prompt = f"""
# You are extracting transactions from an Indian bank statement.

# STRICT RULES:
 
# 1. Return STRICT JSON only.
# 2. No explanation text.
# 3. Do not summarize.
# 4. Extract every transaction row from all pages.
# 5. Ignore headers, footers, advertisements, page numbers.
# 6. Ignore opening balance rows like B/F, Opening Balance.
# 7. If debit and credit columns exist:
#    - Debit > 0 → type = "DEBIT"
#    - Credit > 0 → type = "CREDIT"
# 8. Running balance must be captured if available.
# 9. Dates must be returned in DD-MM-YYYY format.
# 10. Amounts must be numeric (no commas).
# Return STRICT valid JSON only.

# Output format:
# [
#   {{
#     "date": "YYYY-MM-DD",
#     "details": "text",
#     "debit": number or null,
#     "credit": number or null,
#     "balance": number,
#     "confidence": number between 0 and 1
#   }}
# ]
# Transaction Rows:
# {full_text}
# """

#     response = client.chat.completions.create(
#         model=MODEL_NAME,
#         messages=[{"role": "user", "content": prompt}],
#         temperature=0
#     )

#     llm_response = response.choices[0].message.content.strip()

#     print("\n--- RAW LLM RESPONSE ---\n")
#     print(llm_response[:2000])
#     print("\n-------------------------\n")

#     return llm_response
import os
import json
import time
import pikepdf
import google.generativeai as genai
from db.connection import get_connection

API_KEY = os.getenv("GEMINI_API_KEY")
MODEL_ID = "gemini-2.5-flash"

genai.configure(api_key=API_KEY)


def fetch_document_from_db(document_id: int):
    conn = get_connection()
    cursor = conn.cursor(dictionary=True)

    cursor.execute("""
        SELECT file_path, is_password_protected
        FROM documents
        WHERE document_id = %s
    """, (document_id,))
    doc = cursor.fetchone()

    cursor.close()
    conn.close()

    if not doc:
        raise ValueError("Document not found")

    return doc["file_path"], doc["is_password_protected"]


def parse_with_llm(document_id: int):

    file_path, is_protected = fetch_document_from_db(document_id)

    clean_pdf = "temp_clean.pdf"

    # 🔐 Handle password if needed
    if is_protected:
        conn = get_connection()
        cursor = conn.cursor(dictionary=True)

        cursor.execute("""
            SELECT encrypted_password
            FROM document_password
            WHERE document_id = %s
        """, (document_id,))
        row = cursor.fetchone()

        cursor.close()
        conn.close()

        password = row["encrypted_password"]

        with pikepdf.open(file_path, password=password) as pdf:
            pdf.save(clean_pdf)
        upload_path = clean_pdf
    else:
        upload_path = file_path

    # 🚀 Upload to Gemini
    sample_file = genai.upload_file(path=upload_path)

    while sample_file.state.name == "PROCESSING":
        time.sleep(2)
        sample_file = genai.get_file(sample_file.name)

    extraction_prompt = """
    Extract ALL transactions from this Indian bank statement.

    STRICT RULES:
    - Return STRICT JSON only.
    - Extract every transaction.
    - Date format: YYYY-MM-DD
    - Numeric amounts only
    - Ignore headers and opening balance

    RETURN FORMAT:
    {
      "transactions": [
        {
          "date": "",
          "description": "",
          "DEBIT": 0.0,
          "CREDIT": 0.0,
          "running_balance": 0.0
        }
      ]
    }
    """

    model = genai.GenerativeModel(MODEL_ID)

    response = model.generate_content(
        [sample_file, extraction_prompt],
        generation_config={
            "temperature": 0,
            "response_mime_type": "application/json"
        }
    )

    try:
        gemini_json = json.loads(response.text)
        txns = gemini_json.get("transactions", [])

        # 🔄 Convert to your internal format
        normalized = []

        for t in txns:
            normalized.append({
                "date": t.get("date"),
                "details": t.get("description"),
                "debit": t.get("DEBIT"),
                "credit": t.get("CREDIT"),
                "balance": t.get("running_balance"),
                "confidence": 0.95
            })

        return json.dumps(normalized)

    except Exception:
        return "[]"

    finally:
        if os.path.exists(clean_pdf):
            os.remove(clean_pdf)