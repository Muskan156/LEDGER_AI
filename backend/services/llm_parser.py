"""
services/llm_parser.py
──────────────────────
STEP 4 METHOD 2 — Direct LLM transaction extraction.

Sends full text + identifier to Gemini, which returns
structured transaction JSON directly.
"""

import json
import logging

from google import genai
from config import GEMINI_API_KEY, LLM_PARSER_MODEL
from services.llm_retry import call_with_retry

client = genai.Client(api_key=GEMINI_API_KEY)
logger = logging.getLogger("ledgerai.llm_parser")


def parse_with_llm(full_text: str, identifier_json: dict) -> str:
    """
    Ask Gemini to directly extract transactions from the document text.
    Returns raw LLM response string (caller must parse JSON from it).
    """
    doc_family = identifier_json.get("document_family", "BANK_ACCOUNT_STATEMENT")
    doc_subtype = identifier_json.get("document_subtype", "")

    prompt = f"""
You are a financial data extraction engine.

Extract ALL transaction entries from the provided document text.

════════════════════════════════════════════
DOCUMENT INFO
════════════════════════════════════════════
Document Family: {doc_family}
Document Subtype: {doc_subtype}
Institution: {identifier_json.get("institution_name", "Unknown")}

════════════════════════════════════════════
RULES
════════════════════════════════════════════

1. Extract EVERY transaction row. A transaction starts with a date.
2. SKIP these entirely — they are NOT transactions:
   - Headers (Date, Particulars, Debit, Credit, Balance)
   - Footers (Page numbers, disclaimers, generated on...)
   - Summary rows (Opening Balance, Closing Balance, Total Debit/Credit)
   - Account info (Branch, IFSC, MICR, Account Number)
3. DETAILS field must contain ONLY the transaction narration/description:
   - Do NOT include dates, amounts, page numbers, or header text in details.
   - Do NOT include footer text, branch info, or account numbers in details.
   - Example GOOD: "NEFT CR ACME CORP SALARY"
   - Example BAD: "01/01/2025 NEFT CR ACME CORP 50000.00 Page 1 of 3"
4. Handle Indian number formats (1,00,000.00).
5. Normalize dates to YYYY-MM-DD.
6. DEBIT/CREDIT: Every transaction MUST have either debit or credit filled (not both None).
   - If running balance increases, the amount is credit.
   - If running balance decreases, the amount is debit.
   - If column headers say Withdrawal/Debit use those.
   - If column headers say Deposit/Credit use those.

════════════════════════════════════════════
OUTPUT FORMAT (JSON ARRAY)
════════════════════════════════════════════

[
  {{
    "date": "YYYY-MM-DD",
    "details": "<transaction description only, no dates/amounts/noise>",
    "debit": <float or null>,
    "credit": <float or null>,
    "balance": <float or null>,
    "confidence": <0.0 to 1.0>
  }}
]

════════════════════════════════════════════
DOCUMENT TEXT
════════════════════════════════════════════

{full_text}

════════════════════════════════════════════
Return ONLY the JSON array. No markdown. No explanation.
"""

    logger.info("Starting LLM parse: family=%s, text_len=%d",
                doc_family, len(full_text))

    response = call_with_retry(
        client, LLM_PARSER_MODEL, prompt,
        config={"temperature": 0},
    )

    llm_response = response.text.strip()
    logger.info("LLM parse complete: response_len=%d", len(llm_response))

    return llm_response