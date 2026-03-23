"""
services/llm_parser.py
──────────────────────
STEP 4 METHOD 2 — Direct LLM transaction extraction.

Sends full text + identifier to Claude (via Anthropic API), which returns
structured transaction JSON directly.
"""

import json
import logging

import anthropic
from config import ANTHROPIC_API_KEY, ANTHROPIC_MODEL_NAME

client = anthropic.Anthropic(api_key=ANTHROPIC_API_KEY)
logger = logging.getLogger("ledgerai.llm_parser")


def parse_with_llm(full_text: str, identifier_json: dict) -> str:
    """
    Ask Claude to directly extract transactions from the document text.
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
3. DETAILS field must contain the transaction narration/description EXACTLY as it appears in the document.
   - Copy the full description text character-for-character. Do NOT shorten, summarise, or rewrite it.
   - Remove ONLY page numbers, page headers, and footer text if they are embedded in the description.
   - Do NOT remove account numbers, reference numbers, or any part of the narration.
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

    message = client.messages.create(
        model=ANTHROPIC_MODEL_NAME,
        max_tokens=8096,
        temperature=0,
        messages=[
            {"role": "user", "content": prompt}
        ],
    )

    llm_response = message.content[0].text.strip()
    logger.info("LLM parse complete: response_len=%d", len(llm_response))

    return llm_response