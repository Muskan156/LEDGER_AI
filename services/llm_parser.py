from groq import Groq
import os
import json

import google.generativeai as genai

GEMINI_API_KEY = os.getenv("gemini_key")
genai.configure(api_key=GEMINI_API_KEY)
MODEL_NAME = "models/gemini-3-flash-preview"


# ==========================================================
# LLM PARSER (Uses full_text directly – NO pdfplumber here)
# ==========================================================
# def parse_with_llm(full_text: str):

#     prompt = f"""
# You are a financial data extraction engine.

# Your task is to extract ALL financial transaction entries from the provided Indian financial document text.

# -------------------------------------------------------
# STEP 1 — IDENTIFY DOCUMENT TYPE
# -------------------------------------------------------
# First internally determine document type based on content. It may be one of:

# - BANK_STATEMENT
# - CREDIT_CARD
# - LOAN_STATEMENT
# - EMI_SCHEDULE
# - DEMAT_STATEMENT
# - MUTUAL_FUND_CAS
# - GST_STATEMENT
# - INSURANCE_STATEMENT
# - FD_STATEMENT
# - OTHER_FINANCIAL_STATEMENT

# Use keywords, column headers, and transaction patterns to identify type.

# Then apply extraction rules specific to that type.

# -------------------------------------------------------
# STEP 2 — TYPE-SPECIFIC RULES
# -------------------------------------------------------

# A) BANK_STATEMENT:
# - Extract all debit and credit entries.
# - Capture running balance if present.
# - Ignore Opening Balance / B/F / Closing Balance summary rows.
# - Preserve exact transaction order.

# B) CREDIT_CARD:
# - No running balance usually.
# - Extract transaction date, posting date (if available), description, amount.
# - Charges = debit.
# - Payments/refunds = credit.
# - running_balance = null.

# C) LOAN_STATEMENT / EMI_SCHEDULE:
# - Extract EMI date, principal, interest, total EMI amount.
# - If separate columns exist, map correctly.
# - running_balance = outstanding principal after payment.

# D) DEMAT_STATEMENT:
# - Extract trade date.
# - Buy = debit (money out).
# - Sell = credit (money in).
# - Quantity and ISIN can be appended inside details.
# - running_balance = null unless ledger balance present.

# E) MUTUAL_FUND_CAS:
# - Extract transaction date.
# - Purchase = debit.
# - Redemption = credit.
# - Include scheme name in details.
# - running_balance = units balance if available, else null.

# F) GST_STATEMENT:
# - Extract tax period date.
# - Debit = tax payable.
# - Credit = input credit/refund.
# - running_balance = tax ledger balance if available.

# G) INSURANCE_STATEMENT:
# - Extract premium payment date.
# - Premium paid = debit.
# - Claim payout = credit.
# - running_balance = null.

# H) FD_STATEMENT:
# - Extract deposit date.
# - Deposit = debit.
# - Interest payout = credit.
# - running_balance = principal balance if shown.

# -------------------------------------------------------
# STEP 3 — STRICT EXTRACTION RULES
# -------------------------------------------------------

# 1. Extract EVERY transaction row across ALL pages.
# 1A. END-OF-TABLE ENFORCEMENT:
#    Continue extracting until the last visible transaction row 
#    on the final page.
#    Do NOT stop extraction early due to:
#    - Page footers
#    - EMI eligibility notes
#    - Advertisement blocks
#    - Blank lines
#    - Section separators

# 2. DO NOT skip rows.

# 3. Merge multi-line descriptions into a single entry.

# 4. A new transaction row MUST start with a valid date pattern 
#    (DD-MM-YYYY, DD/MM/YYYY, DD-MMM-YYYY, etc.).

#    If a line does NOT begin with a valid date pattern,
#    it MUST NOT be treated as a new transaction.

# 5. CONTINUATION LINE RULE (VERY STRICT):

#    If a line:
#    - does NOT start with a valid date
#    - appears vertically aligned under the Narration/Particulars column
#    - and the previous line was a transaction row

#    THEN:
#    → Append that line to the previous transaction's "details" field.
#    → Preserve exact wording.
#    → Insert a single space between merged lines.
#    → Do NOT create a new transaction.

#    Even if the continuation line contains numbers or reference-like values,
#    it must still be appended if it does not start with a date.

# 5A. NON-TRANSACTION ROW FILTER:

#    If a row:
#    - Has no debit AND no credit amount
#    - AND contains keywords like:
#        Opening Balance, B/F, Brought Forward, Closing Balance,
#        Grand Total, Total, Summary
#    - OR only contains a balance value

#    Then IGNORE the row completely.
#    It must not be included as a transaction.

# 6A. COLUMN BOUNDARY ENFORCEMENT (VERY STRICT):

# - If the document contains structured columns such as:
#   Date | Narration | Cheq./Ref.No. | Value Dt | Withdrawal | Deposit | Balance

# - ONLY extract text from the Narration/Description column into "details".

# - DO NOT append values from:
#     • Cheq./Ref.No.
#     • Value Date column
#     • Withdrawal column
#     • Deposit column
#     • Balance column

# - If a numeric value appears aligned under Cheq./Ref.No., it must NOT be added to details.

# - Reference numbers (long numeric strings 10+ digits) that appear in a separate column must be ignored unless they are clearly printed inside the Narration column itself.

# - If a value is spatially aligned under a different column header, treat it as belonging to that column only.

# - Never merge adjacent column text into description.

# 6B. ADJACENT COLUMN PROTECTION:

#    If a word is vertically aligned under another column header 
#    (e.g., Tran Type, Dr/Cr, Value Date),
#    it MUST NOT be appended to "details".

#    Even if the word is short (e.g., TFR, DR, CR),
#    it belongs only to its respective column.

# 7. Ignore headers, footers, advertisements, page numbers.

# 8. Ignore summary totals.

# 9. DO NOT invent missing balances.

# 10. If running balance not present, return null.

# 11. Preserve chronological order exactly as document.

# -------------------------------------------------------
# STEP 4 — DATA NORMALIZATION
# -------------------------------------------------------

# - Convert all dates to DD-MM-YYYY.
# - Amounts must be numeric only (no commas, no currency symbols).
# - Debit and credit cannot both be non-null.
# - If only one amount column exists:
#     infer debit/credit using keywords:
#         DR, Debit, Withdrawal, Purchase, Charge → debit
#         CR, Credit, Deposit, Salary, Refund, Payment → credit
# - Remove isolated numeric tokens longer than 8 digits from "details" if they belong to reference/check number columns.
# - details must contain the FULL narration text exactly as printed in the Narration/Particulars column, including all continuation lines.
# - Do NOT trim, shorten, clean, or normalize narration wording.
# - details must never end with a pure numeric reference unless it is explicitly printed inside narration text.

# -------------------------------------------------------
# STEP 5 — OUTPUT FORMAT (STRICT)
# -------------------------------------------------------

# Return STRICT VALID JSON only.
# No explanations.
# No markdown.
# No extra text.

# Output format:

# [
#   {{
#     "date": "DD-MM-YYYY",
#     "details": "string",
#     "debit": number or null,
#     "credit": number or null,
#     "balance": number or null,
#     "confidence": number between 0 and 1
#   }}
# ]

# -------------------------------------------------------
# DOCUMENT TEXT:
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

def parse_with_llm(full_text: str, identifier_json: dict):

    document_family = identifier_json.get("document_family")
    document_subtype = identifier_json.get("document_subtype")

    identity = identifier_json.get("identity_markers", {})
    table_identity = identity.get("transaction_table_identity", {})

    table_headers = table_identity.get("table_header_markers", [])
    has_running_balance = table_identity.get("presence_of_running_balance", False)
    debit_credit_style = table_identity.get("debit_credit_style", False)

    prompt = f"""
You are a deterministic financial transaction extraction engine.

DOCUMENT TYPE:
{document_family}

DOCUMENT SUBTYPE:
{document_subtype}

KNOWN TABLE STRUCTURE:
Headers: {table_headers}
Has Running Balance: {has_running_balance}
Debit/Credit Style: {debit_credit_style}

You MUST follow this known structure.
Do NOT re-classify the document.
Do NOT infer new structure.

-------------------------------------------------------
EXTRACTION RULES
-------------------------------------------------------

1. Extract EVERY transaction row across ALL pages.
2. A transaction row MUST begin with a valid date.
3. If a line does not begin with a date but is aligned under narration,
   append it to previous transaction.
4. Ignore summary rows (Opening Balance, Total, Grand Total, etc.)
5. Preserve chronological order exactly.

-------------------------------------------------------
BALANCE HANDLING
-------------------------------------------------------

If Has Running Balance = True:
    Extract running balance.
Else:
    balance must be null.

-------------------------------------------------------
DEBIT / CREDIT HANDLING
-------------------------------------------------------

If Debit/Credit Style = True:
    Use separate debit and credit columns.

If only one amount column:
    Infer using keywords:
        DR, Withdrawal, Purchase → debit
        CR, Deposit, Refund → credit

Debit and credit cannot both be non-null.

-------------------------------------------------------
STRICT OUTPUT FORMAT
-------------------------------------------------------

Return STRICT VALID JSON array only.
No explanation.
No markdown.

[
  {{
    "date": "DD-MM-YYYY",
    "details": "exact narration text",
    "debit": number or null,
    "credit": number or null,
    "balance": number or null,
    "confidence": number between 0 and 1
  }}
]

-------------------------------------------------------
DOCUMENT TEXT:
{full_text}
"""

    model = genai.GenerativeModel(MODEL_NAME)

    response = model.generate_content(
       prompt,
       generation_config={
        "temperature": 0
    })

    llm_response = response.text.strip()
    return llm_response