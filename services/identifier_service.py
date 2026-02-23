# import re
# import json
# import os
# from typing import Dict, Tuple, List
# from groq import Groq
# from repository.statement_category_repo import (
#     get_active_statement_categories,
#     insert_statement_category
# )
# from services.pdf_service import extract_pages
# # ------------------- CONFIGURE LLM -------------------
# client = Groq(api_key=os.getenv("GROQ_API_KEY"))
# # ---------------------------------------------------
# # BANK CODE → BANK NAME MAPPING
# # ---------------------------------------------------
# BANK_CODE_MAP = {
#     "SBIN": "State Bank of India",
#     "HDFC": "HDFC Bank",
#     "ICIC": "ICICI Bank",
#     "BKID": "Bank of India",
#     "PUNB": "Punjab National Bank",
#     "BARB": "Bank of Baroda",
#     "KKBK": "Kotak Mahindra Bank"
# }

# # ---------------------------------------------------
# # DERIVE BANK NAME (IFSC + FALLBACK)
# # ---------------------------------------------------
# def derive_bank_name_from_ifsc(text: str):

#     # First try IFSC pattern
#     match = re.search(r"\b([A-Z]{4})0[A-Z0-9]{6}\b", text)

#     if match:
#         bank_code = match.group(1)
#         bank_name = BANK_CODE_MAP.get(bank_code)
#         return bank_name, bank_code

#     # 🔥 Fallback detection (credit card or non-IFSC docs)
#     for code, name in BANK_CODE_MAP.items():
#         if name.lower() in text.lower():
#             return name, code

#     return None, None


# # ------------------- RULE HELPERS -------------------
# def regex_match(text: str, pattern: str) -> bool:
#     return bool(re.search(pattern, text, re.I | re.S))


# def keyword_presence(text: str, patterns: List[str]) -> bool:
#     t = text.lower()
#     return any(p.lower() in t for p in patterns)


# def apply_rule(text: str, rule: Dict) -> bool:
#     if rule["rule"] == "regex":
#         return regex_match(text, rule["pattern"])
#     if rule["rule"] == "keyword":
#         return keyword_presence(text, rule["patterns"])
#     return False


# def table_header_match(text: str, headers: List[str]) -> Tuple[int, int]:
#     t = re.sub(r"\s+", "", text.lower())
#     matched = 0
#     for h in headers:
#         h_norm = re.sub(r"\s+", "", h.lower())
#         if h_norm in t:
#             matched += 1
#     return matched, len(headers)


# # ------------------- IDENTIFIER EVALUATION -------------------
# def evaluate_identifier(identifier: Dict, text: str) -> Tuple[float, Dict]:
#     score = 0
#     max_score = 0
#     details = {}

#     sim = identifier["statement_identifier"]

#     # ---------------- BANK ----------------
#     bank_patterns = (
#         sim.get("bank_identification", {})
#            .get("bank_name", {})
#            .get("patterns", [])
#     )

#     if bank_patterns:
#         max_score += 20
#         matched = any(p.lower() in text.lower() for p in bank_patterns)
#         if matched:
#             score += 20
#         details["bank"] = matched

#     # ---------------- HEADER ----------------
#     headers = sim.get("header_markers", {}).get("patterns", [])
#     if headers:
#         max_score += 20
#         matched = any(h.lower() in text.lower() for h in headers)
#         if matched:
#             score += 20
#         details["header"] = matched

#     # ---------------- FOOTER ----------------
#     footers = sim.get("footer_markers", {}).get("patterns", [])
#     if footers:
#         max_score += 15
#         matched = any(f.lower() in text.lower() for f in footers)
#         if matched:
#             score += 15
#         details["footer"] = matched

#     # ---------------- TABLE HEADERS ----------------
#     table_headers = (
#         sim.get("table_structure", {})
#            .get("column_headers", [])
#     )

#     if table_headers:
#         max_score += 25
#         matched_count = sum(
#             1 for h in table_headers if h.lower() in text.lower()
#         )

#         if matched_count >= 3:
#             score += 25

#         details["table_match_count"] = matched_count

#     # ---------------- DATE PATTERN ----------------
#     date_pattern = (
#         sim.get("transaction_anchor", {})
#            .get("date_pattern", "")
#     )

#     if date_pattern:
#         max_score += 10
#         if re.search(date_pattern, text):
#             score += 10
#             details["date_pattern"] = True
#         else:
#             details["date_pattern"] = False

#     # ---------------- AMOUNT PATTERN ----------------
#     amount_pattern = sim.get("amount_pattern", "")
#     if amount_pattern:
#         max_score += 10
#         if re.search(amount_pattern, text):
#             score += 10
#             details["amount_pattern"] = True
#         else:
#             details["amount_pattern"] = False

#     confidence = round((score / max_score) * 100, 2) if max_score else 0
#     return confidence, details


# # ------------------- IDENTIFICATION -------------------
# def identify_statement(text: str, threshold: float = 65.0) -> Tuple[bool, Dict]:

#     best = {"id": None, "score": 0, "details": None}
#     categories = get_active_statement_categories()

#     for cat in categories:
#         score, details = evaluate_identifier(cat, text)

#         if score > best["score"]:
#             best = {
#                 "id": cat["statement_id"],
#                 "score": score,
#                 "details": details
#             }

#     if best["score"] >= threshold:
#         return True, best

#     return False, best


# # ------------------- REDUCE TEXT FOR LLM -------------------
# def reduce_text_for_llm(pages: List[str]) -> Dict:

#     header_lines = []

#     for p in pages:
#         for line in p.splitlines():

#             # Detect probable header rows
#             if re.search(r"\b(Date|Debit|Credit|Balance|Withdrawals|Deposits|Description|Narration)\b", line, re.I):

#                 # 🔥 Split by multiple spaces (fix Kotak merged header issue)
#                 parts = re.split(r"\s{2,}", line.strip())

#                 for part in parts:
#                     clean_part = part.strip()
#                     if clean_part and ":" not in clean_part:
#                         header_lines.append(clean_part)

#     # Remove duplicates while preserving order
#     header_lines = list(dict.fromkeys(header_lines))

#     return {
#         "first_page": pages[0][:3000] if pages else "",
#         "last_page": pages[-1][-3000:] if pages else "",
#         "table_headers": header_lines[:15]
#     }


# # ------------------- GENERATE IDENTIFIER VIA LLM -------------------
# def generate_identifier_llm(reduced: Dict) -> Dict:
#     prompt = f"""
# You are a Senior Banking Document Format Architect.

# Your task is to generate a reusable FORMAT IDENTIFIER SCHEMA
# for a BANK STATEMENT.

# This schema will later be used to automatically detect
# and parse statements of this exact bank format.

# ============================================================
# CRITICAL RULES (NON-NEGOTIABLE)
# ============================================================

# 1. DO NOT include:
#    - Customer names
#    - Addresses
#    - Emails
#    - Account numbers
#    - CIF numbers
#    - Phone numbers
#    - Transaction values
#    - Any numeric IDs
#    - Any dynamic content

# 2. Extract ONLY structural constants that appear
#    on ALL statements of this bank format.

# 3. If multiple tables exist:
#    - IGNORE account summary tables
#    - IGNORE metadata blocks
#    - IGNORE relationship summaries
#    - ONLY extract the MAIN TRANSACTION TABLE

# 4. DO NOT invent headers.
# 5. DO NOT invent regex.
# 6. If unsure, return empty list [].
# 7. Return STRICT JSON ONLY.
# 8. No markdown.
# 9. No explanation text.
# 10. No comments.
# 11. No trailing commas.
# 12. No extra keys.

# ============================================================
# REQUIRED JSON STRUCTURE (EXACTLY THIS)
# ============================================================

# {{
#   "bank_identification": {{
#     "bank_name": {{
#       "patterns": []
#     }}
#   }},
#   "header_markers": {{
#     "patterns": []
#   }},
#   "footer_markers": {{
#     "patterns": []
#   }},
#   "metadata_keywords": [],
#   "table_structure": {{
#     "column_headers": []
#   }},
#   "transaction_anchor": {{
#     "date_pattern": ""
#   }},
#   "amount_pattern": ""
# }}

# ============================================================
# FIELD DEFINITIONS
# ============================================================

# bank_identification.patterns:
#   • ONLY static bank name text
#   • Example: "State Bank of India"
#   • NEVER include customer names

# header_markers.patterns:
#   • Static phrases that appear at the top of every statement
#   • Examples:
#         "STATEMENT OF ACCOUNT"
#         "Account Statement"
#         "Statement From"

# footer_markers.patterns:
#   • Static closing markers
#   • Examples:
#         "Page no."
#         "*** End of Statement ***"

# metadata_keywords:
#   • Words indicating NON-TRANSACTION lines
#   • Examples:
#         "Opening Balance"
#         "Closing Balance"
#         "Summary"
#         "Account Summary"

# ============================================================
# STRICT TABLE HEADER EXTRACTION RULES
# ============================================================

# You MUST extract transaction table headers ONLY from:

# TABLE HEADERS DETECTED:
# {json.dumps(reduced['table_headers'])}

# Rules:

# 1. ONLY choose from the provided TABLE HEADERS DETECTED list.
# 2. DO NOT extract from first page paragraphs.
# 3. DO NOT include lines with ":".
# 4. DO NOT include summary blocks.
# 5. A valid transaction header MUST contain:
#    - A date column
#    - A debit/withdrawal column
#    - A credit/deposit column
#    - A balance column

# If this condition fails:
#     return "column_headers": []

# ============================================================
# TRANSACTION DATE PATTERN (STRICT)
# ============================================================

# transaction_anchor.date_pattern must match
# ONLY transaction row dates.

# It must support:

# • 01/12/2025
# • 1/2/2025
# • 01-12-2025
# • 01-12-25
# • 14 Nov 2025
# • 14-Jan-2026
# • 2025-12-01

# Return this EXACT robust pattern:

# "\\d{{1,2}}[ \\/\\-](?:\\d{{1,2}}|[A-Za-z]{{3}})[ \\/\\-]\\d{{2,4}}|\\d{{4}}-\\d{{2}}-\\d{{2}}"

# DO NOT return narrower patterns.

# ============================================================
# AMOUNT PATTERN (LOCKED - DO NOT MODIFY)
# ============================================================

# amount_pattern MUST be EXACTLY:

# "\\d+(?:,\\d{{2}})*(?:,\\d{{3}})*\\.\\d{{2}}"

# DO NOT generate your own money regex.

# ============================================================
# DOCUMENT CONTENT
# ============================================================

# FIRST PAGE (CLEANED):
# {reduced['first_page']}

# TABLE REGION SAMPLE:
# {json.dumps(reduced['table_headers'])}

# LAST PAGE:
# {reduced['last_page']}

# Remember:
# Return STRICT JSON only.
# No explanation.
# No markdown.
# """

#     response = client.chat.completions.create(
#         model="llama-3.1-8b-instant",
#         messages=[{"role": "user", "content": prompt}],
#         temperature=0
#     )

#     raw_output = response.choices[0].message.content.strip()
#     # ---------------- SAFE JSON EXTRACTION ----------------
#     import re

#     raw_output = raw_output.strip()

#     # Remove markdown fences if present
#     if "```" in raw_output:
#        parts = raw_output.split("```")
#        raw_output = parts[1] if len(parts) > 1 else parts[0]

#     raw_output = raw_output.strip()

#     # Extract JSON object safely
#     start = raw_output.find("{")
#     end = raw_output.rfind("}")

#     if start == -1 or end == -1:
#         print("Raw LLM output:\n", raw_output)
#         raise ValueError("No JSON returned from LLM.")

#     json_string = raw_output[start:end+1]

#     # 🔥 FIX ESCAPE ISSUES
#     json_string = json_string.replace("\\/", "/")
#     json_string = json_string.replace("\\-", "-")
#     # json_string = raw_output[start:end+1]
#     # json_string = re.sub(r'(?<!\\)\\(?!["\\/bfnrt])',r'\\\\',json_string)
#     try:
#         identifier_json = json.loads(json_string)
#     except Exception as e:
#         print("Raw LLM output:\n", raw_output)
#         print(raw_output)
#         print("Fixed JSON string:\n", json_string)
#         raise ValueError(f"Invalid JSON returned from LLM: {e}")


#     # ---------------- VALIDATION ----------------
#     required_keys = [
#         "bank_identification",
#         "header_markers",
#         "footer_markers",
#         "metadata_keywords",
#         "table_structure",
#         "transaction_anchor",
#         "amount_pattern"
#     ]

#     missing = [k for k in required_keys if k not in identifier_json]
#     if missing:
#         raise ValueError(f"LLM returned incomplete schema. Missing: {missing}")

#     return identifier_json



# # ------------------- SAVE NEW FORMAT -------------------
# def save_new_statement_format(
#     statement_type,
#     format_name,
#     bank_name,
#     bank_code,
#     identifier_json,
#     extraction_logic_json,
#     threshold=65.0
# ):
#     return insert_statement_category(
#         statement_type=statement_type,
#         format_name=format_name,
#         institution_name=bank_name,
#         ifsc_code=bank_code,
#         identifier_json=identifier_json,
#         extraction_logic_json=extraction_logic_json,
#         threshold=threshold
#     )





# ============================================================
# ================== IMPORTS ==================
# ============================================================

import re
import json
import os
from typing import Dict, List, Tuple
from groq import Groq
from repository.statement_category_repo import (
    get_under_review_formats,
    insert_statement_category
)

# ============================================================
# ================== CONFIG ==================
# ============================================================

import google.generativeai as genai

GEMINI_API_KEY = os.getenv("gemini_key")
genai.configure(api_key=GEMINI_API_KEY)

MODEL_NAME = "models/gemini-3-flash-preview"

# ============================================================
# ================== TEXT REDUCTION (COLAB EXACT) ============
# ============================================================

def reduce_text(pages: List[str]) -> Dict:
    return {
        "first_page": pages[0][:6000] if pages else "",
        "last_page": pages[-1][-3000:] if pages else "",
        "headers": [
            line.strip()
            for p in pages
            for line in p.splitlines()
            if re.search(r"\b(Date|Debit|Credit|Balance|Amount)\b", line, re.I)
        ][:10]
    }

# ============================================================
# ================== UNIVERSAL MATCH ENGINE (COLAB EXACT) ====
# ============================================================

def evaluate_identity_markers(identity: Dict, text: str):

    text_norm = re.sub(r"\s+", " ", text.lower())

    total_score = 0
    total_max = 0

    def process_rule(rule_obj, key_path, weight=5):

        nonlocal total_score, total_max

        if not rule_obj:
            return

        if isinstance(rule_obj, list) and rule_obj:
            total_max += weight
            matched = any(isinstance(k, str) and k.lower() in text_norm for k in rule_obj)
            if matched:
                total_score += weight
            return

        if not isinstance(rule_obj, dict):
            return

        rule_type = rule_obj.get("rule")

        if rule_type == "keyword":
            patterns = rule_obj.get("patterns", [])
            if not patterns:
                return

            total_max += weight
            matched = any(p.lower() in text_norm for p in patterns)
            if matched:
                total_score += weight

        elif rule_type == "regex":
            pattern = rule_obj.get("pattern")
            if not pattern:
                return

            total_max += weight
            matched = bool(re.search(pattern, text, re.I))
            if matched:
                total_score += weight

    # Traverse structure
    for section_name, section in identity.items():
        if not isinstance(section, dict):
           continue

        for field_name, field_value in section.items():
            key_path = f"{section_name}.{field_name}"
            if isinstance(field_value, dict) and "rule" not in field_value:
                for sub_name, sub_value in field_value.items():
                    process_rule(sub_value, f"{key_path}.{sub_name}")
            else:
                process_rule(field_value, key_path)

    # TABLE HEADER FUZZY (HIGH WEIGHT)
    table = identity.get("transaction_table_identity", {})
    headers = table.get("table_header_markers", [])
    min_required = table.get("minimum_column_count", 0)

    if headers:
        weight = 25
        total_max += weight

        text_compact = re.sub(r"\s+", "", text.lower())

        matched_count = sum(
            1 for h in headers
            if re.sub(r"\s+", "", h.lower()) in text_compact
        )

        if matched_count >= min_required:
            total_score += (matched_count / len(headers)) * weight

    # FOOTER
    footer = identity.get("footer_identity", {})
    footer_patterns = footer.get("footer_markers", [])

    if footer_patterns:
        weight = 5
        total_max += weight
        matched = any(
            re.search(p, text, re.I) if "\\" in p else p.lower() in text_norm
            for p in footer_patterns
        )
        if matched:
            total_score += weight

    confidence = round((total_score / total_max) * 100, 2) if total_max else 0
    return confidence

# ============================================================
# ================== DB MATCH (REPLACES REGISTRY) ============
# ============================================================

def find_existing_identifier(text: str, threshold: float = 80.0):

    best = {"identifier": None, "score": 0}
    categories = get_under_review_formats()

    for cat in categories:

        identifier_json = cat.get("statement_identifier")

        if isinstance(identifier_json, str):
            identifier_json = json.loads(identifier_json)

        identity = identifier_json.get("identity_markers", {})

        score = evaluate_identity_markers(identity, text)

        if score > best["score"]:
            best = {
                "identifier": identifier_json,
                "score": score
            }

    if best["score"] >= threshold:
        return True, best["identifier"]

    return False, None

# ============================================================
# ================== LLM CLASSIFICATION (COLAB EXACT) ========
# ============================================================

def classify_document_llm(reduced: Dict):

    prompt = """
You are a Senior Financial Document Classification Engine
and Universal Financial Identity Extraction System.

Your task has TWO PARTS:

PART 1 → Classify the financial document
PART 2 → Extract structural identity markers

Use ONLY the provided text.
Do NOT guess.
If insufficient evidence → classify as UNKNOWN_FINANCIAL_DOCUMENT.

============================================================
PART 1 — DOCUMENT CLASSIFICATION
============================================================

------------------------------------------------------------
LEVEL 1 — DOCUMENT FAMILY (choose EXACTLY ONE)
------------------------------------------------------------

1. BANK_ACCOUNT_STATEMENT
2. CREDIT_CARD_STATEMENT
3. LOAN_STATEMENT
4. OVERDRAFT_CASH_CREDIT_STATEMENT
5. WALLET_STATEMENT
6. PAYMENT_GATEWAY_SETTLEMENT
7. INVESTMENT_STATEMENT
8. DEMAT_STATEMENT
9. TAX_LEDGER_STATEMENT
10. UNKNOWN_FINANCIAL_DOCUMENT

------------------------1 LOAN_STATEMENT--------------------
If ANY appear → MUST classify as LOAN_STATEMENT:
 
• Loan Account Number
• Loan A/c No
• EMI
• Equated Monthly Installment
• Principal Outstanding
• Principal Repaid
• Interest Charged (loan context)
• Interest Rate
• Sanction Amount
• Disbursement
• Repayment Schedule
• Installment Due Date
• Overdue Amount
• Total Loan Outstanding
• Amortization Schedule
 
If EMI OR Principal Outstanding exists → STOP → LOAN_STATEMENT
 
Do NOT classify as BANK if loan markers exist.
---------------------2 CREDIT_CARD_STATEMENT----------------
If ANY appear → CREDIT_CARD_STATEMENT:
 
• Masked card number (XXXX-XXXX-1234)
• Minimum Amount Due
• Total Amount Due
• Payment Due Date
• Credit Limit
• Available Credit
• Revolving Credit
• Finance Charges
• Cash Advance Limit
 
Credit card always has:
• Statement Date
• Due Date
• Total Due
 
If those 3 exist together → CREDIT_CARD_STATEMENT
----------------------3 DEMAT_STATEMENT---------------------
If ANY appear → DEMAT_STATEMENT:
 
• DP ID
• BO ID
• Beneficial Owner
• Depository Participant
• NSDL
• CDSL
• ISIN Code
• Transaction cum Holding Statement
• Holding Statement
• Pledge Balance
• Free Balance (securities context)
• Lock-in securities
 
If ISIN column exists → STOP → DEMAT_STATEMENT
If DP ID + ISIN both exist → STOP → DEMAT_STATEMENT
-----------------4 OVERDRAFT_CASH_CREDIT_STATEMENT----------
If ANY appear:
 
• Drawing Power
• DP
• Sanctioned Limit
• CC Limit
• OD Limit
• Limit Utilized
• Available Limit
• Cash Credit Account
• CC Account
• OD Account
• Margin Requirement
• Hypothecation
• Stock Statement
• Interest on CC
 
AND running balance ledger exists
 
→ MUST classify as OVERDRAFT_CASH_CREDIT_STATEMENT
 
Do NOT classify as BANK if limit structure present.
-------------------5 TAX_LEDGER_STATEMENT-------------------
If ANY appear → TAX_LEDGER_STATEMENT:
 
• GSTIN
• Electronic Cash Ledger
• Electronic Credit Ledger
• Electronic Liability Register
• Input Tax Credit
• Output Tax
• CGST / SGST / IGST
• GSTR
• Tax Period
• Challan Identification Number (CIN in GST context)
 
If GSTIN + Tax Period + CGST/SGST present → TAX_LEDGER_STATEMENT
-------------------6 PAYMENT_GATEWAY_SETTLEMENT-------------------
If ANY appear → PAYMENT_GATEWAY_SETTLEMENT:
 
• Merchant Settlement Report
• Settlement ID
• Settlement Date
• MDR (Merchant Discount Rate)
• Commission
• Net Settlement
• Gross Collection
• Gateway Charges
• Payment Processor (Razorpay, PayU, Stripe, etc.)
 
If settlement batch structure exists → classify here.
-----------------7 INVESTMENT_STATEMENT---------------------
If ANY appear → INVESTMENT_STATEMENT:
 
• Portfolio Summary
• Asset Allocation
• NAV
• Units Held
• Fund Name
• Mutual Fund
• Equity Portfolio
• SIP
• Market Value (portfolio context)
 
If NAV + Units Held appear → INVESTMENT_STATEMENT
 
Do NOT confuse with DEMAT:
DEMAT has ISIN + DP ID
Investment has NAV + Units
------------------------------------------------------------
LEVEL 2 — DOCUMENT SUBTYPE
------------------------------------------------------------

Subtype MUST belong to selected Level 1.
If UNKNOWN_FINANCIAL_DOCUMENT → subtype must be UNKNOWN.

============================================================
PART 2 — UNIVERSAL IDENTITY MARKER EXTRACTION
============================================================

You must extract identity markers that uniquely define
this specific document structure.

These identity categories apply to ALL document families.

If a field does not exist in the document → return null or empty array.

------------------------------------------------------------
IDENTITY CATEGORIES (FOR ALL DOCUMENT TYPES)
------------------------------------------------------------

1️⃣ ISSUER / INSTITUTION IDENTITY
- issuer_name
- brand keywords
- regulatory identifiers regex MUST always be included
  (IFSC, SWIFT, IBAN, GSTIN, CIN, SEBI, RBI, NSDL, CDSL, etc.)
- merchant_id (if applicable)
- dp_id / bo_id (if applicable)

2️⃣ DOCUMENT STRUCTURE IDENTITY
- document_title_phrase
- document_type_keywords
- document_reference_number pattern
- report_generation_phrase

3️⃣ PERIOD / DATE IDENTITY
- statement_period pattern
- statement_date pattern
- billing_cycle pattern
- tax_period pattern

4️⃣ ACCOUNT / ENTITY IDENTITY
(Adapt dynamically based on document type)

Possible patterns include:
- account_number
- masked_card_number
- loan_account_number
- customer_id
- wallet_id
- merchant_id
- gstin
- pan
- bo_id
- dp_id

Use regex where structured.
Use keyword rules for static labels.

5️⃣ TRANSACTION TABLE IDENTITY
- exact header markers found
- minimum_column_count
- presence_of_running_balance (true/false)
- debit_credit_style (true/false)

6️⃣ FINANCIAL SUMMARY IDENTITY
- total_outstanding pattern
- minimum_due pattern
- emi_amount pattern
- credit_limit pattern
- drawing_power pattern
- portfolio_value pattern
- total_tax pattern

Only include patterns actually visible.

7️⃣ FOOTER / STATIC MARKER IDENTITY
- disclaimer phrases
- “system generated” markers
- page number format
- static regulatory lines

============================================================
INSTITUTION EXTRACTION RULES
============================================================

Extract exact issuing institution name from text.
If multiple entities → return issuing authority.
If not found → UNKNOWN.

============================================================
COUNTRY DETECTION
============================================================

Use:
- Currency symbol
- Regulatory identifiers
- IFSC → India
- IBAN → EU
- GSTIN → India
- Address format

If unclear → UNKNOWN.

============================================================
CONFIDENCE SCORING
============================================================

0.90 – 1.00 → Strong structural evidence
0.70 – 0.89 → Strong keyword evidence
0.50 – 0.69 → Moderate evidence
Below 0.50 → Weak

============================================================
DOCUMENT TEXT SAMPLE
============================================================

FIRST PAGE:
""" + reduced["first_page"] + """

LAST PAGE:
""" + reduced["last_page"] + """

TABLE HEADERS:
""" + json.dumps(reduced["headers"]) + """

OTHER TEXT:
""" + reduced.get("other_sample", "") + """

============================================================

STRICT RULES:
- No guessing
- No explanation
- No markdown
- Return STRICT valid JSON only
- Use null where field not present
- Subtype must match Level 1
- If unsure → UNKNOWN_FINANCIAL_DOCUMENT

STATEMENT VERSIONING:
- ID format: [document_family]_[document_subtype]_[VERSION]

============================================================
INSTRUCTIONS
============================================================

1. Identify institution, document type, and structural markers
2. Extract deterministic identity signals ONLY
3. Do NOT infer missing data
4. Prefer regex rules for structured fields
5. Use keyword rules for static phrases
6. Output MUST follow the exact format below
7. Do NOT include explanations
8. Do NOT include markdown
9. Return Python-style literals (None, True, False)

============================================================
REQUIRED OUTPUT FORMAT (STRICT)
============================================================

{
  "id": "UNIQUE_ID_V1",
  "document_family": "",
  "document_subtype": "",
  "institution_name": "",
  "country": "",
  "confidence_score": 0.0,

  "identity_markers": {

    "issuer_identity": {
      "issuer_name": { "rule": "keyword", "patterns": [] },
      "regulatory_identifiers": {
        "ifsc": { "rule": "regex", "pattern": "..." },
        "swift": { "rule": "regex", "pattern": "..." },
        "iban": { "rule": "regex", "pattern": "..." },
        "gstin": { "rule": "regex", "pattern": "..." },
        "other": []
      }
    },

    "document_structure_identity": {
      "document_title_phrase": { "rule": "keyword", "patterns": [] },
      "document_reference_number": { "rule": "regex", "pattern": None },
      "generation_phrase": { "rule": "keyword", "patterns": [] }
    },

    "period_identity": {
      "statement_period": { "rule": "regex", "pattern": None },
      "statement_date": { "rule": "regex", "pattern": None },
      "billing_cycle": { "rule": "regex", "pattern": None },
      "tax_period": { "rule": "regex", "pattern": None }
    },

    "entity_identity": {
      "account_number": { "rule": "regex", "pattern": None },
      "masked_card_number": { "rule": "regex", "pattern": None },
      "loan_account_number": { "rule": "regex", "pattern": None },
      "customer_id": { "rule": "regex", "pattern": None },
      "wallet_id": { "rule": "regex", "pattern": None },
      "merchant_id": { "rule": "regex", "pattern": None },
      "pan": { "rule": "regex", "pattern": None },
      "bo_id": { "rule": "regex", "pattern": None },
      "dp_id": { "rule": "regex", "pattern": None }
    },

    "transaction_table_identity": {
      "table_header_markers": ["Exact", "Column", "Names"],
      "minimum_column_count": 0,
      "presence_of_running_balance": False,
      "debit_credit_style": False
    },

    "financial_summary_identity": {
      "total_outstanding": { "rule": "regex", "pattern": None },
      "minimum_due": { "rule": "regex", "pattern": None },
      "emi_amount": { "rule": "regex", "pattern": None },
      "credit_limit": { "rule": "regex", "pattern": None },
      "drawing_power": { "rule": "regex", "pattern": None },
      "portfolio_value": { "rule": "regex", "pattern": None },
      "total_tax": { "rule": "regex", "pattern": None }
    },

    "footer_identity": {
      "footer_markers": []
    }
  }
}

============================================================
OUTPUT RULES
============================================================

• Return ONLY the object above
• Do NOT wrap in markdown
• Do NOT add explanations
• Use Python literals: None, True, False
"""

    model = genai.GenerativeModel(MODEL_NAME)

    response = model.generate_content(
        prompt,
        generation_config={
        "temperature": 0,
        "response_mime_type": "application/json"
    })

    raw_output = response.text.strip()

    # Normalize rare uppercase NULL
    raw_output = re.sub(r'\bNULL\b', 'null', raw_output)

    identifier = json.loads(raw_output)

    return identifier

def derive_bank_code_from_identifier(identifier_json):
    """
    Extract first 4 characters from IFSC pattern if available.
    Returns None if IFSC not found or invalid.
    """

    try:
        ifsc_obj = (
            identifier_json
            .get("identity_markers", {})
            .get("issuer_identity", {})
            .get("regulatory_identifiers", {})
            .get("ifsc")
        )

        if not ifsc_obj:
            return None

        pattern = ifsc_obj.get("pattern")

        if not pattern:
            return None

        # If Gemini returned actual IFSC like HDFC0000501
        match = re.match(r"^([A-Z]{4})", pattern)
        if match:
            return match.group(1)

        return None

    except Exception:
        return None

# ============================================================
# ================== SAVE FORMAT (UNCHANGED) =================
# ============================================================

# def save_new_statement_format(
#     statement_type,
#     format_name,
#     bank_name,
#     bank_code,
#     identifier_json,
#     extraction_logic_json,
#     threshold=65.0
# ):
#     return insert_statement_category(
#         statement_type=statement_type,
#         format_name=format_name,
#         institution_name=bank_name,
#         ifsc_code=bank_code,
#         identifier_json=identifier_json,
#         extraction_logic_json=extraction_logic_json,
#         threshold=threshold
#     )
def save_new_statement_format(
    format_name,
    bank_code,
    identifier_json,
    threshold=65.0
):
    """
    Save only identifier JSON.
    statement_type and institution_name are auto-derived.
    """

    statement_type = identifier_json.get("document_family")
    institution_name = identifier_json.get("institution_name")
    bank_code = derive_bank_code_from_identifier(identifier_json)
    return insert_statement_category(
        statement_type=statement_type,
        format_name=format_name,
        institution_name=institution_name,
        ifsc_code=bank_code,
        identifier_json=identifier_json,
        threshold=threshold
    )