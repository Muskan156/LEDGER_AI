import re
import json
from repository.statement_category_repo import get_active_statement_categories, insert_statement_category
from services.pdf_service import extract_pages
from typing import Dict, Tuple, List
from groq import Groq
import os
import json

# ------------------- CONFIGURE LLM -------------------
client = Groq(api_key=os.getenv("GROQ_API_KEY"))

# ---------------------------------------------------
# BANK CODE → BANK NAME MAPPING
# ---------------------------------------------------
BANK_CODE_MAP = {
    "SBIN": "State Bank of India",
    "HDFC": "HDFC Bank",
    "ICIC": "ICICI Bank",
    "BKID": "Bank of India",
    "PUNB": "Punjab National Bank",
    "BARB": "Bank of Baroda",
}
# ---------------------------------------------------
# DERIVE BANK NAME FROM IFSC
# ---------------------------------------------------
def derive_bank_name_from_ifsc(text: str):
    match = re.search(r"\b([A-Z]{4})0[A-Z0-9]{6}\b", text)
    if not match:
        return None, None

    bank_code = match.group(1)
    bank_name = BANK_CODE_MAP.get(bank_code)

    return bank_name, bank_code
# # ------------------- RULE HELPERS -------------------
def regex_match(text: str, pattern: str) -> bool:
    return bool(re.search(pattern, text, re.I | re.S))

def keyword_presence(text: str, patterns: List[str]) -> bool:
    t = text.lower()
    return any(p.lower() in t for p in patterns)

def apply_rule(text: str, rule: Dict) -> bool:
    if rule["rule"] == "regex":
        return regex_match(text, rule["pattern"])
    if rule["rule"] == "keyword":
        return keyword_presence(text, rule["patterns"])
    return False

def table_header_match(text: str, headers: List[str]) -> Tuple[int, int]:
    t = re.sub(r"\s+", "", text.lower())
    matched = 0
    for h in headers:
        h_norm = re.sub(r"\s+", "", h.lower())
        if h_norm in t:
            matched += 1
    return matched, len(headers)

# ------------------- IDENTIFIER EVALUATION -------------------
def evaluate_identifier(identifier: Dict, text: str) -> Tuple[float, Dict]:
    score = 0
    max_score = 0
    details = {}

    sim = identifier["statement_identifier"]

    # ---------------- BANK ----------------
    bank_patterns = (
        sim.get("bank_identification", {})
           .get("bank_name", {})
           .get("patterns", [])
    )

    if bank_patterns:
        max_score += 20
        matched = any(p.lower() in text.lower() for p in bank_patterns)
        if matched:
            score += 20
        details["bank"] = matched

    # ---------------- HEADER ----------------
    headers = sim.get("header_markers", {}).get("patterns", [])
    if headers:
        max_score += 20
        matched = any(h.lower() in text.lower() for h in headers)
        if matched:
            score += 20
        details["header"] = matched

    # ---------------- FOOTER ----------------
    footers = sim.get("footer_markers", {}).get("patterns", [])
    if footers:
        max_score += 15
        matched = any(f.lower() in text.lower() for f in footers)
        if matched:
            score += 15
        details["footer"] = matched

    # ---------------- TABLE HEADERS ----------------
    table_headers = (
        sim.get("table_structure", {})
           .get("column_headers", [])
    )

    if table_headers:
        max_score += 25
        matched_count = sum(
            1 for h in table_headers if h.lower() in text.lower()
        )

        if matched_count >= 3:   # at least 3 columns matched
            score += 25

        details["table_match_count"] = matched_count

    # ---------------- DATE PATTERN ----------------
    date_pattern = (
        sim.get("transaction_anchor", {})
           .get("date_pattern", "")
    )

    if date_pattern:
        max_score += 10
        if re.search(date_pattern, text):
            score += 10
            details["date_pattern"] = True
        else:
            details["date_pattern"] = False

    # ---------------- AMOUNT PATTERN ----------------
    amount_pattern = sim.get("amount_pattern", "")
    if amount_pattern:
        max_score += 10
        if re.search(amount_pattern, text):
            score += 10
            details["amount_pattern"] = True
        else:
            details["amount_pattern"] = False

    confidence = round((score / max_score) * 100, 2) if max_score else 0

    return confidence, details
# ------------------- IDENTIFICATION -------------------
def identify_statement(text: str, threshold: float = 65.0) -> Tuple[bool, Dict]:
    best = {"id": None, "score": 0, "details": None}
    categories = get_active_statement_categories()  

    for cat in categories:
        score, details = evaluate_identifier(cat, text)
        if score > best["score"]:
            best = {
                "id": cat["statement_id"],
                "score": score,
                "details": details
            }

    if best["score"] >= threshold:
        return True, best
    return False, best

# ------------------- REDUCE TEXT FOR LLM -------------------
def reduce_text_for_llm(pages: List[str]) -> Dict:
    return {
        "first_page": pages[0][:3000] if pages else "",
        "last_page": pages[-1][-3000:] if pages else "",
        "table_headers": [
            line.strip()
            for p in pages
            for line in p.splitlines()
            if re.search(r"\b(Date|Debit|Credit|Balance|Withdrawals|Deposits)\b", line, re.I)
        ][:7]
    }
def generate_identifier_llm(reduced: Dict) -> Dict:
    prompt = f"""
You are a Senior Banking Document Format Architect.

Your task is to generate a reusable FORMAT IDENTIFIER SCHEMA
for a BANK STATEMENT.

This schema will later be used to automatically detect
and parse statements of this exact bank format.

============================================================
CRITICAL RULES (NON-NEGOTIABLE)
============================================================

1. DO NOT include:
   - Customer names
   - Addresses
   - Emails
   - Account numbers
   - CIF numbers
   - Phone numbers
   - Transaction values
   - Any numeric IDs
   - Any dynamic content

2. Extract ONLY structural constants that appear
   on ALL statements of this bank format.

3. If multiple tables exist:
   - IGNORE account summary tables
   - IGNORE metadata blocks
   - IGNORE relationship summaries
   - ONLY extract the MAIN TRANSACTION TABLE

4. DO NOT invent headers.
5. DO NOT invent regex.
6. If unsure, return empty list [].
7. Return STRICT JSON ONLY.
8. No markdown.
9. No explanation text.
10. No comments.
11. No trailing commas.
12. No extra keys.

============================================================
REQUIRED JSON STRUCTURE (EXACTLY THIS)
============================================================

{{
  "bank_identification": {{
    "bank_name": {{
      "patterns": []
    }}
  }},
  "header_markers": {{
    "patterns": []
  }},
  "footer_markers": {{
    "patterns": []
  }},
  "metadata_keywords": [],
  "table_structure": {{
    "column_headers": []
  }},
  "transaction_anchor": {{
    "date_pattern": ""
  }},
  "amount_pattern": ""
}}

============================================================
FIELD DEFINITIONS
============================================================

bank_identification.patterns:
  • ONLY static bank name text
  • Example: "State Bank of India"
  • NEVER include customer names

header_markers.patterns:
  • Static phrases that appear at the top of every statement
  • Examples:
        "STATEMENT OF ACCOUNT"
        "Account Statement"
        "Statement From"

footer_markers.patterns:
  • Static closing markers
  • Examples:
        "Page no."
        "*** End of Statement ***"

metadata_keywords:
  • Words indicating NON-TRANSACTION lines
  • Examples:
        "Opening Balance"
        "Closing Balance"
        "Summary"
        "Account Summary"

============================================================
STRICT TABLE HEADER EXTRACTION RULES
============================================================

You MUST extract transaction table headers ONLY from:

TABLE HEADERS DETECTED:
{json.dumps(reduced['table_headers'])}

Rules:

1. ONLY choose from the provided TABLE HEADERS DETECTED list.
2. DO NOT extract from first page paragraphs.
3. DO NOT include lines with ":".
4. DO NOT include summary blocks.
5. A valid transaction header MUST contain:
   - A date column
   - A debit/withdrawal column
   - A credit/deposit column
   - A balance column

If this condition fails:
    return "column_headers": []

============================================================
TRANSACTION DATE PATTERN (STRICT)
============================================================

transaction_anchor.date_pattern must match
ONLY transaction row dates.

It must support:

• 01/12/2025
• 1/2/2025
• 01-12-2025
• 01-12-25
• 14 Nov 2025
• 14-Jan-2026
• 2025-12-01

Return this EXACT robust pattern:

"\\d{{1,2}}[ \\/\\-](?:\\d{{1,2}}|[A-Za-z]{{3}})[ \\/\\-]\\d{{2,4}}|\\d{{4}}-\\d{{2}}-\\d{{2}}"

DO NOT return narrower patterns.

============================================================
AMOUNT PATTERN (LOCKED - DO NOT MODIFY)
============================================================

amount_pattern MUST be EXACTLY:

"\\d+(?:,\\d{{2}})*(?:,\\d{{3}})*\\.\\d{{2}}"

DO NOT generate your own money regex.

============================================================
DOCUMENT CONTENT
============================================================

FIRST PAGE (CLEANED):
{reduced['first_page']}

TABLE REGION SAMPLE:
{json.dumps(reduced['table_headers'])}

LAST PAGE:
{reduced['last_page']}

Remember:
Return STRICT JSON only.
No explanation.
No markdown.
"""

    response = client.chat.completions.create(
        model="llama-3.1-8b-instant",
        messages=[{"role": "user", "content": prompt}],
        temperature=0
    )

    raw_output = response.choices[0].message.content.strip()

    # ---------------- SAFE JSON EXTRACTION ----------------
    import re

    raw_output = raw_output.strip()

    # Remove markdown fences if present
    if "```" in raw_output:
       parts = raw_output.split("```")
       raw_output = parts[1] if len(parts) > 1 else parts[0]

    raw_output = raw_output.strip()

    # Extract JSON object safely
    start = raw_output.find("{")
    end = raw_output.rfind("}")

    if start == -1 or end == -1:
        print("Raw LLM output:\n", raw_output)
        raise ValueError("No JSON returned from LLM.")

    json_string = raw_output[start:end+1]

    # 🔥 FIX ESCAPE ISSUES
    json_string = json_string.replace("\\/", "/")
    json_string = json_string.replace("\\-", "-")

    try:
        identifier_json = json.loads(json_string)
    except Exception as e:
        print("Raw LLM output:\n", raw_output)
        print("Fixed JSON string:\n", json_string)
        raise ValueError(f"Invalid JSON returned from LLM: {e}")


    # ---------------- VALIDATION ----------------
    required_keys = [
        "bank_identification",
        "header_markers",
        "footer_markers",
        "metadata_keywords",
        "table_structure",
        "transaction_anchor",
        "amount_pattern"
    ]

    missing = [k for k in required_keys if k not in identifier_json]
    if missing:
        raise ValueError(f"LLM returned incomplete schema. Missing: {missing}")

    return identifier_json

# ------------------- SAVE NEW FORMAT -------------------
# def save_new_statement_format(statement_type, format_name, bank_name, identifier_json, extraction_logic_json, threshold=65.0):
#     return insert_statement_category(
#         statement_type,
#         format_name,
#         bank_name,
#         identifier_json,
#         extraction_logic_json,
#         threshold
#     )
def save_new_statement_format(
    statement_type,
    format_name,
    bank_name,
    bank_code,
    identifier_json,
    extraction_logic_json,
    threshold=65.0
):
    return insert_statement_category(
        statement_type=statement_type,
        format_name=format_name,
        institution_name=bank_name,
        ifsc_code=bank_code,   # storing only 4-letter bank code
        identifier_json=identifier_json,
        extraction_logic_json=extraction_logic_json,
        threshold=threshold
    )

