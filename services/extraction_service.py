# Later, you can implement:
# - generate_extraction_logic_llm()
# - execute_extraction_logic()
# - compare_with_llm_extraction()
# - calculate confidence + HITL flag
# import json
# import os
# import re
# from groq import Groq

# client = Groq(api_key=os.getenv("GROQ_API_KEY"))
# MODEL_NAME = "llama-3.1-8b-instant"
# import json

# def generate_extraction_logic_llm(identifier_json: dict) -> str:
#     """
#     Generates Python transaction extraction function
#     based on identification markers.
#     Returns raw Python source code as string.
#     """

#     prompt = f"""
# Act as a Senior Python Data Engineer specializing in financial document parsing.

# You MUST generate extraction logic using ONLY the provided configuration variables.

# ============================================================
# CONFIG VARIABLES (ALREADY AVAILABLE IN RUNTIME)
# ============================================================

# DATE_PATTERN = {json.dumps(identifier_json.get("transaction_anchor", {}).get("date_pattern", ""))}

# MONEY_REGEX = {json.dumps(identifier_json.get("amount_pattern", ""))}

# METADATA_KEYWORDS = {json.dumps(identifier_json.get("metadata_keywords", []))}

# FOOTER_PATTERNS = {json.dumps(identifier_json.get("footer_markers", {}).get("patterns", []))}

# COLUMN_HEADERS = {json.dumps(identifier_json.get("table_structure", {}).get("column_headers", []))}

# ============================================================
# IMPORTANT RULES
# ============================================================

# • DO NOT redefine these variables.
# • DO NOT reference IDENTIFIER_SCHEMA.
# • Use ONLY the variables provided above.
# • Handle missing or empty values safely.
# • Do NOT invent patterns.

# ============================================================
# Now generate the function:
# ============================================================

# Write:

#     def extract_transactions(text: str) -> List[Dict]:

# Requirements:

# 1️⃣ PREPROCESSING
# - Replace \\u00A0
# - Normalize line endings
# - Strip trailing spaces

# 2️⃣ DATE ANCHOR
# - Build:
#     DATE_ANCHOR_REGEX = rf'^\\s*(?:\\d+\\s+)?({{DATE_PATTERN}})'
# - Must anchor with ^
# - Must not match mid sentence

# 3️⃣ MONEY EXTRACTION
# - Use re.findall(MONEY_REGEX, line)
# - Last value = balance
# - 2 values → amount + balance
# - 3 values → debit/credit + balance
# - Remove commas before float conversion

# 4️⃣ NOISE FILTER
# - Skip line if contains any METADATA_KEYWORDS
# - Skip column header lines
# - Stop parsing if line contains FOOTER_PATTERNS
# - Stop if line starts with "Account Summary"

# 5️⃣ STATE MACHINE
# Maintain:
#     transactions = []
#     current_txn = None
#     previous_balance = None

# Use delta logic:
#     delta = current_balance - previous_balance
# If delta > 0 → credit
# If delta < 0 → debit

# Confidence:
# 1.0 exact
# 0.8 derived
# 0.5 weak

# 6️⃣ RETURN
# Return List[Dict] with:
#     date
#     details
#     debit
#     credit
#     balance
#     confidence

# Constraints:
# • Use only re
# • No external libraries
# • No explanations
# • Output ONLY Python code
# """

#     response = client.chat.completions.create(
#         model=MODEL_NAME,
#         messages=[{"role": "user", "content": prompt}],
#         temperature=0
#     )

#     content = response.choices[0].message.content

#     if content is None:
#         raise ValueError("LLM returned None extraction code.")

#     raw_output = content.strip()

#     # Remove markdown fences safely
#     if "```" in raw_output:
#         parts = raw_output.split("```")
#         raw_output = parts[1] if len(parts) > 1 else parts[0]

#     raw_output = raw_output.strip()

#     if raw_output.lower().startswith("python"):
#         raw_output = raw_output[6:].strip()

#     return raw_output
# import re
# from typing import List, Dict


# def extract_transactions_using_logic(
#     full_text: str,
#     identifier_json: dict,
#     extraction_code: str
# ) -> List[Dict]:
#     """
#     Executes LLM-generated extraction function safely.
#     Injects required variables deterministically.
#     """

#     try:
#         # ---------------------------------------------------
#         # 1️⃣ Clean LLM Output
#         # ---------------------------------------------------
#         cleaned_code = extraction_code.strip()

#         if "```" in cleaned_code:
#             parts = cleaned_code.split("```")
#             cleaned_code = parts[1] if len(parts) > 1 else parts[0]

#         cleaned_code = cleaned_code.strip()

#         if cleaned_code.lower().startswith("python"):
#             cleaned_code = cleaned_code[6:].strip()

#         # ---------------------------------------------------
#         # 2️⃣ Inject Deterministic Runtime Variables
#         # ---------------------------------------------------
#         execution_namespace = {
#             "re": re,
#             "List": List,
#             "Dict": Dict,

#             # 🔥 Inject required config directly
#             "DATE_PATTERN": identifier_json.get("transaction_anchor", {}).get("date_pattern", ""),
#             "MONEY_REGEX": identifier_json.get("amount_pattern", ""),
#             "METADATA_KEYWORDS": identifier_json.get("metadata_keywords", []),
#             "FOOTER_PATTERNS": identifier_json.get("footer_markers", {}).get("patterns", []),
#             "COLUMN_HEADERS": identifier_json.get("table_structure", {}).get("column_headers", []),
#         }

#         # ---------------------------------------------------
#         # 3️⃣ Execute Generated Code
#         # ---------------------------------------------------
#         exec(cleaned_code, execution_namespace)

#         if "extract_transactions" not in execution_namespace:
#             raise ValueError("extract_transactions function not found.")

#         extract_fn = execution_namespace["extract_transactions"]

#         # ---------------------------------------------------
#         # 4️⃣ Execute Extraction
#         # ---------------------------------------------------
#         transactions = extract_fn(full_text)

#         if not isinstance(transactions, list):
#             raise ValueError("extract_transactions must return List[Dict].")

#         validated_transactions = []
#         for txn in transactions:
#             if isinstance(txn, dict):
#               validated_transactions.append(txn)

#         if not validated_transactions:
#             raise ValueError("No valid transaction dictionaries returned.")
#         transactions = validated_transactions

#         print(f"\nExtracted {len(transactions)} transactions.")
#         for i, txn in enumerate(transactions[:10], 1):
#             print(f"{i}: {txn}")
#         return transactions

#     except Exception as e:
#         raise RuntimeError(f"LLM extraction execution failed: {str(e)}")

# def extract_transactions_using_llm(pdf_text):
#     """Run LLM on PDF text to extract transactions"""
#     pass

# def compare_extractions(logic_result, llm_result) -> float:
#     """Return score to decide deployable / HITL"""
#     pass
import os
import re
from typing import List, Dict
from groq import Groq

# ---------------- LLM CONFIG ----------------
client = Groq(api_key=os.getenv("GROQ_API_KEY"))
MODEL_NAME = "llama-3.1-8b-instant"


# ---------------------------------------------------
# GENERATE UNIVERSAL EXTRACTION CODE
# ---------------------------------------------------
def generate_extraction_logic_llm(identifier_json: dict,headers, sample_text, footer) -> str:
    """
    Generates strict universal bank statement extraction code.
    Does NOT depend on identifier_json.
    """

    prompt = f"""
You are a Senior Python Backend Engineer.

You must generate a COMPLETE and VALID Python function:

    def extract_transactions(text: str) -> List[Dict]:

The function must be syntactically correct.
The function must not raise exceptions.
The function must return List[Dict].

If code is invalid, the answer is wrong.

------------------------------------------------------------
STRICT REGEX DEFINITIONS (COPY EXACTLY — DO NOT MODIFY)
------------------------------------------------------------

You MUST copy these two patterns EXACTLY as written.
Do NOT modify, simplify, or rewrite them.

DATE_ANCHOR_REGEX = r'^\\s*(\\d{{1,2}}[ \\/\\-](?:\\d{{1,2}}|[A-Za-z]{{3}})[ \\/\\-]\\d{{2,4}}|\\d{{4}}-\\d{{2}}-\\d{{2}})'

MONEY_REGEX = r'(\\d+(?:,\\d{{2}})*(?:,\\d{{3}})*\\.\\d{{2}})'

Do NOT change parentheses.
Do NOT change quantifiers.
Do NOT change escaping.
Do NOT redefine these patterns later.

------------------------------------------------------------
PREPROCESSING (MANDATORY)
------------------------------------------------------------

At start of function:

    text = text.replace("\\u00A0", " ")
    lines = [line.rstrip() for line in text.splitlines()]

Do NOT remove internal spaces.

------------------------------------------------------------
TERMINATION KEYWORDS (COPY EXACTLY)
------------------------------------------------------------

termination_keywords = [
    "GRAND TOTAL",
    "*** END OF STATEMENT ***",
    "END OF STATEMENT",
    "ABBREVIATIONS USED",
    "DISCLAIMER",
    "SUMMARY",
    "STATEMENT SUMMARY",
    "NOTE:"
]

------------------------------------------------------------
NOISE FILTERING
------------------------------------------------------------

Skip lines containing:

["Customer ID","Account Number","IFSC","MICR",
 "Joint Holders","Branch","Statement From",
 "Nomination","Currency","Page ",
 "Digitally signed","Generated on"]

Also skip header labels:

["Txn Date","Narration","Withdrawals",
 "Deposits","Closing Balance"]

Also skip lines containing:
"Opening Balance"
"Balance as on"
"B/F"

------------------------------------------------------------
STATE MACHINE STRUCTURE (MANDATORY — FOLLOW EXACTLY)
------------------------------------------------------------

Structure MUST be:

    transactions = []
    previous_balance = None
    current_txn = None

    for line in lines:

        stripped = line.strip()

        # termination
        if stripped.upper() in termination_keywords:
            break

        # skip noise
        if any(keyword.lower() in stripped.lower() for keyword in noise_list):
            continue

        # detect date anchor
        match = re.match(DATE_ANCHOR_REGEX, stripped)

        if match:

            # initialize inside this block
            date = match.group(1)
            After extracting date, validate it:
               If the original line starts with two digits (line[0:2].isdigit())
               AND extracted date starts with only one digit,
               then rebuild date as stripped[0:10] and validate again.
            details = ""
            debit = None
            credit = None
            balance = None
            confidence = 0.5
            
            amounts = re.findall(MONEY_REGEX, stripped)
            After extracting date and repairing it:
              Extract amounts using MONEY_REGEX.
              If no money values are found in the line:
                    continue (skip this transaction completely)
            if amounts:
                numeric_amounts = [float(a.replace(",", "")) for a in amounts]

                balance = numeric_amounts[-1]

                if len(numeric_amounts) >= 2:
                    txn_amount = numeric_amounts[-2]

                    if "dr" in stripped.lower():
                        debit = txn_amount
                    elif "cr" in stripped.lower():
                        credit = txn_amount

            # math repair only if previous_balance exists
            if previous_balance is not None and balance is not None:
                delta = balance - previous_balance

                if debit is None and credit is None:
                    if delta > 0:
                        credit = abs(delta)
                        confidence = 0.8
                    elif delta < 0:
                        debit = abs(delta)
                        confidence = 0.8

                if debit is not None or credit is not None:
                    confidence = 1.0

            if balance is not None:
                previous_balance = balance

            # details cleanup
            details = re.sub(DATE_ANCHOR_REGEX, "", stripped)
            details = re.sub(MONEY_REGEX, "", details)
            details = details.replace("Dr","").replace("Cr","")
            details = details.strip()
            transactions.append({{
                "date": date,
                "details": details,
                "debit": debit,
                "credit": credit,
                "balance": balance,
                "confidence": confidence
            }})

    return transactions

------------------------------------------------------------
CRITICAL RULES
------------------------------------------------------------

• Do NOT redefine DATE_ANCHOR_REGEX
• Do NOT redefine MONEY_REGEX
• Do NOT iterate over re.search()
• Do NOT use variables before initializing them
• Do NOT return anything except List[Dict]
• Use only the re library
• Provide ONLY Python code
• Do NOT include markdown
"""


    response = client.chat.completions.create(
        model=MODEL_NAME,
        messages=[{"role": "user", "content": prompt}],
        temperature=0
    )

    content = response.choices[0].message.content
    if content is None:
        raise ValueError("LLM returned empty extraction code.")

    raw_output = content.strip()

    # Remove markdown fences safely
    if "```" in raw_output:
        parts = raw_output.split("```")
        raw_output = parts[1] if len(parts) > 1 else parts[0]

    raw_output = raw_output.strip()

    if raw_output.lower().startswith("python"):
        raw_output = raw_output[6:].strip()

    return raw_output


# ---------------------------------------------------
# EXECUTE GENERATED EXTRACTION CODE
# ---------------------------------------------------
def extract_transactions_using_logic(
    full_text: str,
    extraction_code: str
) -> List[Dict]:

    try:
        cleaned_code = extraction_code.strip()

        if "```" in cleaned_code:
            parts = cleaned_code.split("```")
            cleaned_code = parts[1] if len(parts) > 1 else parts[0]

        cleaned_code = cleaned_code.strip()

        execution_namespace = {
            "re": re,
            "List": List,
            "Dict": Dict
        }

        exec(cleaned_code, execution_namespace)

        if "extract_transactions" not in execution_namespace:
            raise ValueError("extract_transactions function not found.")

        extract_fn = execution_namespace["extract_transactions"]

        transactions = extract_fn(full_text)

        if not isinstance(transactions, list):
            raise ValueError("Extraction must return List[Dict].")

        print(f"\nExtracted {len(transactions)} transactions.")
        for i, txn in enumerate(transactions[:10], 1):
            print(f"{i}: {txn}")
        return transactions

    except Exception as e:
        raise RuntimeError(f"LLM extraction execution failed: {str(e)}")
    
