"""
services/validation_service.py
──────────────────────────────
STEP 5 — Validation Engine.

Compares CODE-generated transactions vs LLM-generated transactions.
Uses weighted bipartite matching to handle row-order differences.
"""

import json
import re
import logging
from datetime import datetime
from difflib import SequenceMatcher

logger = logging.getLogger("ledgerai.validation_service")


# # ── HELPERS ──────────────────────────────────────────────

# def normalize_date(date_str):
#     if not date_str:
#         return date_str
#     for fmt in ("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d %b %Y", "%d-%b-%Y"):
#         try:
#             return datetime.strptime(str(date_str).strip(), fmt).strftime("%Y-%m-%d")
#         except (ValueError, TypeError):
#             continue
#     return str(date_str).strip()


# def extract_json_from_response(response_text: str) -> list:
#     """Parse a JSON array from raw LLM response text."""
#     cleaned = response_text.replace("```json", "").replace("```", "").strip()
#     match = re.search(r"\[.*\]", cleaned, re.DOTALL)
#     if match:
#         try:
#             return json.loads(match.group(0))
#         except (json.JSONDecodeError, ValueError):
#             logger.warning("Failed to parse JSON from LLM response.")
#             return []
#     return []


# def calculate_similarity(a, b) -> float:
#     return SequenceMatcher(None, str(a).lower(), str(b).lower()).ratio()


# # ── PROPRIETY CHECK ──────────────────────────────────────

# NOISE_PATTERNS = [
#     r'page\s*[\d\s/]+of', r'statement\s*summary', r'opening\s*balance',
#     r'closing\s*balance', r'total\s*debit', r'total\s*credit',
#     r'generated\s*on', r'computer\s*generated', r'branch\s*:', 
#     r'ifsc', r'account\s*no', r'balance\s*as\s*on', r'carried\s*forward',
#     r'statement\s*of\s*account', r'customer\s*id', r'micr', r'rtgs',
#     r'date\s*particulars', r'debit\s*credit', r'value\s*date',
# ]

# def is_transaction_proper(txn: dict) -> bool:
#     """Check if a single transaction object looks valid and clean."""
#     if not txn:
#         return False
    
#     desc = str(txn.get("details") or "").strip()
#     if not desc or len(desc) < 3:
#         return False
        
#     # Check for header/footer noise in description
#     for pat in NOISE_PATTERNS:
#         if re.search(pat, desc, re.IGNORECASE):
#             return False
            
#     # Check for excessive delimiters or weird spacing balance
#     if desc.count('|') > 3 or desc.count('-') > 10:
#         return False
        
#     # Check date
#     date = txn.get("date")
#     if not date or date == "null" or date == "None":
#         return False
        
#     # Check amounts - at least one must be present
#     debit = txn.get("debit")
#     credit = txn.get("credit")
#     if debit is None and credit is None:
#         return False
        
#     return True

# def validate_extraction_propriety(txns: list) -> bool:
#     """Check if the entire set of transactions looks proper."""
#     if not txns:
#         return False
    
#     # If more than 20% of transactions are improper, the whole set is suspect
#     improper_count = sum(1 for t in txns if not is_transaction_proper(t))
#     if len(txns) > 0 and (improper_count / len(txns)) > 0.15:
#         return False
        
#     return True


# # ── VALIDATION ENGINE ────────────────────────────────────

# def validate_transactions(code_txns: list, llm_txns: list) -> dict:
#     """
#     Compare CODE vs LLM transactions using weighted bipartite matching.

#     Returns metrics dict with:
#       matched_transactions, date_accuracy, amount_accuracy,
#       balance_accuracy, description_accuracy, overall_accuracy,
#       transaction_count_match
#     """
#     if not code_txns or not llm_txns:
#         return None

#     matched_llm_indexes = set()
#     date_matches = 0
#     amount_matches = 0
#     balance_matches = 0
#     description_scores = []
#     total = 0

#     for code in code_txns:
#         code_date = normalize_date(code.get("date"))
#         code_amount = float(code.get("debit") or code.get("credit") or 0)
#         code_balance = float(code.get("balance") or 0)
#         code_desc = str(code.get("details") or "").strip()

#         best_match_index = None
#         best_match_score = 0

#         for idx, llm in enumerate(llm_txns):
#             if idx in matched_llm_indexes:
#                 continue

#             llm_date = normalize_date(llm.get("date"))
#             llm_amount = float(llm.get("debit") or llm.get("credit") or 0)
#             llm_balance = float(llm.get("balance") or 0)
#             llm_desc = str(llm.get("details") or "").strip()

#             score = 0

#             # Date match (strong weight)
#             if code_date and llm_date and code_date == llm_date:
#                 score += 3

#             # Amount match (strong weight)
#             if abs(code_amount - llm_amount) < 1:
#                 score += 3

#             # Balance match (medium weight)
#             if abs(code_balance - llm_balance) < 1:
#                 score += 2

#             # Description similarity (soft weight)
#             if calculate_similarity(code_desc, llm_desc) > 0.7:
#                 score += 2

#             if score > best_match_score:
#                 best_match_score = score
#                 best_match_index = idx

#         # Require minimum score to accept match
#         if best_match_index is not None and best_match_score >= 3:
#             matched_llm_indexes.add(best_match_index)
#             llm = llm_txns[best_match_index]
#             total += 1

#             if normalize_date(code.get("date")) == normalize_date(llm.get("date")):
#                 date_matches += 1
#             if abs(code_amount - float(llm.get("debit") or llm.get("credit") or 0)) < 1:
#                 amount_matches += 1
#             if abs(code_balance - float(llm.get("balance") or 0)) < 1:
#                 balance_matches += 1
#             description_scores.append(
#                 calculate_similarity(code_desc, llm.get("details", ""))
#             )

#     if total == 0:
#         return None

#     date_accuracy = (date_matches / total) * 100
#     amount_accuracy = (amount_matches / total) * 100
#     balance_accuracy = (balance_matches / total) * 100
#     description_accuracy = (sum(description_scores) / total) * 100

#     # Weighted final score
#     overall_accuracy = (
#         (date_accuracy * 0.30)
#         + (amount_accuracy * 0.30)
#         + (balance_accuracy * 0.25)
#         + (description_accuracy * 0.15)
#     )

#     return {
#         "matched_transactions": total,
#         "code_count": len(code_txns),
#         "llm_count": len(llm_txns),
#         "transaction_count_match": len(code_txns) == len(llm_txns),
#         "date_accuracy": round(date_accuracy, 2),
#         "amount_accuracy": round(amount_accuracy, 2),
#         "balance_accuracy": round(balance_accuracy, 2),
#         "description_accuracy": round(description_accuracy, 2),
#         "overall_accuracy": round(overall_accuracy, 2),
#     }
"""
services/validation_service.py
──────────────────────────────
STEP 5 — Validation Engine.
Updated with STRICT Quality Gate for User Display.
"""

import json
import re
import logging
from datetime import datetime
from difflib import SequenceMatcher

logger = logging.getLogger("ledgerai.validation_service")

# ── HELPERS ──────────────────────────────────────────────

def normalize_date(date_str):
    if not date_str:
        return None
    # Basic cleanup
    clean_str = str(date_str).strip()
    # Try common formats
    for fmt in ("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d %b %Y", "%d-%b-%Y", "%d-%b-%y"):
        try:
            return datetime.strptime(clean_str, fmt).strftime("%Y-%m-%d")
        except (ValueError, TypeError):
            continue
    return clean_str

def extract_json_from_response(response_text: str) -> list:
    cleaned = response_text.replace("```json", "").replace("```", "").strip()
    match = re.search(r"\[.*\]", cleaned, re.DOTALL)
    if match:
        try:
            return json.loads(match.group(0))
        except (json.JSONDecodeError, ValueError):
            return []
    return []

def calculate_similarity(a, b) -> float:
    return SequenceMatcher(None, str(a).lower(), str(b).lower()).ratio()

# ── PROPRIETY CHECK ──────────────────────────────────────

NOISE_PATTERNS = [
    r'page\s*[\d\s/]+of', r'statement\s*summary', r'opening\s*balance',
    r'closing\s*balance', r'total\s*debit', r'total\s*credit',
    r'generated\s*on', r'computer\s*generated', r'branch\s*:',
    r'ifsc', r'account\s*no', r'balance\s*as\s*on', r'carried\s*forward',
    r'statement\s*of\s*account', r'customer\s*id', r'micr', r'rtgs',
    r'date\s*particulars', r'debit\s*credit', r'value\s*date',
]


def is_transaction_proper(txn: dict) -> bool:
    """Check if a single transaction object looks valid and clean."""
    if not txn:
        return False

    desc = str(txn.get("details") or "").strip()
    if not desc or len(desc) < 3:
        return False

    # Check for header/footer noise in description
    for pat in NOISE_PATTERNS:
        if re.search(pat, desc, re.IGNORECASE):
            return False

    # Check for excessive delimiters or weird spacing
    if desc.count('|') > 3 or desc.count('-') > 10:
        return False

    # Check date
    date = txn.get("date")
    if not date or date == "null" or date == "None":
        return False

    # Check amounts - at least one must be present
    debit = txn.get("debit")
    credit = txn.get("credit")
    if debit is None and credit is None:
        return False

    return True


def validate_extraction_propriety(txns: list) -> bool:
    """Check if the entire set of transactions looks proper."""
    if not txns:
        return False

    # If more than 15% of transactions are improper, the whole set is suspect
    improper_count = sum(1 for t in txns if not is_transaction_proper(t))
    if len(txns) > 0 and (improper_count / len(txns)) > 0.15:
        logger.info(
            "Propriety check failed: %d/%d transactions improper (%.0f%%)",
            improper_count, len(txns), (improper_count / len(txns)) * 100,
        )
        return False

    return True


# ── STRICT QUALITY GATE (NEW) ────────────────────────────

STRICT_NOISE_PATTERNS = [
    r'page\s*\d', r'statement', r'balance', r'total', 
    r'generated', r'date', r'particulars', r'withdrawal', 
    r'deposit', r'chq', r'ref\.?no', r'value\s*date',
    r'opening', r'closing', r'carried', r'brought',
    r'\d{2}[-/]\d{2}[-/]\d{2,4}' # Date pattern inside details
]

def validate_code_quality_strict(txns: list) -> bool:
    """
    ZERO TOLERANCE check for User Display.
    Returns False if ANY transaction has:
    1. Noise in details (headers, dates, footer text).
    2. Mathematical inconsistency (Balance logic broken).
    3. Missing critical fields.
    """
    if not txns:
        return False

    previous_balance = None
    
    for i, txn in enumerate(txns):
        # 1. CRITICAL FIELD CHECK
        date = txn.get("date")
        details = str(txn.get("details") or "").strip()
        debit = float(txn.get("debit") or 0)
        credit = float(txn.get("credit") or 0)
        balance = txn.get("balance") # Can be None for some formats, but if present must match

        # Must have a date and at least one amount or balance
        if not date or (debit == 0 and credit == 0 and balance is None):
            logger.info(f"Strict Check Failed: Row {i} missing critical fields.")
            return False

        # 2. DETAILS HYGIENE CHECK
        if len(details) < 3:
            logger.info(f"Strict Check Failed: Row {i} details too short: '{details}'")
            return False
            
        # Check for banned noise patterns in Description
        for pat in STRICT_NOISE_PATTERNS:
            if re.search(pat, details, re.IGNORECASE):
                # Allow 'Balance' only if it's 'Minimum Balance Charge' etc.
                if "balance" in pat and ("charge" in details.lower() or "fee" in details.lower()):
                    continue
                logger.info(f"Strict Check Failed: Row {i} details contain noise '{pat}': {details}")
                return False

        # 3. MATHEMATICAL LOGIC CHECK (Delta Check)
        # Only applicable if balance is present in this row AND previous row
        if balance is not None and previous_balance is not None:
            curr_bal = float(balance)
            prev_bal = float(previous_balance)
            
            # Expected Balance = Prev - Debit + Credit
            expected_bal = prev_bal - debit + credit
            
            # Allow small float error (0.05)
            if abs(curr_bal - expected_bal) > 0.05:
                # Some banks use reverse logic (Credit cards: Prev + Debit - Credit)
                # Check that scenario too before failing
                expected_bal_reverse = prev_bal + debit - credit
                
                if abs(curr_bal - expected_bal_reverse) > 0.05:
                    logger.info(f"Strict Check Failed: Row {i} Math Mismatch. Prev: {prev_bal}, Dr: {debit}, Cr: {credit}, Curr: {curr_bal}")
                    return False
        
        if balance is not None:
            previous_balance = balance

    return True

# ── VALIDATION METRICS (EXISTING) ────────────────────────

def validate_transactions(code_txns: list, llm_txns: list) -> dict:
    """
    Compare CODE vs LLM transactions using weighted bipartite matching.

    Returns metrics dict with:
      matched_transactions, date_accuracy, amount_accuracy,
      balance_accuracy, description_accuracy, overall_accuracy,
      transaction_count_match
    """
    if not code_txns or not llm_txns:
        return None

    matched_llm_indexes = set()
    date_matches = 0
    amount_matches = 0
    balance_matches = 0
    description_scores = []
    total = 0

    for code in code_txns:
        code_date = normalize_date(code.get("date"))
        code_amount = float(code.get("debit") or code.get("credit") or 0)
        code_balance = float(code.get("balance") or 0)
        code_desc = str(code.get("details") or "").strip()

        best_match_index = None
        best_match_score = 0

        for idx, llm in enumerate(llm_txns):
            if idx in matched_llm_indexes:
                continue

            llm_date = normalize_date(llm.get("date"))
            llm_amount = float(llm.get("debit") or llm.get("credit") or 0)
            llm_balance = float(llm.get("balance") or 0)
            llm_desc = str(llm.get("details") or "").strip()

            score = 0

            # Date match (strong weight)
            if code_date and llm_date and code_date == llm_date:
                score += 3

            # Amount match (strong weight)
            if abs(code_amount - llm_amount) < 1:
                score += 3

            # Balance match (medium weight)
            if abs(code_balance - llm_balance) < 1:
                score += 2

            # Description similarity (soft weight)
            if calculate_similarity(code_desc, llm_desc) > 0.7:
                score += 2

            if score > best_match_score:
                best_match_score = score
                best_match_index = idx

        # Require minimum score to accept match
        if best_match_index is not None and best_match_score >= 3:
            matched_llm_indexes.add(best_match_index)
            llm = llm_txns[best_match_index]
            total += 1

            if normalize_date(code.get("date")) == normalize_date(llm.get("date")):
                date_matches += 1
            if abs(code_amount - float(llm.get("debit") or llm.get("credit") or 0)) < 1:
                amount_matches += 1
            if abs(code_balance - float(llm.get("balance") or 0)) < 1:
                balance_matches += 1
            description_scores.append(
                calculate_similarity(code_desc, llm.get("details", ""))
            )

    if total == 0:
        return None

    date_accuracy = (date_matches / total) * 100
    amount_accuracy = (amount_matches / total) * 100
    balance_accuracy = (balance_matches / total) * 100
    description_accuracy = (sum(description_scores) / total) * 100

    # Weighted final score
    overall_accuracy = (
        (date_accuracy * 0.30)
        + (amount_accuracy * 0.30)
        + (balance_accuracy * 0.25)
        + (description_accuracy * 0.15)
    )

    return {
        "matched_transactions": total,
        "code_count": len(code_txns),
        "llm_count": len(llm_txns),
        "transaction_count_match": len(code_txns) == len(llm_txns),
        "date_accuracy": round(date_accuracy, 2),
        "amount_accuracy": round(amount_accuracy, 2),
        "balance_accuracy": round(balance_accuracy, 2),
        "description_accuracy": round(description_accuracy, 2),
        "overall_accuracy": round(overall_accuracy, 2),
    }