import json
import re
import logging
from datetime import datetime

logger = logging.getLogger("ledgerai.validation_service")


# ── HELPERS ──────────────────────────────────────────────────────────────────

def normalize_date(date_str):
    if not date_str:
        return None
    clean_str = str(date_str).strip()
    for fmt in ("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d %b %Y", "%d-%b-%Y", "%d-%b-%y"):
        try:
            return datetime.strptime(clean_str, fmt).strftime("%Y-%m-%d")
        except (ValueError, TypeError):
            continue
    return clean_str


def extract_json_from_response(response_text: str) -> list:
    """Parse a JSON array from raw LLM response text."""
    cleaned = response_text.replace("```json", "").replace("```", "").strip()
    match = re.search(r"\[.*\]", cleaned, re.DOTALL)
    if match:
        try:
            return json.loads(match.group(0))
        except (json.JSONDecodeError, ValueError):
            logger.warning("Failed to parse JSON from LLM response.")
            return []
    return []


# ── EXACT MATCH CHECK ────────────────────────────────────────────────────────
# Compare CODE transactions against LLM on all 5 fields + count.
# LLM is used as ground truth.
# CODE wins only if everything matches exactly — otherwise LLM wins.

def verify_exact_match(code_txns: list, llm_txns: list) -> dict:
    """
    Strict field-by-field comparison of CODE vs LLM transactions.

    Checks:
      1. Count must be equal
      2. Every CODE transaction must match an LLM transaction exactly on:
           date, details, debit, credit, balance

    Returns:
        {
            "match":      bool,   # True = CODE is correct, use CODE
            "reason":     str,    # why it failed (empty string if match=True)
            "mismatches": list,   # details of each mismatch
            "code_count": int,
            "llm_count":  int,
        }
    """
    # ── Check 1: Count ────────────────────────────────────────────────────
    if len(code_txns) != len(llm_txns):
        reason = f"Count mismatch: CODE={len(code_txns)} LLM={len(llm_txns)}"
        logger.info("verify_exact_match FAILED: %s", reason)
        return {
            "match":      False,
            "reason":     reason,
            "mismatches": [reason],
            "code_count": len(code_txns),
            "llm_count":  len(llm_txns),
        }

    # ── Check 2: Field-by-field ───────────────────────────────────────────
    mismatches = []
    matched_llm = set()

    for ci, code in enumerate(code_txns):
        code_date    = normalize_date(code.get("date")) or ""
        code_debit   = round(float(code.get("debit")   or 0), 2)
        code_credit  = round(float(code.get("credit")  or 0), 2)
        code_balance = round(float(code.get("balance") or 0), 2)
        code_details = str(code.get("details") or "").strip()

        found = False
        for li, llm in enumerate(llm_txns):
            if li in matched_llm:
                continue

            llm_date    = normalize_date(llm.get("date")) or ""
            llm_debit   = round(float(llm.get("debit")   or 0), 2)
            llm_credit  = round(float(llm.get("credit")  or 0), 2)
            llm_balance = round(float(llm.get("balance") or 0), 2)
            llm_details = str(llm.get("details") or "").strip()

            date_ok    = code_date    == llm_date
            debit_ok   = abs(code_debit   - llm_debit)   < 0.01
            credit_ok  = abs(code_credit  - llm_credit)  < 0.01
            balance_ok = abs(code_balance - llm_balance) < 0.05
            details_ok = code_details == llm_details

            if date_ok and debit_ok and credit_ok and balance_ok and details_ok:
                matched_llm.add(li)
                found = True
                break

            # Date + amount match but other fields differ — record mismatch
            if date_ok and debit_ok and credit_ok:
                field_issues = []
                if not balance_ok:
                    field_issues.append(f"balance CODE={code_balance} LLM={llm_balance}")
                if not details_ok:
                    field_issues.append(f"details CODE='{code_details}' LLM='{llm_details}'")
                if field_issues:
                    mismatches.append(f"Txn {ci} ({code_date}): {'; '.join(field_issues)}")
                    matched_llm.add(li)
                    found = True
                    break

        if not found:
            mismatches.append(
                f"Txn {ci} ({code_date} debit={code_debit} credit={code_credit})"
                f" not found in LLM"
            )

    match  = len(mismatches) == 0
    reason = "; ".join(mismatches) if mismatches else ""

    if match:
        logger.info(
            "verify_exact_match PASSED: all %d transactions match LLM exactly",
            len(code_txns),
        )
    else:
        for m in mismatches:
            logger.info("verify_exact_match FAILED: %s", m)

    return {
        "match":      match,
        "reason":     reason,
        "mismatches": mismatches,
        "code_count": len(code_txns),
        "llm_count":  len(llm_txns),
    }