# import re
# import json
# import logging
# from typing import Dict, List, Optional

# from google import genai
# from config import GEMINI_API_KEY, GEMINI_MODEL_NAME
# from services.llm_retry import call_with_retry
# from repository.statement_category_repo import (
#     get_all_matchable_formats,
#     insert_statement_category,
# )

# client = genai.Client(api_key=GEMINI_API_KEY)
# logger = logging.getLogger("ledgerai.identifier_service")


# # ════════════════════════════════════════════════════════════
# # DETERMINISTIC IDENTIFIERS (IFSC, etc.)
# # ════════════════════════════════════════════════════════════

# def extract_ifsc(text: str) -> str:
#     """Extract Indian Financial System Code (IFSC) from text."""
#     match = re.search(r"\b([A-Z]{4}0[A-Z0-9]{6})\b", text)
#     if match:
#         return match.group(1)[:4]  # Return BANK CODE (first 4 chars)
#     return None


# # ════════════════════════════════════════════════════════════
# # TEXT REDUCTION
# # ════════════════════════════════════════════════════════════

# def reduce_text(pages: List[str]) -> Dict:
#     """Compress pages into a structure suitable for LLM classification."""
#     first_page = pages[0][:6000] if pages else ""
#     last_page = pages[-1][-3000:] if pages else ""

#     # Include second page sample if available (often has first transactions)
#     second_page = ""
#     if len(pages) > 1:
#         second_page = pages[1][:3000]

#     headers = [
#         line.strip()
#         for p in pages
#         for line in p.splitlines()
#         if re.search(
#             r"\b(Date|Debit|Credit|Balance|Amount|Withdrawal|Deposit|Narration|Particulars|Description)\b",
#             line, re.I,
#         )
#     ][:15]

#     return {
#         "first_page": first_page,
#         "second_page": second_page,
#         "last_page": last_page,
#         "headers": headers,
#     }


# # ════════════════════════════════════════════════════════════
# # RC4 SUPPORT: FAMILY SIGNAL PRE-FILTER
# # ════════════════════════════════════════════════════════════
# # Terms that are EXCLUSIVE to each family — they do NOT routinely appear
# # in other financial document families. Only used for pre-filtering, not scoring.

# _FAMILY_SIGNALS: Dict[str, List[str]] = {
#     "LOAN_STATEMENT": [
#         "loan account", "home loan", "personal loan", "car loan", "auto loan",
#         "gold loan", "vehicle loan", "loan account no", "loan account number",
#         "emi", "equated monthly installment", "outstanding principal",
#         "principal outstanding", "repayment schedule", "pre-closure",
#         "overdue amount", "bounce charge", "penal interest", "interest applied",
#         "loan statement", "disbursement", "moratorium", "principal due",
#     ],
#     "WALLET_STATEMENT": [
#         "paytm", "google pay", "gpay", "phonepe", "mobikwik", "amazon pay",
#         "freecharge",
#         "upi statement", "passbook payments", "passbook payment history",
#         "total money paid", "total money received",
#         "upi id:", "upi ref no", "order id:", "upi transaction id",
#         "recharge of", "paid to", "received from", "wallet statement",
#     ],
#     "CREDIT_CARD_STATEMENT": [
#         "credit card statement", "minimum amount due", "minimum due",
#         "min. amt. due", "total amount due", "payment due date",
#         "available credit limit", "available cash limit",
#         "reward points", "cash advance limit", "finance charges",
#         "annual fee", "statement balance",
#     ],
#     "INVESTMENT_STATEMENT": [
#         "mutual fund", "net asset value", "folio",
#         "systematic investment plan", "sip",
#         "units allotted", "redemption", "switch in", "switch out",
#         "growth option", "direct plan", "regular plan",
#         "gross systematic investment", "net systematic investment",
#     ],
#     "DEMAT_STATEMENT": [
#         "demat account", "dp id", "bo id", "isin",
#         "depository participant", "beneficiary owner",
#         "buy/cr", "sell/dr", "holdings as on",
#         "cdsl", "nsdl", "zerodha",
#         "transaction with holding statement",
#         "free balance", "pledge balance",
#     ],
#     "BANK_ACCOUNT_STATEMENT": [
#         "savings account", "current account", "salary account",
#         "overdraft account",
#         "chq no", "cheque no", "cheque number",
#         "atm withdrawal", "atm cash",
#         "neft", "rtgs", "imps",
#         "opening balance", "closing balance",
#         "account summary", "ifsc",
#     ],
# }

# _FAMILY_MIN_SIGNALS: Dict[str, int] = {
#     "LOAN_STATEMENT": 2,
#     "WALLET_STATEMENT": 1,   # Even one Paytm/GPay signal is conclusive
#     "CREDIT_CARD_STATEMENT": 2,
#     "INVESTMENT_STATEMENT": 2,
#     "DEMAT_STATEMENT": 2,
#     "BANK_ACCOUNT_STATEMENT": 2,
# }

# # When a family is detected, formats from these families are excluded
# _FAMILY_EXCLUSIONS: Dict[str, List[str]] = {
#     "LOAN_STATEMENT":        ["BANK_ACCOUNT_STATEMENT", "CREDIT_CARD_STATEMENT",
#                                "GENERIC_STATEMENT_OF_ACCOUNT"],
#     "WALLET_STATEMENT":      ["BANK_ACCOUNT_STATEMENT", "GENERIC_STATEMENT_OF_ACCOUNT"],
#     "CREDIT_CARD_STATEMENT": ["BANK_ACCOUNT_STATEMENT", "LOAN_STATEMENT"],
#     "INVESTMENT_STATEMENT":  ["BANK_ACCOUNT_STATEMENT", "DEMAT_STATEMENT"],
#     "DEMAT_STATEMENT":       ["BANK_ACCOUNT_STATEMENT", "INVESTMENT_STATEMENT"],
# }

# _GENERIC_FAMILIES = frozenset([
#     "GENERIC_STATEMENT_OF_ACCOUNT",
#     "UNKNOWN_FINANCIAL_DOCUMENT",
# ])


# def _detect_primary_family(text: str) -> Optional[str]:
#     """
#     Scan the document text for exclusive family signals.
#     Returns the family with the most signals above its minimum, or None.
#     """
#     text_lower = text.lower()
#     family_counts: Dict[str, int] = {}
#     for family, signals in _FAMILY_SIGNALS.items():
#         count = sum(1 for s in signals if s in text_lower)
#         if count >= _FAMILY_MIN_SIGNALS.get(family, 2):
#             family_counts[family] = count
#     if not family_counts:
#         return None
#     return max(family_counts, key=lambda f: family_counts[f])


# # ════════════════════════════════════════════════════════════
# # RC2 SUPPORT: GENERIC HEADER SPECIFICITY WEIGHT
# # ════════════════════════════════════════════════════════════

# _GENERIC_HEADERS = frozenset([
#     "date", "amount", "balance", "debit", "credit", "description",
#     "details", "narration", "particulars", "sl no", "sr no", "no",
#     "transaction", "type", "ref", "reference", "value date",
#     "chq", "cheque", "dr", "cr",
# ])


# def _header_specificity_multiplier(header: str) -> float:
#     """
#     Returns 0.3 for generic headers that appear in every financial document,
#     and 1.0 for distinctive headers exclusive to specific document families.
#     Prevents generic columns (Date/Debit/Credit/Balance) from giving a format
#     30 free points against unrelated document types.
#     """
#     h_clean = re.sub(r"[\s().\[\]:]", "", header.lower())
#     if h_clean in _GENERIC_HEADERS:
#         return 0.3
#     first_word = header.lower().split()[0] if header.split() else ""
#     if first_word in _GENERIC_HEADERS:
#         return 0.5
#     return 1.0


# # ════════════════════════════════════════════════════════════
# # IDENTITY MARKER EVALUATION — FIXED (RC1, RC2, RC3, RC6)
# # ════════════════════════════════════════════════════════════

# def evaluate_identity_markers(
#     identity: Dict,
#     text: str,
#     identifier_json: Optional[Dict] = None,
# ) -> float:
#     """
#     Score how well a set of identity markers matches the document text.
#     Returns a confidence percentage (0-100).

#     RC1/RC6: exclusion_markers hard-fail — returns 0.0 if any exclusion
#              pattern matches the text.
#     RC2:     table headers weighted by specificity — generic headers earn 30%.
#     RC3:     document_title_phrase scored at weight=15 instead of default 5.
#     """
#     text_norm = re.sub(r"\s+", " ", text.lower())
#     total_score = 0.0
#     total_max = 0.0

#     # ── RC1/RC6: Hard-fail on exclusion_markers ──────────────────────────────
#     # Top-level exclusion_markers in the identifier_json row
#     if identifier_json:
#         excl = identifier_json.get("exclusion_markers", {})
#         excl_patterns = excl.get("patterns", []) if isinstance(excl, dict) else (excl or [])
#         for pat in excl_patterns:
#             if not pat:
#                 continue
#             try:
#                 if re.search(pat, text, re.I):
#                     logger.debug("Exclusion marker '%s' matched → score=0", pat)
#                     return 0.0
#             except re.error:
#                 if pat.lower() in text_norm:
#                     logger.debug("Exclusion keyword '%s' matched → score=0", pat)
#                     return 0.0

#     # Exclusion_markers nested inside identity_markers (alternate storage path)
#     inner_excl = identity.get("exclusion_markers", {})
#     inner_patterns = (
#         inner_excl.get("patterns", []) if isinstance(inner_excl, dict) else (inner_excl or [])
#     )
#     for pat in inner_patterns:
#         if not pat:
#             continue
#         try:
#             if re.search(pat, text, re.I):
#                 logger.debug("Exclusion marker (inner) '%s' matched → score=0", pat)
#                 return 0.0
#         except re.error:
#             if pat.lower() in text_norm:
#                 return 0.0

#     # ── Rule processor — RC3: configurable weight per field ──────────────────
#     def process_rule(rule_obj, weight: float = 5.0):
#         nonlocal total_score, total_max
#         if not rule_obj:
#             return

#         # Plain list of keywords
#         if isinstance(rule_obj, list) and rule_obj:
#             total_max += weight
#             if any(isinstance(k, str) and k.lower() in text_norm for k in rule_obj):
#                 total_score += weight
#             return

#         if not isinstance(rule_obj, dict):
#             return

#         rule_type = rule_obj.get("rule")

#         if rule_type == "keyword":
#             patterns = rule_obj.get("patterns", [])
#             if not patterns:
#                 return
#             total_max += weight
#             if any(p.lower() in text_norm for p in patterns):
#                 total_score += weight

#         elif rule_type == "regex":
#             pattern = rule_obj.get("pattern")
#             if not pattern:
#                 return
#             total_max += weight
#             try:
#                 if bool(re.search(pattern, text, re.I)):
#                     total_score += weight
#             except re.error:
#                 pass  # Silently ignore malformed patterns

#     # Walk the identity structure (skipping table/exclusion which are handled below)
#     for section_name, section in identity.items():
#         if not isinstance(section, dict):
#             continue
#         if section_name in ("transaction_table_identity", "exclusion_markers"):
#             continue

#         for field_name, field_value in section.items():
#             # RC3: document_title_phrase at 3× weight — it's the strongest
#             # differentiator when the LLM has stored an exclusive title phrase.
#             field_weight = 15.0 if field_name == "document_title_phrase" else 5.0

#             if isinstance(field_value, dict) and "rule" not in field_value:
#                 # Nested sub-dict (e.g. regulatory_identifiers)
#                 for sub_name, sub_value in field_value.items():
#                     process_rule(sub_value, weight=field_weight)
#             else:
#                 process_rule(field_value, weight=field_weight)

#     # ── RC2: Weighted table header matching ───────────────────────────────────
#     table = identity.get("transaction_table_identity", {})
#     headers = table.get("table_header_markers", [])
#     min_required = table.get("minimum_column_count", 1)

#     if headers:
#         base_weight = 30.0
#         total_max += base_weight
#         text_compact = re.sub(r"\s+", "", text.lower())

#         per_header_base = base_weight / len(headers)
#         # Weighted total achievable if all headers matched at full specificity
#         weighted_possible = sum(
#             per_header_base * _header_specificity_multiplier(h) for h in headers
#         )
#         weighted_matched = 0.0
#         matched_count = 0

#         for h in headers:
#             if re.sub(r"\s+", "", h.lower()) in text_compact:
#                 matched_count += 1
#                 weighted_matched += per_header_base * _header_specificity_multiplier(h)

#         # Normalise back onto the base_weight scale so total_max stays consistent
#         header_score = (
#             (weighted_matched / weighted_possible) * base_weight
#             if weighted_possible > 0 else 0.0
#         )

#         if matched_count >= min_required:
#             total_score += header_score
#         else:
#             # Still give partial points but penalised for missing minimum
#             total_score += header_score * 0.5

#     # Footer matching (unchanged)
#     footer = identity.get("footer_identity", {})
#     footer_patterns = footer.get("footer_markers", [])
#     if footer_patterns:
#         weight = 5.0
#         total_max += weight
#         matched = any(
#             re.search(p, text, re.I) if "\\" in p else p.lower() in text_norm
#             for p in footer_patterns
#         )
#         if matched:
#             total_score += weight

#     confidence = round((total_score / total_max) * 100, 2) if total_max else 0.0
#     return confidence


# # ════════════════════════════════════════════════════════════
# # STEP 2 — FORMAT CHECK IN DATABASE — FIXED (RC4, RC7)
# # ════════════════════════════════════════════════════════════

# def find_existing_identifier(text: str, default_threshold: float = 80.0):
#     """
#     Search all matchable formats (ACTIVE + UNDER_REVIEW + EXPERIMENTAL).
#     Respects the per-category match_threshold stored in the database.
#     Returns (True, category_row) if matched, (False, None) otherwise.

#     RC4: (a) Family signal pre-filter narrows the candidate pool to only the
#          detected family before scoring — prevents bank formats scoring on loan
#          docs and vice versa.
#          (b) Generic formats (no institution + GENERIC family) are only used as
#          a fallback when no specific format matched, with a 15-pt score penalty.
#     RC7: Sort key adds specificity_rank so institution-specific formats rank
#          before generic ones at equal or near-equal scores.
#     """
#     categories = get_all_matchable_formats()

#     # ── Deterministic IFSC filtering (unchanged) ─────────────────────────────
#     bank_code = extract_ifsc(text)
#     if bank_code:
#         logger.info("Detected Bank Code: %s. Filtering candidates.", bank_code)
#         filtered = [
#             c for c in categories
#             if c.get("ifsc_code") == bank_code or not c.get("ifsc_code")
#         ]
#         if filtered:
#             categories = filtered
#             logger.info("Reduced candidate pool to %d (via Bank Code).", len(categories))

#     # ── RC4a: Family signal pre-filter ───────────────────────────────────────
#     detected_family = _detect_primary_family(text)

#     if detected_family:
#         excluded_families = _FAMILY_EXCLUSIONS.get(detected_family, [])
#         family_matched = [c for c in categories if c.get("document_family") == detected_family]
#         generic_pool   = [c for c in categories if c.get("document_family") in _GENERIC_FAMILIES]
#         excluded_pool  = [c for c in categories if c.get("document_family") in excluded_families]

#         if family_matched:
#             # Primary pool: matching-family formats + generics as fallback
#             categories = family_matched + [c for c in generic_pool if c not in family_matched]
#             logger.info(
#                 "[FAMILY FILTER] Detected '%s'. Narrowed to %d candidates "
#                 "(excluded %d cross-family).",
#                 detected_family, len(categories), len(excluded_pool),
#             )
#         else:
#             # Desired family not yet in DB — exclude cross-family noise only
#             categories = [c for c in categories if c not in excluded_pool]
#             logger.info(
#                 "[FAMILY FILTER] Detected '%s' but no DB formats exist for it. "
#                 "Excluded %d cross-family candidates.",
#                 detected_family, len(excluded_pool),
#             )
#     else:
#         logger.info(
#             "[FAMILY FILTER] No strong family signal. Scoring all %d candidates.",
#             len(categories),
#         )

#     logger.info("Matching against %d candidates.", len(categories))

#     # ── RC4b: Separate generic formats into lower-priority pool ──────────────
#     doc_clean = re.sub(r"\s+", "", text[:2000].lower())
#     specific_candidates: List[Dict] = []
#     generic_candidates:  List[Dict] = []

#     for cat in categories:
#         inst_name  = (cat.get("institution_name") or "").lower()
#         inst_clean = re.sub(r"\s+", "", inst_name)
#         doc_family = cat.get("document_family", "")

#         is_generic = (
#             (not inst_clean or inst_clean == "unknown") and doc_family in _GENERIC_FAMILIES
#         )

#         if is_generic:
#             generic_candidates.append(cat)
#             continue

#         # Standard institution name filter (unchanged logic)
#         if inst_clean and inst_clean != "unknown" and inst_clean not in doc_clean:
#             continue

#         specific_candidates.append(cat)

#     logger.info(
#         "After institution filter: %d specific, %d generic candidates.",
#         len(specific_candidates), len(generic_candidates),
#     )

#     # ── Scoring ───────────────────────────────────────────────────────────────
#     from decimal import Decimal

#     def score_pool(pool: List[Dict], score_penalty: float = 0.0) -> List[Dict]:
#         matches = []
#         for cat in pool:
#             identifier_json = cat.get("statement_identifier", {})
#             if isinstance(identifier_json, str):
#                 try:
#                     identifier_json = json.loads(identifier_json)
#                 except Exception:
#                     identifier_json = {}

#             identity = identifier_json.get("identity_markers", {})

#             # RC1/RC6: Pass full identifier_json so top-level exclusion_markers apply
#             score = evaluate_identity_markers(identity, text, identifier_json=identifier_json)
#             adjusted_score = max(0.0, score - score_penalty)

#             stored_threshold = cat.get("match_threshold")
#             if isinstance(stored_threshold, Decimal):
#                 stored_threshold = float(stored_threshold)
#             target_threshold = stored_threshold if stored_threshold is not None else default_threshold

#             logger.info(
#                 "  Evaluating: %s | raw=%.2f adjusted=%.2f | threshold=%.1f | status=%s",
#                 cat.get("format_name", "?"), score, adjusted_score,
#                 target_threshold, cat.get("status"),
#             )

#             if adjusted_score >= target_threshold:
#                 matches.append({
#                     "category": cat,
#                     "score": adjusted_score,
#                     "status": cat.get("status"),
#                     "is_generic": score_penalty > 0,
#                 })
#         return matches

#     # Score specific formats first
#     matches = score_pool(specific_candidates, score_penalty=0.0)

#     # RC4b: Only try generics when nothing specific matched
#     if not matches and generic_candidates:
#         logger.info(
#             "No specific match. Trying %d generic formats (15-pt penalty).",
#             len(generic_candidates),
#         )
#         matches = score_pool(generic_candidates, score_penalty=15.0)

#     if not matches:
#         logger.info("No format reached the match threshold.")
#         return False, None

#     # ── RC7: Sort with specificity rank ──────────────────────────────────────
#     def sort_key(m: Dict):
#         status_rank      = 0 if m["status"] == "ACTIVE" else 1
#         specificity_rank = 1 if m.get("is_generic") else 0   # specifics first
#         return (status_rank, specificity_rank, -m["score"], -m["category"].get("statement_id", 0))

#     matches.sort(key=sort_key)
#     best_match = matches[0]

#     logger.info(
#         ">>> MATCHED: %s (score %.2f, status %s, family %s)",
#         best_match["category"].get("format_name"),
#         best_match["score"],
#         best_match["status"],
#         best_match["category"].get("document_family", "?"),
#     )

#     return True, best_match["category"]


# # ════════════════════════════════════════════════════════════
# # STEP 3 — GENERATE IDENTIFICATION MARKERS (LLM) — FIXED (RC5, RC6)
# # ════════════════════════════════════════════════════════════

# def classify_document_llm(reduced: Dict) -> Dict:
#     """
#     Use Gemini to classify the document and generate granular identity markers
#     using the detailed V1 JSON schema.

#     RC5: Prompt now requires EXCLUSIVE markers per RULE 1 — terms that appear
#          in THIS family only, with wrong vs correct examples per family.
#     RC6: exclusion_markers added as a mandatory first-class output field.
#     RC3: Prompt requires document_title_phrase to be the EXACT title phrase.
#     """

#     prompt = f"""
# You are a Senior Financial Document Classification Engine.
# Return a detailed structural JSON "blueprint" for the following document.
# Your task has TWO PARTS:

# PART 1 → Classify the financial document
# PART 2 → Extract structural identity markers

# Use ONLY the provided text.
# Do NOT guess.
# If insufficient evidence → classify as UNKNOWN_FINANCIAL_DOCUMENT.

# ════════════════════════════════════════════
# PART 1 — CLASSIFICATION FAMILIES
# ════════════════════════════════════════════
# Choose EXACTLY ONE from this specific list:
#  1. BANK_ACCOUNT_STATEMENT
#  2. CREDIT_CARD_STATEMENT
#  3. LOAN_STATEMENT
#  4. OVERDRAFT_CASH_CREDIT_STATEMENT
#  5. WALLET_STATEMENT
#  6. PAYMENT_GATEWAY_SETTLEMENT
#  7. INVESTMENT_STATEMENT
#  8. DEMAT_STATEMENT
#  9. TAX_LEDGER_STATEMENT
# 10. FIXED_DEPOSIT_STATEMENT
# 11. RECURRING_DEPOSIT_STATEMENT
# 12. INSURANCE_POLICY_STATEMENT
# 13. PENSION_STATEMENT
# 14. BROKERAGE_CONTRACT_NOTE
# 15. FOREX_STATEMENT
# 16. ESCROW_STATEMENT
# 17. GENERIC_STATEMENT_OF_ACCOUNT

# ════════════════════════════════════════════
# ⚠️  CLASSIFICATION CONFLICT RESOLUTION
#     (APPLY THESE HARD STOPS BEFORE CLASSIFYING)
# ════════════════════════════════════════════

# These rules override all other signals. Check them IN ORDER.

# STOP 1 — LOAN_STATEMENT (highest priority over BANK)
#   If ANY of the following exist → STOP → classify as LOAN_STATEMENT:
#     • Loan Account Number / Loan A/c No
#     • EMI / Equated Monthly Installment
#     • Principal Outstanding / Outstanding Principal
#     • Principal Repaid
#     • Sanction Amount / Disbursement
#     • Repayment Schedule / Installment Due Date
#     • Total Loan Outstanding
#     • Amortization Schedule
#   ⚠️  Do NOT classify as BANK_ACCOUNT_STATEMENT if ANY loan marker exists.
#   ⚠️  A loan statement may contain a debit/credit ledger — that does NOT make it a bank statement.

# STOP 2 — CREDIT_CARD_STATEMENT
#   If ANY of the following exist → STOP → classify as CREDIT_CARD_STATEMENT:
#     • Masked card number (XXXX-XXXX-1234)
#     • Minimum Amount Due
#     • Credit Limit / Available Credit
#     • Cash Advance Limit
#     • Finance Charges / Revolving Credit
#   ⚠️  If Minimum Amount Due AND Payment Due Date AND Total Amount Due all exist → CREDIT_CARD_STATEMENT.

# STOP 3 — DEMAT_STATEMENT
#   If ANY of the following exist → STOP → classify as DEMAT_STATEMENT:
#     • DP ID
#     • BO ID / Beneficial Owner
#     • ISIN column
#     • NSDL / CDSL
#     • Pledge Balance / Free Balance (securities context)
#   ⚠️  If ISIN column exists → STOP → DEMAT_STATEMENT.
#   ⚠️  If DP ID + ISIN both exist → STOP → DEMAT_STATEMENT.

# STOP 4 — OVERDRAFT_CASH_CREDIT_STATEMENT
#   Classify as OVERDRAFT_CASH_CREDIT_STATEMENT ONLY IF:
#     • TWO OR MORE of: Drawing Power, Sanctioned Limit, CC Limit, Limit Utilized,
#       Available Limit, Cash Credit Account, OD Account, Interest on CC
#     AND OD Limit > 0 OR Sanctioned Limit with utilization structure exists
#     AND running balance ledger exists.
#   ⚠️  If OD Limit = 0.00 AND no sanctioned limit structure → do NOT classify as OVERDRAFT.

# STOP 5 — TAX_LEDGER_STATEMENT
#   If GSTIN + Tax Period + CGST/SGST present → STOP → TAX_LEDGER_STATEMENT.

# STOP 6 — INVESTMENT_STATEMENT vs DEMAT (disambiguation)
#   DEMAT:      has ISIN + DP ID → use STOP 3 above.
#   INVESTMENT: has NAV + Units Held (no ISIN column) → INVESTMENT_STATEMENT.

# STOP 7 — BANK_ACCOUNT_STATEMENT (only after all stops above are clear)
#   Classify as BANK_ACCOUNT_STATEMENT ONLY IF:
#     • No loan markers (STOP 1 is clear)
#     • No credit card markers (STOP 2 is clear)
#     • No demat markers (STOP 3 is clear)
#     • No OD/CC markers that qualify (STOP 4 is clear)
#     AND
#     • Account Number exists
#     AND (IFSC OR Branch Name exists)
#     AND transaction ledger with Date + Debit + Credit + Balance exists
#     OR running balance column is present.

# ════════════════════════════════════════════
# DOCUMENT TEXT SAMPLE
# ════════════════════════════════════════════

# FIRST PAGE:
# """ + reduced["first_page"] + """

# LAST PAGE:
# """ + reduced["last_page"] + """

# TABLE HEADERS:
# """ + json.dumps(reduced["headers"]) + """

# OTHER TEXT:
# """ + reduced.get("other_sample", "") + """

# ════════════════════════════════════════════
# ⚠️  CRITICAL RULES FOR IDENTITY MARKERS
# ════════════════════════════════════════════

# RULE 1 — USE EXCLUSIVE SIGNALS ONLY
#   Every pattern you store MUST be exclusive to this document family.
#   It must NOT appear in other financial document types.

#   WRONG — these appear in every financial document:
#     document_title_phrase: ["Account Statement", "Statement of Account"]
#     table_header_markers:  ["Date", "Debit", "Credit", "Balance"]

#   CORRECT — exclusive to each family:
#     LOAN:        "Home Loan Account Statement", "Loan Account No", "EMI", "Outstanding Principal"
#     WALLET:      "Paytm UPI Statement", "Passbook Payments History", "UPI Ref No", "Total Money Paid"
#     CREDIT CARD: "Credit Card Statement", "Minimum Amount Due", "Available Credit Limit", "Reward Points"
#     INVESTMENT:  "Mutual Fund", "Net Systematic Investment", "Folio No", "NAV", "Units Allotted"
#     DEMAT:       "Transaction With Holding Statement", "Buy/Cr", "Sell/Dr", "DP ID", "ISIN"

# RULE 2 — document_title_phrase MUST BE THE EXACT TITLE
#   Copy the EXACT title phrase visible at the top of the document.
#   Do NOT shorten it or use a generic version.

#   BAD:  ["Account Statement"]
#   GOOD: ["Home Loan Account Statement"] or ["Paytm UPI Statement"]

# RULE 3 — exclusion_markers IS MANDATORY (3–5 entries required)
#   List terms whose presence definitively rules out this format.
#   Use terms from SIMILAR but DIFFERENT document families.

#   For LOAN_STATEMENT:          ["Savings Account", "Credit Card", "Paytm", "Folio No", "ISIN"]
#   For WALLET_STATEMENT:        ["Loan Account", "Credit Limit", "IFSC", "Folio No", "ISIN"]
#   For BANK_ACCOUNT_STATEMENT:  ["Loan Account No", "EMI", "Minimum Amount Due", "Paytm", "Folio"]
#   For INVESTMENT_STATEMENT:    ["Loan Account", "Credit Card", "Paytm", "ISIN", "DP ID"]
#   For DEMAT_STATEMENT:         ["Loan Account", "Credit Card", "Folio No", "SIP", "NAV"]

# RULE 4 — table_header_markers: PREFER DISTINCTIVE HEADERS
#   Include ALL visible column headers from the transaction table.
#   PRIORITISE distinctive ones specific to this document type:
#     Loan:        "Outstanding Balance", "Principal", "Interest Applied"
#     Wallet:      "Your Account", "Notes & Tags", "UPI Transaction ID"
#     Credit Card: "Merchant Category", "Reward Points", "Available Credit"
#     Investment:  "NAV", "Units", "Balance Units", "Transaction Type"
#     Demat:       "Buy/Cr", "Sell/Dr", "ISIN", "Current Bal", "Free Bal"

# ════════════════════════════════════════════
# STRICT RULES:
# - No guessing
# - No explanation
# - No markdown
# - Return STRICT valid JSON only
# - Use null where field not present
# - Subtype must match Level 1
# - If unsure → UNKNOWN_FINANCIAL_DOCUMENT

# STATEMENT VERSIONING:
# - ID format: [document_family]_[document_subtype]_[VERSION]

# ════════════════════════════════════════════
# PART 2 — RETURN THIS EXACT JSON STRUCTURE
# ════════════════════════════════════════════
# {{
#   "id": "UNIQUE_ID_V1",
#   "document_family": "<chosen family>",
#   "document_subtype": "<e.g. Savings, Current, Platinum Card>",
#   "institution_name": "<detected institution>",
#   "country": "India",
#   "confidence_score": 0.95,

#   "exclusion_markers": {{
#     "patterns": [
#       "<term whose presence definitively rules out this format>",
#       "<term whose presence definitively rules out this format>",
#       "<term whose presence definitively rules out this format>"
#     ]
#   }},

#   "identity_markers": {{
#     "issuer_identity": {{
#       "issuer_name": {{ "rule": "keyword", "patterns": [] }},
#       "regulatory_identifiers": {{
#         "ifsc": {{ "rule": "regex", "pattern": "..." }},
#         "swift": {{ "rule": "regex", "pattern": "..." }},
#         "iban": {{ "rule": "regex", "pattern": "..." }},
#         "gstin": {{ "rule": "regex", "pattern": "..." }},
#         "other": []
#       }}
#     }},

#     "document_structure_identity": {{
#       "document_title_phrase": {{
#         "rule": "keyword",
#         "patterns": ["<EXACT title phrase — must be exclusive to this family>"]
#       }},
#       "document_reference_number": {{ "rule": "regex", "pattern": null }},
#       "generation_phrase": {{ "rule": "keyword", "patterns": [] }}
#     }},

#     "period_identity": {{
#       "statement_period": {{ "rule": "regex", "pattern": null }},
#       "statement_date":   {{ "rule": "regex", "pattern": null }},
#       "billing_cycle":    {{ "rule": "regex", "pattern": null }},
#       "tax_period":       {{ "rule": "regex", "pattern": null }}
#     }},

#     "entity_identity": {{
#       "account_number":      {{ "rule": "regex", "pattern": null }},
#       "masked_card_number":  {{ "rule": "regex", "pattern": null }},
#       "loan_account_number": {{ "rule": "regex", "pattern": null }},
#       "customer_id":         {{ "rule": "regex", "pattern": null }},
#       "wallet_id":           {{ "rule": "regex", "pattern": null }},
#       "merchant_id":         {{ "rule": "regex", "pattern": null }},
#       "pan":                 {{ "rule": "regex", "pattern": null }},
#       "bo_id":               {{ "rule": "regex", "pattern": null }},
#       "dp_id":               {{ "rule": "regex", "pattern": null }}
#     }},

#     "transaction_table_identity": {{
#       "table_header_markers": ["<ALL visible column headers — prioritise distinctive ones>"],
#       "minimum_column_count": 0,
#       "presence_of_running_balance": false,
#       "debit_credit_style": false
#     }},

#     "financial_summary_identity": {{
#       "total_outstanding": {{ "rule": "regex", "pattern": null }},
#       "minimum_due":       {{ "rule": "regex", "pattern": null }},
#       "emi_amount":        {{ "rule": "regex", "pattern": null }},
#       "credit_limit":      {{ "rule": "regex", "pattern": null }},
#       "drawing_power":     {{ "rule": "regex", "pattern": null }},
#       "portfolio_value":   {{ "rule": "regex", "pattern": null }},
#       "total_tax":         {{ "rule": "regex", "pattern": null }}
#     }},

#     "footer_identity": {{
#       "footer_markers": []
#     }}
#   }}
# }}
# ════════════════════════════════════════════
# OUTPUT RULES
# ════════════════════════════════════════════

# - Return ONLY the object above
# - Do NOT wrap in markdown
# - Do NOT add explanations
# - Use Python literals: None, True, False
# """

#     response = call_with_retry(
#         client, GEMINI_MODEL_NAME, prompt,
#         config={"temperature": 0},
#     )

#     # ── Robust JSON extraction (unchanged) ───────────────────────────────────
#     raw = response.text.strip()

#     def sanitize_json(s: str) -> str:
#         s = re.sub(r"```(?:json)?", "", s)
#         start = s.find("{")
#         end   = s.rfind("}")
#         if start == -1 or end == -1:
#             return s
#         s = s[start:end + 1]
#         s = re.sub(r",\s*([\]}])", r"\1", s)
#         s = re.sub(r":\s*True\b",  ": true",  s)
#         s = re.sub(r":\s*False\b", ": false", s)
#         s = re.sub(r":\s*None\b",  ": null",  s)
#         if s.count("{") > s.count("}"):
#             s += "}" * (s.count("{") - s.count("}"))
#         return s

#     try:
#         clean      = sanitize_json(raw)
#         identifier = json.loads(clean)
#     except Exception as e:
#         logger.error(
#             "Failed to parse classification JSON. Error: %s. Raw response: %s", e, raw
#         )
#         match = re.search(r"(\{.*\})", raw, re.DOTALL)
#         if match:
#             try:
#                 identifier = json.loads(sanitize_json(match.group(1)))
#             except Exception:
#                 raise ValueError(
#                     f"LLM classification returned invalid JSON despite sanitization: {e}"
#                 )
#         else:
#             raise ValueError(
#                 f"LLM classification returned no JSON-like content: {e}"
#             )

#     logger.info(
#         "LLM classified: family=%s, institution=%s",
#         identifier.get("document_family"),
#         identifier.get("institution_name"),
#     )

#     return identifier

# # ════════════════════════════════════════════════════════════
# # SAVE NEW FORMAT
# # ════════════════════════════════════════════════════════════

# def derive_statement_type(identifier_json: dict) -> str:
#     family = identifier_json.get("document_family", "UNKNOWN")
#     type_map = {
#         "BANK_ACCOUNT_STATEMENT":          "BANK_STATEMENT",
#         "CREDIT_CARD_STATEMENT":           "CREDIT_CARD",
#         "LOAN_STATEMENT":                  "LOAN",
#         "WALLET_STATEMENT":                "WALLET",
#         "INVESTMENT_STATEMENT":            "INVESTMENT",
#         "DEMAT_STATEMENT":                 "DEMAT",
#         "TAX_LEDGER_STATEMENT":            "TAX_LEDGER",
#         "PAYMENT_GATEWAY_SETTLEMENT":      "PAYMENT_GATEWAY",
#         "OVERDRAFT_CASH_CREDIT_STATEMENT": "OD_CC",
#     }
#     return type_map.get(family, family)


# def save_new_statement_format(
#     format_name: str,
#     identifier_json: dict,
#     extraction_logic: str,
#     threshold: float = 65.0,
# ) -> int:
#     """
#     Persist a newly generated format into statement_categories.
#     """
#     statement_type   = derive_statement_type(identifier_json)
#     institution_name = identifier_json.get("institution_name", "Unknown")

#     logger.info(
#         "Saving new format: name=%s type=%s institution=%s",
#         format_name, statement_type, institution_name,
#     )

#     return insert_statement_category(
#         statement_type=statement_type,
#         format_name=format_name,
#         institution_name=institution_name,
#         identifier_json=identifier_json,
#         extraction_logic=extraction_logic,
#         threshold=threshold,
#     )
import re
import json
import logging
from typing import Dict, List, Optional

from google import genai
from config import GEMINI_API_KEY, GEMINI_MODEL_NAME
from services.llm_retry import call_with_retry
from repository.statement_category_repo import (
    get_all_matchable_formats,
    insert_statement_category,
)

client = genai.Client(api_key=GEMINI_API_KEY)
logger = logging.getLogger("ledgerai.identifier_service")


# ════════════════════════════════════════════════════════════
# DETERMINISTIC IDENTIFIERS (IFSC, etc.)
# ════════════════════════════════════════════════════════════

def extract_ifsc(text: str) -> str:
    """Extract Indian Financial System Code (IFSC) from text."""
    match = re.search(r"\b([A-Z]{4}0[A-Z0-9]{6})\b", text)
    if match:
        return match.group(1)[:4]
    return None


# ════════════════════════════════════════════════════════════
# TEXT REDUCTION
# ════════════════════════════════════════════════════════════

def reduce_text(pages: List[str]) -> Dict:
    """Compress pages into a structure suitable for LLM classification."""
    first_page  = pages[0][:6000] if pages else ""
    last_page   = pages[-1][-3000:] if pages else ""
    second_page = pages[1][:3000] if len(pages) > 1 else ""

    headers = [
        line.strip()
        for p in pages
        for line in p.splitlines()
        if re.search(
            r"\b(Date|Debit|Credit|Balance|Amount|Withdrawal|Deposit|Narration|Particulars|Description)\b",
            line, re.I,
        )
    ][:15]

    return {
        "first_page":  first_page,
        "second_page": second_page,
        "last_page":   last_page,
        "headers":     headers,
    }


# ════════════════════════════════════════════════════════════
# RC4 SUPPORT: FAMILY SIGNAL PRE-FILTER
# ════════════════════════════════════════════════════════════

_FAMILY_SIGNALS: Dict[str, List[str]] = {
    "LOAN_STATEMENT": [
        "loan account", "home loan", "personal loan", "car loan", "auto loan",
        "gold loan", "vehicle loan", "loan account no", "loan account number",
        "emi", "equated monthly installment", "outstanding principal",
        "principal outstanding", "repayment schedule", "pre-closure",
        "overdue amount", "bounce charge", "penal interest", "interest applied",
        "loan statement", "disbursement", "moratorium", "principal due",
    ],
    "WALLET_STATEMENT": [
        "paytm", "google pay", "gpay", "phonepe", "mobikwik", "amazon pay",
        "freecharge", "upi statement", "passbook payments", "passbook payment history",
        "total money paid", "total money received", "upi id:", "upi ref no",
        "order id:", "upi transaction id", "recharge of", "paid to",
        "received from", "wallet statement",
    ],
    "CREDIT_CARD_STATEMENT": [
        "credit card statement", "minimum amount due", "minimum due",
        "min. amt. due", "total amount due", "payment due date",
        "available credit limit", "available cash limit",
        "reward points", "cash advance limit", "finance charges",
        "annual fee", "statement balance",
    ],
    "INVESTMENT_STATEMENT": [
        "mutual fund", "net asset value", "folio",
        "systematic investment plan", "sip",
        "units allotted", "redemption", "switch in", "switch out",
        "growth option", "direct plan", "regular plan",
        "gross systematic investment", "net systematic investment",
    ],
    "DEMAT_STATEMENT": [
        "demat account", "dp id", "bo id", "isin",
        "depository participant", "beneficiary owner",
        "buy/cr", "sell/dr", "holdings as on",
        "cdsl", "nsdl", "zerodha",
        "transaction with holding statement",
        "free balance", "pledge balance",
    ],
    "BANK_ACCOUNT_STATEMENT": [
        "savings account", "current account", "salary account",
        "overdraft account", "chq no", "cheque no", "cheque number",
        "atm withdrawal", "atm cash", "neft", "rtgs", "imps",
        "opening balance", "closing balance", "account summary", "ifsc",
    ],
}

_FAMILY_MIN_SIGNALS: Dict[str, int] = {
    "LOAN_STATEMENT":          2,
    "WALLET_STATEMENT":        1,
    "CREDIT_CARD_STATEMENT":   2,
    "INVESTMENT_STATEMENT":    2,
    "DEMAT_STATEMENT":         2,
    "BANK_ACCOUNT_STATEMENT":  2,
}

_FAMILY_EXCLUSIONS: Dict[str, List[str]] = {
    "LOAN_STATEMENT":        ["BANK_ACCOUNT_STATEMENT", "CREDIT_CARD_STATEMENT",
                               "GENERIC_STATEMENT_OF_ACCOUNT"],
    "WALLET_STATEMENT":      ["BANK_ACCOUNT_STATEMENT", "GENERIC_STATEMENT_OF_ACCOUNT"],
    "CREDIT_CARD_STATEMENT": ["BANK_ACCOUNT_STATEMENT", "LOAN_STATEMENT"],
    "INVESTMENT_STATEMENT":  ["BANK_ACCOUNT_STATEMENT", "DEMAT_STATEMENT"],
    "DEMAT_STATEMENT":       ["BANK_ACCOUNT_STATEMENT", "INVESTMENT_STATEMENT"],
}

_GENERIC_FAMILIES = frozenset([
    "GENERIC_STATEMENT_OF_ACCOUNT",
    "UNKNOWN_FINANCIAL_DOCUMENT",
])


def _detect_primary_family(text: str) -> Optional[str]:
    text_lower = text.lower()
    family_counts: Dict[str, int] = {}
    for family, signals in _FAMILY_SIGNALS.items():
        count = sum(1 for s in signals if s in text_lower)
        if count >= _FAMILY_MIN_SIGNALS.get(family, 2):
            family_counts[family] = count
    if not family_counts:
        return None
    return max(family_counts, key=lambda f: family_counts[f])


# ════════════════════════════════════════════════════════════
# RC2 SUPPORT: GENERIC HEADER SPECIFICITY WEIGHT
# ════════════════════════════════════════════════════════════

_GENERIC_HEADERS = frozenset([
    "date", "amount", "balance", "debit", "credit", "description",
    "details", "narration", "particulars", "sl no", "sr no", "no",
    "transaction", "type", "ref", "reference", "value date",
    "chq", "cheque", "dr", "cr",
])


def _header_specificity_multiplier(header: str) -> float:
    h_clean = re.sub(r"[\s().\[\]:]", "", header.lower())
    if h_clean in _GENERIC_HEADERS:
        return 0.3
    first_word = header.lower().split()[0] if header.split() else ""
    if first_word in _GENERIC_HEADERS:
        return 0.5
    return 1.0


# ════════════════════════════════════════════════════════════
# IDENTITY MARKER EVALUATION
# ════════════════════════════════════════════════════════════

def evaluate_identity_markers(
    identity: Dict,
    text: str,
    identifier_json: Optional[Dict] = None,
) -> float:
    text_norm  = re.sub(r"\s+", " ", text.lower())
    total_score = 0.0
    total_max   = 0.0

    if identifier_json:
        excl = identifier_json.get("exclusion_markers", {})
        excl_patterns = excl.get("patterns", []) if isinstance(excl, dict) else (excl or [])
        for pat in excl_patterns:
            if not pat:
                continue
            try:
                if re.search(pat, text, re.I):
                    logger.debug("Exclusion marker '%s' matched → score=0", pat)
                    return 0.0
            except re.error:
                if pat.lower() in text_norm:
                    return 0.0

    inner_excl     = identity.get("exclusion_markers", {})
    inner_patterns = (
        inner_excl.get("patterns", []) if isinstance(inner_excl, dict) else (inner_excl or [])
    )
    for pat in inner_patterns:
        if not pat:
            continue
        try:
            if re.search(pat, text, re.I):
                return 0.0
        except re.error:
            if pat.lower() in text_norm:
                return 0.0

    def process_rule(rule_obj, weight: float = 5.0):
        nonlocal total_score, total_max
        if not rule_obj:
            return
        if isinstance(rule_obj, list) and rule_obj:
            total_max += weight
            if any(isinstance(k, str) and k.lower() in text_norm for k in rule_obj):
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
            if any(p.lower() in text_norm for p in patterns):
                total_score += weight
        elif rule_type == "regex":
            pattern = rule_obj.get("pattern")
            if not pattern:
                return
            total_max += weight
            try:
                if bool(re.search(pattern, text, re.I)):
                    total_score += weight
            except re.error:
                pass

    for section_name, section in identity.items():
        if not isinstance(section, dict):
            continue
        if section_name in ("transaction_table_identity", "exclusion_markers"):
            continue
        for field_name, field_value in section.items():
            field_weight = 15.0 if field_name == "document_title_phrase" else 5.0
            if isinstance(field_value, dict) and "rule" not in field_value:
                for sub_name, sub_value in field_value.items():
                    process_rule(sub_value, weight=field_weight)
            else:
                process_rule(field_value, weight=field_weight)

    table    = identity.get("transaction_table_identity", {})
    headers  = table.get("table_header_markers", [])
    min_req  = table.get("minimum_column_count", 1)

    if headers:
        base_weight      = 30.0
        total_max       += base_weight
        text_compact     = re.sub(r"\s+", "", text.lower())
        per_header_base  = base_weight / len(headers)
        weighted_possible = sum(
            per_header_base * _header_specificity_multiplier(h) for h in headers
        )
        weighted_matched = 0.0
        matched_count    = 0
        for h in headers:
            if re.sub(r"\s+", "", h.lower()) in text_compact:
                matched_count    += 1
                weighted_matched += per_header_base * _header_specificity_multiplier(h)
        header_score = (
            (weighted_matched / weighted_possible) * base_weight
            if weighted_possible > 0 else 0.0
        )
        if matched_count >= min_req:
            total_score += header_score
        else:
            total_score += header_score * 0.5

    footer          = identity.get("footer_identity", {})
    footer_patterns = footer.get("footer_markers", [])
    if footer_patterns:
        weight   = 5.0
        total_max += weight
        matched  = any(
            re.search(p, text, re.I) if "\\" in p else p.lower() in text_norm
            for p in footer_patterns
        )
        if matched:
            total_score += weight

    confidence = round((total_score / total_max) * 100, 2) if total_max else 0.0
    return confidence


# ════════════════════════════════════════════════════════════
# STEP 2 — FORMAT CHECK IN DATABASE
# ════════════════════════════════════════════════════════════

def find_existing_identifier(text: str, default_threshold: float = 80.0):
    categories = get_all_matchable_formats()

    bank_code = extract_ifsc(text)
    if bank_code:
        filtered = [
            c for c in categories
            if c.get("ifsc_code") == bank_code or not c.get("ifsc_code")
        ]
        if filtered:
            categories = filtered

    detected_family = _detect_primary_family(text)

    if detected_family:
        excluded_families = _FAMILY_EXCLUSIONS.get(detected_family, [])
        family_matched    = [c for c in categories if c.get("document_family") == detected_family]
        generic_pool      = [c for c in categories if c.get("document_family") in _GENERIC_FAMILIES]
        excluded_pool     = [c for c in categories if c.get("document_family") in excluded_families]

        if family_matched:
            categories = family_matched + [c for c in generic_pool if c not in family_matched]
        else:
            categories = [c for c in categories if c not in excluded_pool]
    
    doc_clean            = re.sub(r"\s+", "", text[:2000].lower())
    specific_candidates: List[Dict] = []
    generic_candidates:  List[Dict] = []

    for cat in categories:
        inst_name  = (cat.get("institution_name") or "").lower()
        inst_clean = re.sub(r"\s+", "", inst_name)
        doc_family = cat.get("document_family", "")
        is_generic = (
            (not inst_clean or inst_clean == "unknown") and doc_family in _GENERIC_FAMILIES
        )
        if is_generic:
            generic_candidates.append(cat)
            continue
        if inst_clean and inst_clean != "unknown" and inst_clean not in doc_clean:
            continue
        specific_candidates.append(cat)

    from decimal import Decimal

    def score_pool(pool: List[Dict], score_penalty: float = 0.0) -> List[Dict]:
        matches = []
        for cat in pool:
            identifier_json = cat.get("statement_identifier", {})
            if isinstance(identifier_json, str):
                try:
                    identifier_json = json.loads(identifier_json)
                except Exception:
                    identifier_json = {}
            identity       = identifier_json.get("identity_markers", {})
            score          = evaluate_identity_markers(identity, text, identifier_json=identifier_json)
            adjusted_score = max(0.0, score - score_penalty)
            stored_threshold = cat.get("match_threshold")
            if isinstance(stored_threshold, Decimal):
                stored_threshold = float(stored_threshold)
            target_threshold = stored_threshold if stored_threshold is not None else default_threshold
            if adjusted_score >= target_threshold:
                matches.append({
                    "category": cat,
                    "score":    adjusted_score,
                    "status":   cat.get("status"),
                    "is_generic": score_penalty > 0,
                })
        return matches

    matches = score_pool(specific_candidates, score_penalty=0.0)
    if not matches and generic_candidates:
        matches = score_pool(generic_candidates, score_penalty=15.0)

    if not matches:
        return False, None

    def sort_key(m: Dict):
        status_rank      = 0 if m["status"] == "ACTIVE" else 1
        specificity_rank = 1 if m.get("is_generic") else 0
        return (status_rank, specificity_rank, -m["score"], -m["category"].get("statement_id", 0))

    matches.sort(key=sort_key)
    best_match = matches[0]
    return True, best_match["category"]


# ════════════════════════════════════════════════════════════
# STEP 3 — GENERATE IDENTIFICATION MARKERS (LLM)
# NOW ALSO GENERATES parsing_hints BLOCK
# ════════════════════════════════════════════════════════════

def classify_document_llm(reduced: Dict) -> Dict:
    """
    Use Gemini to classify the document and generate granular identity markers
    plus the new parsing_hints block that the code parser needs.
    """

    prompt = f"""
You are a Senior Financial Document Classification Engine.
Return a detailed structural JSON "blueprint" for the following document.
Your task has TWO PARTS:

PART 1 → Classify the financial document
PART 2 → Extract structural identity markers AND parsing hints

Use ONLY the provided text.
Do NOT guess.
If insufficient evidence → classify as UNKNOWN_FINANCIAL_DOCUMENT.

════════════════════════════════════════════
PART 1 — CLASSIFICATION FAMILIES
════════════════════════════════════════════
Choose EXACTLY ONE:
 1. BANK_ACCOUNT_STATEMENT
 2. CREDIT_CARD_STATEMENT
 3. LOAN_STATEMENT
 4. OVERDRAFT_CASH_CREDIT_STATEMENT
 5. WALLET_STATEMENT
 6. PAYMENT_GATEWAY_SETTLEMENT
 7. INVESTMENT_STATEMENT
 8. DEMAT_STATEMENT
 9. TAX_LEDGER_STATEMENT
10. FIXED_DEPOSIT_STATEMENT
11. RECURRING_DEPOSIT_STATEMENT
12. INSURANCE_POLICY_STATEMENT
13. PENSION_STATEMENT
14. BROKERAGE_CONTRACT_NOTE
15. FOREX_STATEMENT
16. ESCROW_STATEMENT
17. GENERIC_STATEMENT_OF_ACCOUNT

════════════════════════════════════════════
⚠️  CLASSIFICATION CONFLICT RESOLUTION
════════════════════════════════════════════

STOP 1 — LOAN_STATEMENT
  If Loan Account Number / EMI / Outstanding Principal / Repayment Schedule → LOAN_STATEMENT

STOP 2 — CREDIT_CARD_STATEMENT
  If Masked card number / Minimum Amount Due / Credit Limit / Cash Advance Limit → CREDIT_CARD_STATEMENT

STOP 3 — DEMAT_STATEMENT
  If DP ID / BO ID / ISIN column / NSDL / CDSL → DEMAT_STATEMENT

STOP 4 — OVERDRAFT_CASH_CREDIT_STATEMENT
  If TWO OR MORE of: Drawing Power / Sanctioned Limit / CC Limit / OD Account → OVERDRAFT

STOP 5 — TAX_LEDGER_STATEMENT
  If GSTIN + Tax Period + CGST/SGST → TAX_LEDGER_STATEMENT

STOP 7 — BANK_ACCOUNT_STATEMENT (only after all stops clear)
  Account Number + (IFSC or Branch) + transaction ledger with running balance

════════════════════════════════════════════
DOCUMENT TEXT SAMPLE
════════════════════════════════════════════

FIRST PAGE:
""" + reduced["first_page"] + """

SECOND PAGE (first transactions):
""" + reduced.get("second_page", "") + """

LAST PAGE:
""" + reduced["last_page"] + """

TABLE HEADERS DETECTED:
""" + json.dumps(reduced["headers"]) + """

════════════════════════════════════════════
⚠️  CRITICAL RULES FOR IDENTITY MARKERS
════════════════════════════════════════════

RULE 1 — USE EXCLUSIVE SIGNALS ONLY
RULE 2 — document_title_phrase: copy EXACT title from top of document
RULE 3 — exclusion_markers: 3-5 terms from SIMILAR but DIFFERENT families
RULE 4 — table_header_markers: ALL visible column headers, prefer distinctive ones

════════════════════════════════════════════
⚠️  RULES FOR parsing_hints  (NEW — MANDATORY)
════════════════════════════════════════════

parsing_hints captures HOW the document's raw extracted text is structured.
Every field is MANDATORY. Use null only where genuinely not applicable.

FIELD 1 — layout_type
  Examine whether the document has a two-column layout on any page:
  - "TWO_COLUMN_PDF"  : account summary block (Total Due, Min Due, Credit Limit etc.)
                        appears LEFT of the transaction table on the same page.
                        When PDF text is extracted, these two columns get INTERLEAVED.
                        → Use this for RBL, HDFC, SBI credit cards.
  - "SINGLE_COLUMN"   : transaction table is the only major content on each page.
                        → Use this for YES Bank, Axis, ICICI credit cards.
  - "MULTI_SECTION"   : multiple distinct tables on one page (e.g. bank passbook).

FIELD 2 — summary_section_labels
  List ALL label strings that mark a line as an account-summary row, NOT a transaction.
  These are the left-column label names in a two-column layout, or section headings.
  Examples: "Total Amount Due", "Min. Amt. Due", "Payment Due Date",
            "Available Credit Limit", "Opening Balance", "Closing Balance"
  → Include every label you can see. These will be used to pre-filter garbage rows.

FIELD 3 — transaction_boundary_signals
  What signals a NEW transaction row when the PDF text is linearized?
  Always include "DATE". Add others if applicable:
  - "UPI_PREFIX"  : transactions start with "UPI_" on a new line (YES Bank style)
  - "REF_NO_RT"   : each transaction has a "RT" + 18-digit ref number (YES Bank style)
  - "NEFT_PREFIX" : transactions start with "NEFT/"
  - "CHQ_PREFIX"  : transactions start with "CHQ/"
  → IMPORTANT: If transactions can span multiple lines WITHOUT a date on the continuation
    line, you MUST add the appropriate signal so the code parser can detect the boundary.

FIELD 4 — ref_no_pattern
  If transaction rows contain a reference number that should be STRIPPED from the
  details/description field, provide a regex to match it.
  Examples:
  - YES Bank: "-?\\s*Ref No:\\s*RT\\w+"   strips "- Ref No: RT260190390001770000728"
  - HDFC:     "Ref\\s*No\\.?\\s*\\d+"
  - null if no ref numbers appear in the description column.

FIELD 5 — page_break_pattern
  A regex that reliably matches the page-number line at the bottom of each page.
  This is used to exit table mode at page end and re-enter on the next page header.
  Standard: "Page \\\\d+ of \\\\d+"
  If the document uses a different format (e.g. "Page 1/5" or "- 1 -"), adjust.

FIELD 6 — details_strip_patterns
  List of regex patterns to strip from the extracted details/description field.
  Include:
  - The ref_no_pattern again if it needs stripping from details
  - Merchant category values (e.g. "Miscellaneous Stores", "Retail Outlet Services")
  - Any other trailing noise that appears after the merchant name in the raw text
  These are applied sequentially after raw description extraction.

FIELD 7 — known_summary_amounts
  List the EXACT amount strings (as they appear in the document, with commas)
  for account-summary values that must NEVER be used as transaction amounts.
  Examples: "10,146.00" (Total Amount Due), "1,552.00" (Minimum Due),
            "2,03,000.00" (Credit Limit), "51.08" (GST total)
  → Leave as [] if amounts change every statement (not hardcodable) or if
    the layout_type is SINGLE_COLUMN and there is no interleaving risk.

════════════════════════════════════════════
STRICT RULES:
- No guessing. No explanation. No markdown.
- Return STRICT valid JSON only.
- Use null where field not present.
- parsing_hints is MANDATORY — never omit it.

STATEMENT VERSIONING:
- ID format: [document_family]_[document_subtype]_[VERSION]
- document_subtype is the ACCOUNT TYPE (e.g. Savings, Current, Salary, Platinum Card, Home Loan)
- document_subtype is NOT the institution name (never use HDFC, SBI, ICICI etc. in the ID)
- VERSION always starts at V1
- Example: BANK_ACCOUNT_STATEMENT_SAVINGS_V1, CREDIT_CARD_STATEMENT_PLATINUM_V1
════════════════════════════════════════════
PART 2 — RETURN THIS EXACT JSON STRUCTURE
════════════════════════════════════════════
{{
  "id": "UNIQUE_ID_V1",
  "document_family": "<chosen family>",
  "document_subtype": "<e.g. Savings, Current, Platinum Card>",
  "institution_name": "<detected institution>",
  "country": "India",
  "confidence_score": 0.95,

  "exclusion_markers": {{
    "patterns": ["...", "...", "..."]
  }},

  "parsing_hints": {{
    "layout_type": "<SINGLE_COLUMN | TWO_COLUMN_PDF | MULTI_SECTION>",
    "summary_section_labels": [
      "<label string that marks an account-summary line, not a transaction>"
    ],
    "transaction_boundary_signals": ["DATE"],
    "ref_no_pattern": "<regex to match and strip ref numbers, or null>",
    "page_break_pattern": "Page \\\\d+ of \\\\d+",
    "details_strip_patterns": [
      "<regex pattern to strip from details field>"
    ],
    "known_summary_amounts": [
      "<exact amount string that is a summary value, never a transaction>"
    ]
  }},

  "identity_markers": {{
    "issuer_identity": {{
      "issuer_name": {{ "rule": "keyword", "patterns": [] }},
      "regulatory_identifiers": {{
        "ifsc": {{ "rule": "regex", "pattern": null }},
        "swift": {{ "rule": "regex", "pattern": null }},
        "iban": {{ "rule": "regex", "pattern": null }},
        "gstin": {{ "rule": "regex", "pattern": null }},
        "other": []
      }}
    }},
    "document_structure_identity": {{
      "document_title_phrase": {{
        "rule": "keyword",
        "patterns": ["<EXACT title phrase>"]
      }},
      "document_reference_number": {{ "rule": "regex", "pattern": null }},
      "generation_phrase": {{ "rule": "keyword", "patterns": [] }}
    }},
    "period_identity": {{
      "statement_period": {{ "rule": "regex", "pattern": null }},
      "statement_date":   {{ "rule": "regex", "pattern": null }},
      "billing_cycle":    {{ "rule": "regex", "pattern": null }},
      "tax_period":       {{ "rule": "regex", "pattern": null }}
    }},
    "entity_identity": {{
      "account_number":      {{ "rule": "regex", "pattern": null }},
      "masked_card_number":  {{ "rule": "regex", "pattern": null }},
      "loan_account_number": {{ "rule": "regex", "pattern": null }},
      "customer_id":         {{ "rule": "regex", "pattern": null }},
      "wallet_id":           {{ "rule": "regex", "pattern": null }},
      "merchant_id":         {{ "rule": "regex", "pattern": null }},
      "pan":                 {{ "rule": "regex", "pattern": null }},
      "bo_id":               {{ "rule": "regex", "pattern": null }},
      "dp_id":               {{ "rule": "regex", "pattern": null }}
    }},
    "transaction_table_identity": {{
      "table_header_markers": ["<ALL visible column headers>"],
      "minimum_column_count": 0,
      "presence_of_running_balance": false,
      "debit_credit_style": false
    }},
    "financial_summary_identity": {{
      "total_outstanding": {{ "rule": "regex", "pattern": null }},
      "minimum_due":       {{ "rule": "regex", "pattern": null }},
      "emi_amount":        {{ "rule": "regex", "pattern": null }},
      "credit_limit":      {{ "rule": "regex", "pattern": null }},
      "drawing_power":     {{ "rule": "regex", "pattern": null }},
      "portfolio_value":   {{ "rule": "regex", "pattern": null }},
      "total_tax":         {{ "rule": "regex", "pattern": null }}
    }},
    "footer_identity": {{
      "footer_markers": []
    }}
  }}
}}
════════════════════════════════════════════
OUTPUT RULES: Return ONLY the JSON object. No markdown. No explanations.
"""

    response = call_with_retry(
        client, GEMINI_MODEL_NAME, prompt,
        config={"temperature": 0},
    )

    raw = response.text.strip()

    def sanitize_json(s: str) -> str:
        s = re.sub(r"```(?:json)?", "", s)
        start = s.find("{")
        end   = s.rfind("}")
        if start == -1 or end == -1:
            return s
        s = s[start:end + 1]
        s = re.sub(r",\s*([\]}])", r"\1", s)
        s = re.sub(r":\s*True\b",  ": true",  s)
        s = re.sub(r":\s*False\b", ": false", s)
        s = re.sub(r":\s*None\b",  ": null",  s)
        if s.count("{") > s.count("}"):
            s += "}" * (s.count("{") - s.count("}"))
        return s

    try:
        clean      = sanitize_json(raw)
        identifier = json.loads(clean)
    except Exception as e:
        logger.error("Failed to parse classification JSON. Error: %s. Raw: %s", e, raw)
        match = re.search(r"(\{.*\})", raw, re.DOTALL)
        if match:
            try:
                identifier = json.loads(sanitize_json(match.group(1)))
            except Exception:
                raise ValueError(f"LLM classification returned invalid JSON: {e}")
        else:
            raise ValueError(f"LLM classification returned no JSON-like content: {e}")

    # ── Validate parsing_hints block is present ───────────────────────────────
    if "parsing_hints" not in identifier:
        logger.warning(
            "LLM did not return parsing_hints block — injecting safe defaults."
        )
        identifier["parsing_hints"] = {
            "layout_type":                 "SINGLE_COLUMN",
            "summary_section_labels":      [],
            "transaction_boundary_signals":["DATE"],
            "ref_no_pattern":              None,
            "page_break_pattern":          r"Page \d+ of \d+",
            "details_strip_patterns":      [],
            "known_summary_amounts":       [],
        }
    else:
        # Ensure all keys exist with safe defaults
        ph = identifier["parsing_hints"]
        ph.setdefault("layout_type",                 "SINGLE_COLUMN")
        ph.setdefault("summary_section_labels",      [])
        ph.setdefault("transaction_boundary_signals",["DATE"])
        ph.setdefault("ref_no_pattern",              None)
        ph.setdefault("page_break_pattern",          r"Page \d+ of \d+")
        ph.setdefault("details_strip_patterns",      [])
        ph.setdefault("known_summary_amounts",       [])

    logger.info(
        "LLM classified: family=%s, institution=%s, layout=%s, boundaries=%s",
        identifier.get("document_family"),
        identifier.get("institution_name"),
        identifier.get("parsing_hints", {}).get("layout_type"),
        identifier.get("parsing_hints", {}).get("transaction_boundary_signals"),
    )

    return identifier


# ════════════════════════════════════════════════════════════
# SAVE NEW FORMAT
# ════════════════════════════════════════════════════════════

def derive_statement_type(identifier_json: dict) -> str:
    family   = identifier_json.get("document_family", "UNKNOWN")
    type_map = {
        "BANK_ACCOUNT_STATEMENT":          "BANK_STATEMENT",
        "CREDIT_CARD_STATEMENT":           "CREDIT_CARD",
        "LOAN_STATEMENT":                  "LOAN",
        "WALLET_STATEMENT":                "WALLET",
        "INVESTMENT_STATEMENT":            "INVESTMENT",
        "DEMAT_STATEMENT":                 "DEMAT",
        "TAX_LEDGER_STATEMENT":            "TAX_LEDGER",
        "PAYMENT_GATEWAY_SETTLEMENT":      "PAYMENT_GATEWAY",
        "OVERDRAFT_CASH_CREDIT_STATEMENT": "OD_CC",
    }
    return type_map.get(family, family)


def save_new_statement_format(
    format_name: str,
    identifier_json: dict,
    extraction_logic: str,
    threshold: float = 65.0,
) -> int:
    statement_type   = derive_statement_type(identifier_json)
    institution_name = identifier_json.get("institution_name", "Unknown")
    logger.info(
        "Saving new format: name=%s type=%s institution=%s",
        format_name, statement_type, institution_name,
    )
    return insert_statement_category(
        statement_type=statement_type,
        format_name=format_name,
        institution_name=institution_name,
        identifier_json=identifier_json,
        extraction_logic=extraction_logic,
        threshold=threshold,
    )