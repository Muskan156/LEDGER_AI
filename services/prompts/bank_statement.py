
import re

# def build_prompt(identifier_json: dict, text_sample: str) -> str:
#     import re as _re

#     # ------------------------------------------------------------------ #
#     #  EXTRACT ALL FIELDS FROM IDENTIFICATION JSON
#     # ------------------------------------------------------------------ #
    # family      = identifier_json.get("document_family", "BANK_ACCOUNT_STATEMENT")
#     subtype     = identifier_json.get("document_subtype", "Unknown")
#     institution = identifier_json.get("institution_name", "Unknown")
#     country     = identifier_json.get("country", "India")

#     identity    = identifier_json.get("identity_markers", {})

#     issuer      = identity.get("issuer_identity", {})
#     ifsc_pat    = issuer.get("regulatory_identifiers", {}).get("ifsc", {}).get("pattern", None)

#     doc_struct  = identity.get("document_structure_identity", {})
#     doc_titles  = doc_struct.get("document_title_phrase", {}).get("patterns", [])

#     period      = identity.get("period_identity", {})
#     stmt_period = period.get("statement_period", {}).get("pattern", None)

#     entity      = identity.get("entity_identity", {})
#     acct_pat    = entity.get("account_number", {}).get("pattern", None)

#     # --- Transaction table (THE KEY SECTION) ---
#     txn_table   = identity.get("transaction_table_identity", {})
#     headers     = txn_table.get("table_header_markers", [])
#     min_cols    = txn_table.get("minimum_column_count", 0)
#     has_balance = txn_table.get("presence_of_running_balance", False)
#     dc_style    = txn_table.get("debit_credit_style", False)

#     footer      = identity.get("footer_identity", {})
#     footers     = footer.get("footer_markers", [])

#     fin_summary = identity.get("financial_summary_identity", {})

#     # ------------------------------------------------------------------ #
#     #  DERIVE COLUMN PROPERTIES FROM HEADERS
#     # ------------------------------------------------------------------ #
#     headers_lower = [h.lower().strip() for h in headers]

#     def _has_col(*candidates):
#         return any(any(c in h for h in headers_lower) for c in candidates)

#     has_serial_col    = _has_col("sr no", "sr.no", "s no", "s.no", "#", "serial")
#     has_value_date    = _has_col("value date", "val date", "post date")
#     has_txn_date      = _has_col("txn date", "transaction date", "date")
#     has_chq_ref       = _has_col("chq", "ref", "cheque", "reference", "voucher")
#     has_tran_type     = _has_col("tran type", "txn type", "type", "transaction type", "tran_type")
#     has_cheque_detail = _has_col("cheque detail", "chq detail", "cheque no")
#     has_withdrawal    = _has_col("withdrawal", "withdraw", "debit amt", "dr amount", "dr amt")
#     has_deposit       = _has_col("deposit", "credit amt", "cr amount", "cr amt")
#     has_debit_col     = _has_col("debit") and not _has_col("debit amt", "dr amount")
#     has_credit_col    = _has_col("credit") and not _has_col("credit amt", "cr amount")
#     has_dr_cr_col     = _has_col("dr/cr", "dr cr", "d/c")
#     has_balance_col   = _has_col("balance", "closing balance", "running balance")
#     has_narration_col = _has_col("narration", "particulars", "description", "details",
#                                   "remarks", "transaction details")

#     dual_date_mode = has_value_date and has_txn_date

#     # ------------------------------------------------------------------ #
#     # FIX 1 (dc_strategy): Two-part fix for correct strategy selection.
#     #
#     # Part A -- Boolean normalization:
#     #   identifier_json may set "debit_credit_style": true (JSON boolean) instead
#     #   of a recognised string like "single_amount_suffix".  Python receives this
#     #   as dc_style=True.  True is truthy, so every `not dc_style` guard (used to
#     #   enable structural column inference) evaluates to False -- all structural
#     #   checks are skipped and the strategy falls through to DELTA_FALLBACK, which
#     #   is wrong.  Fix: normalise any non-string dc_style to None so the structural
#     #   inference path runs correctly.
#     #
#     # Part B -- Priority order:
#     #   has_dr_cr_col MUST be checked BEFORE has_withdrawal+has_deposit.
#     #   Federal Bank has BOTH Withdrawals AND Deposits columns in its header, PLUS
#     #   a Dr/Cr column.  The Dr/Cr column is the authoritative direction signal;
#     #   the Withdrawals/Deposits columns are layout artefacts (only one is non-zero
#     #   per row).  Old code triggered WITHDRAWAL_DEPOSIT because it tested
#     #   has_withdrawal+has_deposit first.  Fixed priority:
#     #   1. separate_columns / has_debit_col+has_credit_col -> SEPARATE_COLUMNS
#     #   2. single_amount_suffix / eol_marker / has_dr_cr_col -> AMOUNT_WITH_DR_CR
#     #   3. two_amount_columns / has_withdrawal+has_deposit    -> WITHDRAWAL_DEPOSIT
#     #   4. txn_type_column / has_tran_type                   -> AMOUNT_WITH_TYPE_COL
#     #   5. fallback                                           -> DELTA_FALLBACK
#     # ------------------------------------------------------------------ #
#     # Part A: normalise boolean dc_style to None so string comparisons work correctly
#     if not isinstance(dc_style, str):
#         dc_style = None

#     if dc_style == "separate_columns" or (dc_style is None and has_debit_col and has_credit_col):
#         dc_strategy = "SEPARATE_COLUMNS"
#     elif dc_style in ("single_amount_suffix", "eol_marker") or (dc_style is None and has_dr_cr_col):
#         dc_strategy = "AMOUNT_WITH_DR_CR"
#     elif dc_style == "two_amount_columns" or (dc_style is None and has_withdrawal and has_deposit):
#         dc_strategy = "WITHDRAWAL_DEPOSIT"
#     elif dc_style == "txn_type_column" or (dc_style is None and has_tran_type):
#         dc_strategy = "AMOUNT_WITH_TYPE_COL"
#     else:
#         dc_strategy = "DELTA_FALLBACK"

#     # ------------------------------------------------------------------ #
#     #  COUNT NON-MONEY COLUMNS (critical for Federal Bank)
#     # ------------------------------------------------------------------ #
#     noise_col_count = sum([
#         1 if has_tran_type     else 0,
#         1 if has_cheque_detail else 0,
#         1 if has_chq_ref       else 0,
#     ])

#     noise_strip_patterns = []
#     if has_tran_type:
#         noise_strip_patterns.append(
#             r'\b(TFR|CLG|FT|MB|ATM|CASH|NACH|NEFT|IMPS|UPI|RTGS|ECS|ACH|BBPS|SWIFT)\b'
#         )
#     if has_cheque_detail or has_chq_ref:
#         noise_strip_patterns.append(r'\b0{3,}\d{4,}\b')
#         noise_strip_patterns.append(r'(?<!\w)-(?!\w)')

#     noise_patterns_repr = repr(noise_strip_patterns)

#     # ------------------------------------------------------------------ #
#     #  BUILD COLUMN LAYOUT DESCRIPTION
#     # ------------------------------------------------------------------ #
#     col_layout_lines = [
#         f"Detected columns ({len(headers)} total): {headers}",
#         f"Serial number column      : {has_serial_col}",
#         f"Dual date columns         : {dual_date_mode}  (use FIRST date as txn date)",
#         f"Tran Type column present  : {has_tran_type}   <- strip tokens before money parse",
#         f"Cheque/Ref column present : {has_chq_ref or has_cheque_detail}   <- strip ref numbers before money parse",
#         f"DR/CR column present      : {has_dr_cr_col}",
#         f"Running balance present   : {has_balance_col}",
#         f"Noise column count        : {noise_col_count}  (non-money cols between narration and amounts)",
#         f"Debit/Credit strategy     : {dc_strategy}",
#         f"Debit/Credit style (raw)  : {dc_style}",
#         "",
#     ]

#     if dc_strategy == "SEPARATE_COLUMNS":
#         col_layout_lines += [
#             "DEBIT/CREDIT RULE -- SEPARATE_COLUMNS:",
#             "  Debit col and Credit col are independent. Only one is non-zero per row.",
#             "  Use balance DELTA as primary: delta>0 -> credit, delta<0 -> debit.",
#             "  Fallback to keyword detection when prev_balance is None.",
#             "  IMPORTANT for SBI: ₹ symbol appears in header ('₹ Debit', '₹ Credit').",
#             "  After ₹ stripping in Phase 1, header becomes 'Debit Credit Balance'.",
#             "  The skip pattern must NOT match SBI's data rows -- only the header row.",
#             "  SBI data rows have a date in column 1, so line_starts_transaction() protects them.",
#         ]
#     elif dc_strategy == "WITHDRAWAL_DEPOSIT":
#         col_layout_lines += [
#             "DEBIT/CREDIT RULE -- WITHDRAWAL_DEPOSIT:",
#             "  Two amount cols: Withdrawal (debit) and Deposit (credit), then Balance.",
#             "  Use delta to confirm direction. If delta>0 -> credit, delta<0 -> debit.",
#         ]
#     elif dc_strategy == "AMOUNT_WITH_DR_CR":
#         col_layout_lines += [
#             "DEBIT/CREDIT RULE -- AMOUNT_WITH_DR_CR:",
#             "  Single amount col. Direction from: (1) suffix on amount, (2) Dr/Cr col at EOL.",
#             "  Federal Bank: 'Dr'/'Cr' appears as the LAST token on every transaction line.",
#             "  CRITICAL: Strip TFR/cheque-detail columns BEFORE parsing money values.",
#             "  Federal layout per line: DATE  VALDATE  NARRATION  TFR  000  AMOUNT  BALANCE  Dr",
#             "  After stripping TFR and 000, money_vals = [amount, balance]. This is correct.",
#         ]

#     if has_tran_type or has_cheque_detail or has_chq_ref:
#         col_layout_lines += [
#             "",
#             "NOISE COLUMN STRIPPING (CRITICAL):",
#             f"  Apply these patterns to CLEAN LINE before money parsing AND before details extraction:",
#             f"  noise_patterns = {noise_strip_patterns}",
#             "  Strip them in order. This prevents TFR/000/ref-numbers being parsed as amounts.",
#             "  Also strip them from the details field so they do not pollute narration text.",
#         ]

#     if dual_date_mode:
#         col_layout_lines += [
#             "",
#             "DUAL DATE: Each line has TWO dates (txn date + value date).",
#             "Always use re.findall(DATE_REGEX, line)[0] -- the FIRST date is the transaction date.",
#             "The second date must NOT trigger a new transaction block.",
#         ]

#     if has_serial_col:
#         col_layout_lines += [
#             "",
#             "SERIAL COL: Strip leading integer from details: r'^\\s*\\d{1,4}\\s+'",
#         ]

#     col_layout_description = "\n".join(col_layout_lines)

#     # ------------------------------------------------------------------ #
#     #  FOOTER / HARD-STOP SECTION
#     #
#     # FIX 3 (footer marker classification):
#     #   identifier_json footer_markers may contain address/contact strings like
#     #   "Federal Towers, Market Rd, Periyar Nagar" that appear in the page footer
#     #   of EVERY page (not just the last one).  Using these as hard-stop triggers
#     #   causes is_hard_stop() to fire on page 1 footer line, killing the main
#     #   loop before page 2 transactions are processed (explains 12/14 for Federal).
#     #
#     #   Fix: split footer_markers into two buckets at build time:
#     #   - page-level markers (address/contact) -> added to SKIP_PATTERNS
#     #   - terminal markers                     -> used in is_hard_stop() only
#     #
#     #   A marker is page-level if it contains address/contact keywords
#     #   (road, nagar, tower, phone, www, PIN codes, state names, etc.).
#     # ------------------------------------------------------------------ #
#     _PAGE_LEVEL_RE = _re.compile(
#         r"""(?ix)
#         \b(road|rd\.?|street|st\.?|nagar|colony|towers?|house|floor|block|plot
#             |phone|tel|fax|mobile|www\.
#             |kerala|maharashtra|mumbai|delhi|bangalore|chennai|hyderabad|ernakulam
#             |aluva|market)\b
#         |\b\d{6}\b
#         |\d{3,}[-\s]\d{3,}"""
#     )
#     _terminal_footer_markers   = []
#     _page_level_footer_markers = []
#     for _fm in set(footers):
#         if _PAGE_LEVEL_RE.search(_fm):
#             _page_level_footer_markers.append(_fm)
#         else:
#             _terminal_footer_markers.append(_fm)

#     all_footer_markers      = _terminal_footer_markers    # used in is_hard_stop()
#     page_level_skip_markers = _page_level_footer_markers  # injected into SKIP_PATTERNS

#     summary_labels = []
#     for k, v in fin_summary.items():
#         if v and isinstance(v, dict) and v.get("pattern"):
#             summary_labels.append(v["pattern"])

#     if headers:
#         anchor_headers = [_re.escape(h.strip()) for h in headers[:3] if h.strip()]
#         header_skip_pat = "".join(f"(?=.*{h})" for h in anchor_headers)
#         header_row_pat = f'r"(?i){header_skip_pat}."'
#     else:
#         header_row_pat = r'r"(?i)(date|narration|particulars)\s*$"'

#     OCR_SPLIT_RE   = r"(?<![,\d])(\d{1,3}) (\d{3}\.\d{2})(?!\d)"
#     OCR_SPLIT_REPL = r"\1\2"


#     return f"""
# You are a Senior Python Backend Engineer specializing in Financial Ledger Parsing.

# ============================================================
# CRITICAL OBJECTIVE
# ============================================================
# Generate EXACTLY one deterministic Python function:

#     def extract_transactions(text: str) -> list:

# Rules:
# - Import re inside the function (no other imports needed)
# - Wrap the ENTIRE function body in try/except -- never crash
# - Never use LLM or external calls
# - Return list of dicts, empty list if nothing found

# ============================================================
# DOCUMENT CONTEXT
# ============================================================
# Institution   : {institution}
# Family        : {family}
# Subtype       : {subtype}
# Country       : {country}
# Account Pat   : {acct_pat}
# IFSC Pattern  : {ifsc_pat}
# Doc Titles    : {doc_titles}
# Stmt Period   : {stmt_period}

# ============================================================
# COLUMN LAYOUT  (from transaction_table_identity)
# ============================================================
# {col_layout_description}

# ============================================================
# PHASE 1 -- PREPROCESSING
# ============================================================

#     try:
#         text = text.replace("\\u00A0", " ").replace("\\xa0", " ")

#         raw_lines = [line.rstrip() for line in text.splitlines()]

#         def clean_line(line):
#             line = line.replace("₹", "").replace("Rs.", "").replace("INR", "")
#             line = line.replace("\\u20b9", "")  # Unicode ₹
#             # Normalize OD (overdraft) -> negative
#             line = re.sub(r'(\\d+\\.\\d{{2}})\\s*OD\\b', r'-\\1', line)
#             # Normalize parenthetical (Dr)/(Cr) suffixes
#             line = re.sub(r'(\\d+\\.\\d{{2}})\\s*\\(Dr\\)', r'\\1DR', line, flags=re.IGNORECASE)
#             line = re.sub(r'(\\d+\\.\\d{{2}})\\s*\\(Cr\\)', r'\\1CR', line, flags=re.IGNORECASE)
#             return line

#     except Exception:
#         raw_lines = []
#         def clean_line(line): return line

# ============================================================
# PHASE 2 -- SKIP & HARD-STOP PATTERNS
# ============================================================

#     def is_header_row(raw_line):
#         \"\"\"
#         Detects the column header row using raw text (before ₹ stripping).
#         RULES:
#           1. Requires 3+ column header words on the same line (not just 2).
#           2. A line that STARTS WITH A DATE can never be a header row.
#         \"\"\"
#         try:
#             DATE_REGEX_CHECK = r'^\\s*\\d{{1,2}}[-/ ](?:\\d{{1,2}}|[A-Za-z]{{3,9}})[-/ ]\\d{{2,4}}'
#             if re.search(DATE_REGEX_CHECK, raw_line.strip(), re.IGNORECASE):
#                 return False
#             col_hits = sum(1 for h in {headers!r}
#                            if h.lower() in raw_line.lower())
#             return col_hits >= 3
#         except Exception:
#             return False

#     SKIP_PATTERNS = [
#         r'^\\s*$',
#         r'(?i)^\\s*(opening\\s*balance|b/?f|brought\\s*forward|carried\\s*forward|c/?f)',
#         r'(?i)(page\\s*\\d+\\s*(of\\s*\\d+)?|page\\s*no\\.?\\s*:?\\s*\\d+)',
#         r'(?i)(statement\\s*of\\s*account|account\\s*summary)',
#         r'(?i)^\\s*(sr\\.?\\s*no\\.?|s\\.?\\s*no\\.?)\\s+(date|txn)',
#         r'(?i)(balance\\s*c/?f|balance\\s*carried\\s*forward)',
#         r'[\\u0900-\\u097F]{{3,}}',
#         r'(?i)^\\s*-+\\s*$',
#         r'(?i)(any\\s*discrepancy)',
#         r'(?i)(please\\s*do\\s*not\\s*share)',
#         r'(?i)(abbreviations?\\s*used)',
#         r'(?i)(24\\s*\\*?\\s*7\\s*phone\\s*banking)',
#         r'(?i)(www\\.[a-z]+\\.co\\.in|www\\.[a-z]+bank\\.com)',
#         r'(?i)^\\s*(the\\s+)?federal\\s+bank\\s+ltd',
#         r'(?i)corporate\\s+office.*federal',
#         # Federal Bank inter-page summary lines -- appear BETWEEN pages of a multi-page
#         # statement, before remaining transactions on the next page.
#         # "Statement Summary" bare (no ":-") -> skip, keep looping for next page txns
#         r'(?i)^\\s*statement\\s*summary\\s*$',
#         # "Total Withdrawals / Total Deposits" per-page subtotals -> skip, not a true end
#         r'(?i)^\\s*total\\s*(withdrawals?|deposits?)\\s*[:\\s]',
#         # Page-level footer address markers (e.g. "Federal Towers, Market Rd, Periyar Nagar")
#         # These appear in EVERY page footer, not just the last page.
#         # MUST be in SKIP (not hard-stop) so multi-page statements process all pages.
#         # Generated from identifier_json footer_markers containing address/contact keywords.
#     ] + [re.escape(m) for m in {page_level_skip_markers!r}]

#     FOOTER_MARKERS = {all_footer_markers!r}

#     HARD_STOP_PATTERNS = [
#         # FIX: "Statement Summary" alone (no ":-") is an INTER-PAGE summary in
#         # multi-page Federal Bank statements -- it appears between page 1 and page 2
#         # BEFORE the remaining transactions.  Using it as a hard-stop here kills the
#         # loop too early (12/14 transactions extracted instead of 14/14).
#         # RULE: only treat as hard-stop when followed by ":-" (HDFC style true-end marker).
#         # The bare "Statement Summary" line is handled by the SKIP entry below.
#         r'(?i)statement\\s*summary\\s*:-',
#         r'(?i)(generated\\s*on.*generated\\s*by)',
#         r'(?i)(end\\s*of\\s*statement)',
#         r'(?i)(grand\\s*total)',
#         r'(?i)(brought\\s*forward.*dr\\s*count|dr\\s*count.*cr\\s*count)',
#         # FIX: "Total Withdrawals" / "Total Deposits" are PER-PAGE summary lines in
#         # Federal Bank multi-page statements; moved to SKIP_PATTERNS below so the loop
#         # continues after the inter-page summary block.
#         # Keep "Total Debits" and "Total Credits" as hard-stops for other banks.
#         r'(?i)(total\\s*debits?\\b|total\\s*credits?\\b)',
#         r'(?i)(this\\s*is\\s*a\\s*computer)',
#     ]

#     HARD_STOP_PATTERNS.append(r'(?i)closing\\s*balance\\s*[:\\s]*$')
#     HARD_STOP_PATTERNS.append(r'(?i)account\\s+statement\\s*\\d{{2}}')
#     HARD_STOP_PATTERNS.append(r'(?i)end\\s+of\\s+statement')
#     HARD_STOP_PATTERNS.append(r'(?i)savings\\s+account\\s*\\(sa\\)')
#     HARD_STOP_PATTERNS.append(r'(?i)any\\s+discrepancy\\s+in\\s+the\\s+statement')

#     def is_hard_stop(clean_line):
#         try:
#             for pat in HARD_STOP_PATTERNS:
#                 if re.search(pat, clean_line):
#                     return True
#             stripped = clean_line.strip().lower()
#             if any(m.lower() in stripped for m in FOOTER_MARKERS if m):
#                 return True
#         except Exception:
#             pass
#         return False

#     def should_skip(clean_line):
#         try:
#             stripped = clean_line.strip()
#             if not stripped:
#                 return True
#             for pat in SKIP_PATTERNS:
#                 if re.search(pat, stripped):
#                     return True
#         except Exception:
#             pass
#         return False

# ============================================================
# PHASE 3 -- DATE REGEX
# ============================================================

#     DATE_REGEX = r'\\b(\\d{{1,2}}[-/ ](?:\\d{{1,2}}|[A-Za-z]{{3,9}})[-/ ]\\d{{2,4}})\\b'

#     def extract_first_date(line):
#         try:
#             return re.findall(DATE_REGEX, line, re.IGNORECASE)[0]
#         except Exception:
#             return None

#     def line_starts_transaction(raw_line):
#         \"\"\"
#         Check the RAW line for a date near the start.
#         Window size accounts for optional serial number prefix.
#         Serial col present: {has_serial_col} -> window = {'35' if has_serial_col else '25'} chars.
#         Dual date present : {dual_date_mode} -> only the FIRST date match counts.
#         \"\"\"
#         try:
#             window = raw_line.strip()[:{35 if has_serial_col else 25}]
#             return bool(re.search(DATE_REGEX, window, re.IGNORECASE))
#         except Exception:
#             return False

# ============================================================
# PHASE 4 -- NOISE COLUMN STRIPPING  <- CRITICAL FOR FEDERAL BANK
# ============================================================

#     NOISE_PATTERNS = {noise_patterns_repr}
#     NOISE_PATTERNS = NOISE_PATTERNS + [
#         r'\\b[A-Z]{{3,6}}\\d{{9,16}}\\b',
#     ]
#     def strip_noise_columns(line):
#         \"\"\"
#         Strips transaction-type tokens and zero-padded reference numbers.
#         Apply BEFORE money parsing AND before building details string.
#         NOTE: Does NOT strip dates -- date stripping for money context is
#         handled inside parse_money() using the strict date regex.
#         \"\"\"
#         try:
#             for pat in NOISE_PATTERNS:
#                 line = re.sub(pat, ' ', line, flags=re.IGNORECASE)
#             line = re.sub(r'(?<![\\w\\d])-(?![\\w\\d\\.])', ' ', line)
#             line = re.sub(r'\\s{{2,}}', ' ', line).strip()
#         except Exception:
#             pass
#         return line

# ============================================================
# PHASE 5 -- MONEY REGEX & SAFE MONEY PARSING
# ============================================================

#     # FIX 2 (HDFC value-date "26" leak + Federal balance truncation):
#     #
#     # ROOT CAUSE: The original parse_money applied OCR-split repair directly on
#     # text that still contained dates like "13/01/26". When PDF extraction splits
#     # a date across a line boundary ("13/01/" on one line, "26" on the next),
#     # the "26" appears isolated after joining block lines. The OCR repair regex
#     # then merges "26 871.26" -> "26,871.26", producing debit=26871.26.
#     #
#     # A secondary bug: using the space-inclusive DATE_REGEX inside parse_money
#     # to strip dates BEFORE repair caused "00 60 530" inside "113.00 60 530.51"
#     # to be falsely identified as a date and stripped, leaving "113. .51" (invalid).
#     # This produced empty money_vals for Federal Bank, causing balance=530.51 only
#     # on the rare OCR-split path.
#     #
#     # THE FIX (implemented below):
#     # 1. Use DATE_REGEX_STRICT ([-/] only, NO space separator) to strip complete dates.
#     #    This prevents "00 60 530" false-date matches since spaces aren't separators.
#     # 2. Also strip partial date fragments "NN/NN/ YY" (date broken at line boundary)
#     #    using a combined pattern that removes both the "NN/NN/" tail AND the trailing
#     #    year digits atomically, so no isolated "26" fragment remains.
#     # 3. Apply OCR-split repair AFTER date strip, in two passes:
#     #    Pass A: 3-digit prefix groups (unambiguous thousands separators).
#     #    Pass B: 1-2 digit prefix groups (only safe after year fragments are removed).
#     # 4. strip_noise_columns() is NOT changed -- it intentionally keeps dates intact
#     #    so the EOL Dr/Cr marker detection and details cleaning work correctly.

#     MONEY_REGEX = r'(-?\\d{{1,3}}(?:,\\d{{2,3}})*\\.\\d{{2}}|-?\\d{{4,9}}\\.\\d{{2}})\\s*([CcDd][Rr])?'

#     # Strict date regex: [-/] separators only (no space), used INSIDE parse_money
#     # to strip dates without corrupting adjacent amount fragments like "113.00 60 530.51"
#     _DATE_STRICT = r'\\b\\d{{1,2}}[-/](?:\\d{{1,2}}|[A-Za-z]{{3,9}})[-/]\\d{{2,4}}\\b'
#     # Partial date fragment: "NN/NN/" optionally followed by 2-4 digit year leftover
#     # Matches split dates like "13/01/ 26" or "13/01/26" not caught by _DATE_STRICT
#     _DATE_PARTIAL = r'\\b\\d{{1,2}}[/-]\\d{{1,2}}[/-]\\s*\\d{{0,4}}'

#     def parse_money(text):
#         results = []
#         try:
#             # Step 1: Strip complete dates (strict: [-/] only) to remove value-date columns
#             t = re.sub(_DATE_STRICT, ' ', text, flags=re.IGNORECASE)
#             # Step 2: Strip partial date fragments (e.g. "13/01/" + orphaned year "26")
#             t = re.sub(_DATE_PARTIAL, ' ', t)
#             # Step 3: Collapse whitespace
#             t = re.sub(r'\\s{{2,}}', ' ', t).strip()
#             # Step 4a: OCR-split repair -- 3-digit prefix (unambiguous: can never be a 2-digit year)
#             t = re.sub(r'(?<![,\\d/])(\\d{{3}}),?\\s(\\d{{3}}\\.\\d{{2}})(?!\\d)', r'\\1,\\2', t)
#             # Step 4b: OCR-split repair -- 1-2 digit prefix (safe now: year fragments removed above)
#             t = re.sub(r'(?<![,\\d/])(\\d{{1,2}}),?\\s(\\d{{3}}\\.\\d{{2}})(?!\\d)', r'\\1,\\2', t)
#             for m in re.finditer(MONEY_REGEX, t):
#                 raw = m.group(1).replace(",", "")
#                 suffix = (m.group(2) or "").upper()
#                 results.append((float(raw), suffix))
#         except Exception:
#             pass
#         return results

# ============================================================
# PHASE 6 -- DEBIT/CREDIT DETECTION HELPERS
# ============================================================

#     CREDIT_KEYWORDS = [
#         "cr inw", "cr-inw", "apbs cr", "neft cr", "imps cr", "neft in",
#         "deposit", "credit", "received", "interest", "cashback earned",
#         "refund", "salary", "upi in", "reversal cr", "by transfer",
#         "by cash", "dividend", "maturity", "proceeds", "upi/cr",
#         "neftcr",
#         "neft cr-",
#         "cr-",
#         "dep",
#         "dep tfr",
#     ]
#     DEBIT_KEYWORDS = [
#         "withdrawal", "debit", "paid", "purchase",
#         "upi out",
#         "ach d-", "emi", "charge", "fee", "to transfer", "nach dr",
#         "neft out", "imps out", "bill pay", "tax deducted", "upi/dr",
#     ]

#     def detect_credit_from_keywords(text):
#         try:
#             lower = text.lower()
#             c = sum(1 for k in CREDIT_KEYWORDS if k in lower)
#             d = sum(1 for k in DEBIT_KEYWORDS if k in lower)
#             if c > d: return True
#             if d > c: return False
#         except Exception:
#             pass
#         return None

#     def detect_dr_cr_eol(line):
#         \"\"\"
#         Detects explicit Dr/Cr marker at END of line.
#         Federal Bank appends 'Dr' or 'Cr' as the last column on every data row.
#         \"\"\"
#         try:
#             stripped = line.strip()
#             if re.search(r'\\bDr\\b\\s*$', stripped, re.IGNORECASE): return 'DR'
#             if re.search(r'\\bCr\\b\\s*$', stripped, re.IGNORECASE): return 'CR'
#         except Exception:
#             pass
#         return None

#     def compute_confidence(debit, credit, balance, prev_bal, dc_method):
#         try:
#             base = {{'suffix': 0.99, 'eol_marker': 0.97, 'delta': 0.93,
#                      'keyword': 0.75, 'fallback': 0.55}}.get(dc_method, 0.6)
#             if prev_bal is not None and balance is not None:
#                 expected_delta = round(balance - prev_bal, 2)
#                 actual_amount  = debit if debit is not None else (credit if credit is not None else 0)
#                 actual_signed  = -actual_amount if debit is not None else actual_amount
#                 if abs(actual_signed - expected_delta) < 0.02:
#                     base = min(1.0, base + 0.03)
#                 else:
#                     base = max(0.0, base - 0.20)
#             return round(base, 2)
#         except Exception:
#             return 0.6

# ============================================================
# PHASE 7 -- TRANSACTION BLOCK FLUSH
# ============================================================

#     # DC_STRATEGY: {dc_strategy}
#     # Columns  : {headers}

#     def flush_block(block_lines, date_str, prev_bal):
#         try:
#             if not block_lines or not date_str:
#                 return None, prev_bal

#             raw_full  = " ".join(block_lines)

#             # Step 1: Strip noise columns from the FULL block text
#             clean_full = strip_noise_columns(raw_full)

#             # Step 2: Parse money from CLEANED text
#             # parse_money() internally strips dates with strict regex before OCR repair
#             money_vals = parse_money(clean_full)
#             if not money_vals:
#                 return None, prev_bal

#             balance     = money_vals[-1][0]
#             debit       = None
#             credit      = None
#             dc_method   = 'fallback'

#             if "{dc_strategy}" == "SEPARATE_COLUMNS":
#                 if prev_bal is not None:
#                     delta = round(balance - prev_bal, 2)
#                     if delta > 0:
#                         credit    = round(abs(delta), 2)
#                         dc_method = 'delta'
#                     elif delta < 0:
#                         debit     = round(abs(delta), 2)
#                         dc_method = 'delta'
#                     else:
#                         amt = money_vals[-2][0] if len(money_vals) >= 2 else None
#                         is_cr = detect_credit_from_keywords(raw_full)
#                         if amt is not None:
#                             if is_cr is True:  credit = amt
#                             else:              debit  = amt
#                         dc_method = 'keyword'
#                 else:
#                     amt = money_vals[-2][0] if len(money_vals) >= 2 else None
#                     is_cr = detect_credit_from_keywords(raw_full)
#                     if amt is not None:
#                         if is_cr is True:  credit = amt; dc_method = 'keyword'
#                         elif is_cr is False: debit = amt; dc_method = 'keyword'
#                         else:              debit  = amt; dc_method = 'fallback'

#             elif "{dc_strategy}" == "WITHDRAWAL_DEPOSIT":
#                 if len(money_vals) >= 3:
#                     w_val = money_vals[-3][0]
#                     d_val = money_vals[-2][0]
#                     if prev_bal is not None:
#                         delta = round(balance - prev_bal, 2)
#                         if delta > 0:   credit = round(abs(delta), 2); dc_method = 'delta'
#                         elif delta < 0: debit  = round(abs(delta), 2); dc_method = 'delta'
#                     else:
#                         if d_val > 0 and w_val == 0: credit = d_val; dc_method = 'keyword'
#                         elif w_val > 0 and d_val == 0: debit = w_val; dc_method = 'keyword'
#                         else:
#                             is_cr = detect_credit_from_keywords(raw_full)
#                             if is_cr is True: credit = d_val
#                             else:             debit  = w_val
#                             dc_method = 'keyword'
#                 elif len(money_vals) == 2:
#                     amt = money_vals[-2][0]
#                     is_cr = detect_credit_from_keywords(raw_full)
#                     if is_cr is True:  credit = amt; dc_method = 'keyword'
#                     elif is_cr is False: debit = amt; dc_method = 'keyword'
#                     else:              debit  = amt; dc_method = 'fallback'
#                     if prev_bal is not None:
#                         delta = round(balance - prev_bal, 2)
#                         actual_signed = amt if credit is not None else -amt
#                         if abs(actual_signed - delta) < 0.02: dc_method = 'delta'

#             elif "{dc_strategy}" in ("AMOUNT_WITH_DR_CR", "AMOUNT_WITH_TYPE_COL"):
#                 if len(money_vals) >= 2:
#                     txn_amount = money_vals[-2][0]
#                     txn_suffix = money_vals[-2][1]
#                     if txn_suffix == "CR":
#                         credit    = txn_amount; dc_method = 'suffix'
#                     elif txn_suffix == "DR":
#                         debit     = txn_amount; dc_method = 'suffix'
#                     else:
#                         eol = detect_dr_cr_eol(block_lines[-1])
#                         if eol == 'DR':   debit  = txn_amount; dc_method = 'eol_marker'
#                         elif eol == 'CR': credit = txn_amount; dc_method = 'eol_marker'
#                         else:
#                             ttype = re.search(
#                                 r'\\b(TFR|CLG|FT|MB|NACH|NEFT|IMPS|UPI|RTGS)\\s*(IN|OUT)?\\b',
#                                 raw_full, re.IGNORECASE
#                             )
#                             if ttype:
#                                 direction = (ttype.group(2) or "").upper()
#                                 if direction == "IN":   credit = txn_amount; dc_method = 'eol_marker'
#                                 elif direction == "OUT": debit = txn_amount; dc_method = 'eol_marker'
#                                 else:
#                                     if prev_bal is not None:
#                                         delta = round(balance - prev_bal, 2)
#                                         if delta > 0:   credit = round(abs(delta), 2); dc_method = 'delta'
#                                         elif delta < 0: debit  = round(abs(delta), 2); dc_method = 'delta'
#                             else:
#                                 if prev_bal is not None:
#                                     delta = round(balance - prev_bal, 2)
#                                     if delta > 0:   credit = round(abs(delta), 2); dc_method = 'delta'
#                                     elif delta < 0: debit  = round(abs(delta), 2); dc_method = 'delta'
#                                 else:
#                                     is_cr = detect_credit_from_keywords(raw_full)
#                                     if is_cr is True: credit = txn_amount; dc_method = 'keyword'
#                                     else:             debit  = txn_amount; dc_method = 'keyword'

#             else:  # DELTA_FALLBACK
#                 if len(money_vals) >= 2:
#                     txn_amount = money_vals[-2][0]
#                     txn_suffix = money_vals[-2][1]
#                     if txn_suffix == "CR":   credit = txn_amount; dc_method = 'suffix'
#                     elif txn_suffix == "DR": debit  = txn_amount; dc_method = 'suffix'
#                     elif prev_bal is not None:
#                         delta = round(balance - prev_bal, 2)
#                         if delta > 0:   credit = round(abs(delta), 2); dc_method = 'delta'
#                         elif delta < 0: debit  = round(abs(delta), 2); dc_method = 'delta'
#                         else:
#                             is_cr = detect_credit_from_keywords(raw_full)
#                             if is_cr is True: credit = txn_amount
#                             else:             debit  = txn_amount
#                             dc_method = 'keyword'
#                     else:
#                         is_cr = detect_credit_from_keywords(raw_full)
#                         if is_cr is True:    credit = txn_amount; dc_method = 'keyword'
#                         elif is_cr is False: debit  = txn_amount; dc_method = 'keyword'
#                         else:                debit  = txn_amount; dc_method = 'fallback'
#                 elif len(money_vals) == 1:
#                     if prev_bal is not None:
#                         delta = round(balance - prev_bal, 2)
#                         if delta > 0:   credit = round(abs(delta), 2); dc_method = 'delta'
#                         elif delta < 0: debit  = round(abs(delta), 2); dc_method = 'delta'

#             # ----------------------------------------------------------
#             # CLEAN DETAILS
#             # ----------------------------------------------------------
#             details = clean_full
#             details = re.sub(DATE_REGEX, "", details, flags=re.IGNORECASE)
#             details = re.sub(MONEY_REGEX, "", details)
#             details = re.sub(r'\\b(DR|CR)\\b', '', details, flags=re.IGNORECASE)
#             details = re.sub(r'^\\s*\\d{{1,4}}\\s+', '', details)
#             details = re.sub(r'\\s{{2,}}', ' ', details).strip()
#             details = details[:200]

#             conf = compute_confidence(debit, credit, balance, prev_bal, dc_method)

#             return {{
#                 "date"      : date_str,
#                 "details"   : details,
#                 "debit"     : debit,
#                 "credit"    : credit,
#                 "balance"   : balance,
#                 "confidence": conf,
#             }}, balance

#         except Exception:
#             return None, prev_bal

# ============================================================
# PHASE 8 -- OPENING BALANCE DETECTION
# ============================================================

#     previous_balance = None
#     try:
#         ob_scan = raw_lines[:25] + raw_lines[-20:]
#         for raw_line in ob_scan:
#             if re.search(r'(?i)(opening\\s*balance|b/?f|brought\\s*forward)', raw_line):
#                 cl = clean_line(raw_line)
#                 m  = parse_money(strip_noise_columns(cl))
#                 if m:
#                     val, sfx     = m[-1]
#                     previous_balance = -abs(val) if sfx == 'DR' else abs(val)
#                 break
#     except Exception:
#         pass

# ============================================================
# PHASE 9 -- FIRST-TRANSACTION GUARANTEE (READ CAREFULLY)
# ============================================================

#     # CRITICAL: The FIRST transaction in the document MUST be captured.
#     # Common LLM mistake: adding a guard like `if prev_bal is None: return None`
#     # or `if i == 0: skip` inside flush_block. DO NOT DO THIS.
#     # The first transaction block IS valid even if prev_bal differs from expected.
#     # Treat the first transaction IDENTICALLY to all subsequent ones.
#     # If keyword detection returns None for the first transaction (no credit/debit keyword),
#     # assign it as debit with dc_method='fallback' -- never discard it.

# ============================================================
# PHASE 9 -- MAIN EXTRACTION LOOP
# ============================================================

#     transactions    = []
#     current_lines   = []
#     current_date    = None
#     stop_processing = False

#     try:
#         RAW_SKIP_PATTERNS = [
#             r'(?i)as\\s+on\\s+\\d',
#             r'(?i)welcome[:,]?\\s',
#             r'(?i)account\\s+summary',
#             r'(?i)statement\\s+from',
#             r'(?i)^\\s*mr\\.?\\s+\\w',
#             r'(?i)branch\\s+code\\s*[:\\d]',
#             r'(?i)ifsc\\s+code\\s*[:\\s]',
#             r'(?i)date\\s+of\\s+statement',
#             r'(?i)clear\\s+balance\\s*:',
#             r'(?i)monthly\\s+avg\\s+balance',
#             r'(?i)account\\s+open\\s+date',
#             r'(?i)^\\s*cif\\s*(number|no)',
#             r'(?i)^\\s*drawing\\s+power',
#         ]

#         for raw_line in raw_lines:
#             if stop_processing:
#                 break

#             raw_stripped = raw_line.strip()
#             if any(re.search(p, raw_stripped) for p in RAW_SKIP_PATTERNS):
#                 continue

#             if is_header_row(raw_line):
#                 continue

#             cl = clean_line(raw_line)

#             if is_hard_stop(cl):
#                 if current_lines and current_date:
#                     txn, previous_balance = flush_block(current_lines, current_date, previous_balance)
#                     if txn and (txn["debit"] is not None or txn["credit"] is not None):
#                         transactions.append(txn)
#                 stop_processing = True
#                 break

#             if should_skip(cl):
#                 continue

#             if line_starts_transaction(raw_line):
#                 if current_lines and current_date:
#                     txn, previous_balance = flush_block(current_lines, current_date, previous_balance)
#                     if txn and (txn["debit"] is not None or txn["credit"] is not None):
#                         transactions.append(txn)
#                 current_date  = extract_first_date(raw_line)
#                 current_lines = [cl]
#             else:
#                 if current_lines:
#                     stripped = cl.strip()
#                     if stripped:
#                         current_lines.append(stripped)

#         if not stop_processing and current_lines and current_date:
#             txn, previous_balance = flush_block(current_lines, current_date, previous_balance)
#             if txn and (txn["debit"] is not None or txn["credit"] is not None):
#                 transactions.append(txn)

#     except Exception:
#         pass

# ============================================================
# PHASE 10 -- POST-PROCESSING VALIDATION
# ============================================================

#     DATE_VALID = r'\\d{{1,2}}[-/ ](?:\\d{{1,2}}|[A-Za-z]{{3,9}})[-/ ]\\d{{2,4}}'
#     validated  = []
#     try:
#         for txn in transactions:
#             if txn.get("debit") is None and txn.get("credit") is None:
#                 continue
#             if txn.get("balance") is None:
#                 continue
#             if not txn.get("details", "").strip():
#                 continue
#             if not re.search(DATE_VALID, txn.get("date", "")):
#                 continue
#             if txn.get("debit")  == 0.0 and txn.get("credit") is None:
#                 continue
#             if txn.get("credit") == 0.0 and txn.get("debit")  is None:
#                 continue
#             validated.append(txn)
#     except Exception:
#         pass

#     return validated

# ============================================================
# OUTPUT FORMAT
# ============================================================

# Each transaction dict MUST have exactly these 6 keys:

#     {{
#         "date"      : str,         # As found in document -- do not reformat
#         "details"   : str,         # Cleaned narration, max 200 chars
#         "debit"     : float|None,  # Withdrawal / outflow
#         "credit"    : float|None,  # Deposit / inflow
#         "balance"   : float|None,  # Running balance after this transaction
#         "confidence": float,       # 0.0-1.0  (see compute_confidence())
#     }}

# Confidence scoring guide:
#     1.00 - Amount suffix (1500.00CR) confirmed by delta
#     0.99 - Amount suffix only
#     0.97 - End-of-line Dr/Cr marker confirmed by delta
#     0.93 - Balance delta only (no explicit marker)
#     0.75 - Keyword detection
#     0.55 - Fallback (no signal available)
#     Subtract 0.20 if delta contradicts the classified direction.

# ============================================================
# SAFETY RULES
# ============================================================

# - Wrap the ENTIRE function body in try/except -- never crash
# - Return empty list on total failure, never raise
# - NEVER run is_hard_stop() or should_skip() on RAW lines -- use cleaned lines
# - NEVER run line_starts_transaction() or is_header_row() on cleaned lines -- use raw
# - Skip transactions where BOTH debit and credit are None
# - Skip zero-amount transactions (debit=0.0 with credit=None, or vice versa)
# - Never include footer, summary, page-header, or noise-column text in details
# - Truncate details to 200 characters maximum

# ============================================================
# RETURN RULE
# ============================================================

# Return ONLY the Python function code.
# Do NOT include markdown backticks.
# Do NOT add any explanation or comments outside the function.
# The function must be completely self-contained and immediately runnable.

# ============================================================
# INPUT TEXT SAMPLE  (study column positions carefully)
# ============================================================

# {text_sample}
# """
# def build_prompt(identifier_json: dict, text_sample: str) -> str:
#     institution = identifier_json.get("institution_name", "Unknown")
#     doc_family = identifier_json.get("document_family", "BANK_ACCOUNT_STATEMENT")
#     doc_subtype = identifier_json.get("document_subtype", "")
#     return f"""
# You are a financial data extraction engineer as well as a Python engineer. 
# First Extract ALL transaction entries from the provided bank statement document text
# Then Write a Python function that extracts transactions
# from an provided bank statementdocument text same as you extracted.

# The function will be called with the raw extracted text of a real statement.
# A extracted text and document info of that statement is provided below so you can observe its structure.

# ════════════════════════════════════════════
# DOCUMENT INFO
# ════════════════════════════════════════════
# Document Family: {doc_family}
# Document Subtype: {doc_subtype}

# ════════════════════════════════════════════
# SAMPLE DOCUMENT TEXT
# ════════════════════════════════════════════

# {text_sample}

# Now first extract the transactions from the above text as per the rules given below
# ════════════════════════════════════════════
# RULES
# ════════════════════════════════════════════

# 1. Extract EVERY transaction row. A transaction starts with a date.
# 2. SKIP these entirely — they are NOT transactions:
#    - Headers (Date, Particulars, Debit, Credit, Balance)
#    - Footers (Page numbers, disclaimers, generated on...)
#    - Summary rows (Opening Balance, Closing Balance, Total Debit/Credit)
#    - Account info (Branch, IFSC, MICR, Account Number)
# 3. DETAILS field must contain ONLY the transaction narration/description:
#    - Do NOT include dates, amounts, page numbers, or header text in details.
#    - Do NOT include footer text, branch info, or account numbers in details.
#    - Example GOOD: "NEFT CR ACME CORP SALARY"
#    - Example BAD: "01/01/2025 NEFT CR ACME CORP 50000.00 Page 1 of 3"
# 4. Handle Indian number formats (1,00,000.00).
# 5. Normalize dates to DD/MM/YYYY.
# 6. DEBIT/CREDIT: Every transaction MUST have either debit or credit filled (not both None).
#    - If running balance increases, the amount is credit.
#    - If running balance decreases, the amount is debit.
#    - If column headers say Withdrawal/Debit use those.
#    - If column headers say Deposit/Credit use those.
# ════════════════════════════════════════════
# OUTPUT FORMAT FOR EXTRACTED TRANSACTIONS (JSON ARRAY)
# ════════════════════════════════════════════

# [
#   {{
#     "date": "DD/MM/YYYY",
#     "details": "<transaction description only, no dates/amounts/noise>",
#     "debit": <float or None>,
#     "credit": <float or None>,
#     "balance": <float or None>,
#     "confidence": <0.0 to 1.0>
#   }}
# ]

# Now you already read the extracted text of above document and also extracted transactions from it
# , so perform the following task
# ════════════════════════════════════════════
# YOUR TASK
# ════════════════════════════════════════════
# Now see the transactions you have extracted and Write extract_transactions(text) 
# that does exactly what you just did when reading this document. The function 
# receives the same kind of text and must return the same transactions you 
# identified — nothing more, nothing less.

# def extract_transactions(text: str) -> list:
#     \"\"\"
#     Returns list of dicts — one per transaction, in document order:
#     {{
#         "date"      : str,          # DD/MM/YYYY — zero-padded, 4-digit year
#         "details"   : str,          # narration/description of transaction as it appears
#         "debit"     : float | None, # money going OUT — None if credit
#         "credit"    : float | None, # money coming IN — None if debit
#         "balance"   : float | None, # running balance after this transaction
#         "confidence": float         # 0.95 normal, 0.92 fee/charge, 0.70 uncertain
#     }}
#     \"\"\"

# ════════════════════════════════════════════
# RULES
# ════════════════════════════════════════════

# - Raw Python only. No markdown fences. No imports except re (pre-injected).
# - Exactly one of debit or credit per transaction. Never both. Never neither.
# - Deduplicate on (date, details, debit, credit) — keep first occurrence.
# - Return [] if text is clearly not from {institution}.

# The function must begin with exactly: def extract_transactions(text: str) -> list:
# """

def build_prompt(identifier_json: dict, text_sample: str) -> str:
    institution = identifier_json.get("institution_name", "Unknown")
    doc_family = identifier_json.get("document_family", "BANK_ACCOUNT_STATEMENT")
    doc_subtype = identifier_json.get("document_subtype", "")
    return f"""
════════════════════════════════════════════
C — CAPACITY (Who you are)
════════════════════════════════════════════
You are a senior financial data engineer with deep expertise in parsing Indian
bank account statements. You have processed statements from every major Indian
bank — SBI, HDFC, YES Bank, Kotak, Federal, BOI, Axis, ICICI — and you
understand exactly how each bank formats its transaction data, how PDF
extraction distorts column layouts, and how to write robust Python parsers
that handle all of these variations correctly.

════════════════════════════════════════════
R — REQUEST (What to do)
════════════════════════════════════════════
First, read the bank statement text below and extract every transaction from it.
Then write a Python function extract_transactions(text) that does exactly what
you just did — so that when called with any page of this same statement, it
returns the same transactions you identified.

════════════════════════════════════════════
I — INSIGHT (Context and document)
════════════════════════════════════════════
Document Family : {doc_family}
Document Subtype: {doc_subtype}
Institution     : {institution}

DOCUMENT TEXT:
{text_sample}

════════════════════════════════════════════
S — STEPS (How to do it)
════════════════════════════════════════════
Step 1 — Extract transactions from the document text above using these rules:

  1. A transaction row starts with a date. Extract every such row.
  2. Skip entirely: column headers, page numbers, disclaimers, account info,
     Opening Balance, Closing Balance, Grand Total, Summary rows.
  3. DETAILS must contain only the transaction narration/description —
     no dates, no amounts, no ref numbers, no page numbers, no footer text.
     GOOD: "NEFT CR ACME CORP SALARY"
     BAD : "01/01/2025 NEFT CR ACME CORP 50000.00 Page 1 of 3"
  4. Handle Indian number formats: 1,00,000.00
  5. Normalize dates to DD/MM/YYYY.
  6. Determine debit or credit — every transaction must have exactly one:
     - Use column headers (Withdrawal/Deposit, Debit/Credit) if present.
     - When two amount columns exist and one is 0.00 or empty, the non-zero
       amount is the transaction amount — its column determines direction.
     - When only one amount appears with no column context, use balance change:
       balance went up = credit, balance went down = debit.

Step 2 — Output extracted transactions as a JSON array:
[
  {{
    "date"      : "DD/MM/YYYY",
    "details"   : "<narration only>",
    "debit"     : <float or None>,
    "credit"    : <float or None>,
    "balance"   : <float or None>,
    "confidence": <0.95 normal | 0.92 fee/charge | 0.70 uncertain>
  }}
]

Step 3 — Write the Python function that replicates exactly what you did in
Step 1 and Step 2 for any page of this statement:

def extract_transactions(text: str) -> list:
    \"\"\"
    Returns list of dicts — one per transaction, in document order:
    {{
        "date"      : str,          # DD/MM/YYYY — zero-padded, 4-digit year
        "details"   : str,          # narration/description only
        "debit"     : float | None, # money going OUT — None if credit
        "credit"    : float | None, # money coming IN — None if debit
        "balance"   : float | None, # running balance after this transaction
        "confidence": float         # 0.95 normal, 0.92 fee/charge, 0.70 uncertain
    }}
    \"\"\"

════════════════════════════════════════════
P — PERFORMANCE (Quality standard)
════════════════════════════════════════════
- Every transaction in the document must be extracted — none missed, none added.
- Debit and credit must be correct for every transaction.
- Details must be clean — no noise, no leakage from adjacent columns or footers.
- The function must work on any page of this statement, not just the sample.

════════════════════════════════════════════
E — EXTRA (Hard constraints)
════════════════════════════════════════════
- Raw Python only. No markdown fences. No imports except re (pre-injected).
- Exactly one of debit or credit per transaction. Never both. Never neither.
- Deduplicate on (date, details, debit, credit) — keep first occurrence.
- Return [] if text is clearly not from {institution}.
- The function must begin with exactly: def extract_transactions(text: str) -> list:
"""