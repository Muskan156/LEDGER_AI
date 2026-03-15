# import re as _re


# def build_prompt(identifier_json: dict, text_sample: str) -> str:

#     family      = identifier_json.get("document_family", "CREDIT_CARD_STATEMENT")
#     institution = identifier_json.get("institution_name", "Unknown")
#     subtype     = identifier_json.get("document_subtype", "")

#     return f"""
# You are a Senior Python Backend Engineer specializing in Indian financial document parsing.

# Read the ENTIRE document text below first. Then generate EXACTLY one Python function:

#     def extract_transactions(text: str) -> list:

# Target  : {institution} — {subtype}
# Family  : {family}
# Output  : Raw Python code only. No markdown fences. No explanation. No comments.
# Rules   : No import statements (re is pre-injected). No LLM calls. Deterministic.

# ════════════════════════════════════════════════════════════════════════════════
# DOCUMENT TEXT
# ════════════════════════════════════════════════════════════════════════════════

# {text_sample}

# ════════════════════════════════════════════════════════════════════════════════
# OUTPUT SCHEMA
# ════════════════════════════════════════════════════════════════════════════════

# Return a list of dicts, one per transaction, in document order:
# {{
#     "date":       str,          # DD/MM/YYYY — always 4-digit year
#     "details":    str,          # clean merchant/narration only — no noise
#     "debit":      float | None,
#     "credit":     float | None,
#     "balance":    float | None, # always None — credit cards have no running balance
#     "confidence": float         # 0.95 normal | 0.92 fee/surcharge/tax | 0.90 EMI | 0.70 if header not found
# }}

# ════════════════════════════════════════════════════════════════════════════════
# CRITICAL — READ THESE EXAMPLES BEFORE WRITING CODE
# ════════════════════════════════════════════════════════════════════════════════

# These are REAL patterns that appear in Indian credit card statement PDFs.
# Your code MUST handle all of them correctly.

# EXAMPLE 1 — Single-line transaction (straightforward):
#   Raw line : "23/01/2026  UPI_SATKAR CATERERS IND  Miscellaneous Stores  375.00 Dr"
#   Correct  : date=23/01/2026, details="UPI_SATKAR CATERERS IND", debit=375.0

# EXAMPLE 2 — Transaction split across two lines (PDF line wrap):
#   Raw line 1: "23/01/2026  UPI_GANESH BAPU GAIKWA IND  Miscellaneous Stores  40.00 Dr"
#   Raw line 2: "UPI_GIRNARINSURANCEBRO ER IND - Ref No:"
#   Raw line 3: "RT260230422001690000117  Miscellaneous Stores  10,779.80 Dr"
#   These are TWO separate transactions:
#     Txn 1: date=23/01/2026, details="UPI_GANESH BAPU GAIKWA IND", debit=40.0
#     Txn 2: date=23/01/2026, details="UPI_GIRNARINSURANCEBRO ER IND", debit=10779.80
#   WHY: Line 2 starts with "UPI_" — that signals a NEW transaction even with no date.
#   Line 3 has no date and no UPI_ — it is the continuation of line 2.

# EXAMPLE 3 — FX annotation row (skip entirely):
#   Raw line 1: "28/01/2026  CURSOR AI POWERED IDE  840  2,166.17 Dr"
#   Raw line 2: "28/01/2026  23.60 USD"
#   Raw line 3: "28/01/2026  FOREIGN CURRENCY MARKUP FEE  59.57 Dr"
#   Line 1 → extract: details="CURSOR AI POWERED IDE", debit=2166.17
#   Line 2 → SKIP: after the date, the content is ONLY "23.60 USD" — amount + currency code, no merchant name
#   Line 3 → extract: details="FOREIGN CURRENCY MARKUP FEE", debit=59.57

# EXAMPLE 4 — Details must be merchant name only:
#   Raw    : "25/01/2026  UPI_HOTEL SHIVRAJ DHAB IND - Ref No: RT260250370002030000139  Miscellaneous Stores  995.00 Dr"
#   Correct: details="UPI_HOTEL SHIVRAJ DHAB IND"
#   Strip  : "- Ref No: RT260250370002030000139" (ref number)
#   Strip  : "Miscellaneous Stores" (merchant category label)
#   Strip  : "995.00 Dr" (amount + suffix)

# EXAMPLE 5 — Page footer must not become a transaction:
#   "to +91 95522 20020  1800 103 1212 | 1800 103 6000  CIN : L65190MH2003PLC143249  Credit Card Statement"
#   NO date in first 35 chars → skip entirely.

# EXAMPLE 6 — TWO-COLUMN PDF (RBL style):
#   Account summary labels appear INTERLEAVED with transaction rows because the PDF
#   has a left sidebar. A line like "Total Amount Due  10,146.00" has no leading date
#   → skip it. Never extract summary labels as transactions.

# ════════════════════════════════════════════════════════════════════════════════
# IMPLEMENTATION RULES
# ════════════════════════════════════════════════════════════════════════════════

# 1. FIND THE TABLE
#    Find the column header row (contains "Date" and "Amount" or similar).
#    Start extracting raw lines only AFTER this header.
#    When the same header appears again on a new page, re-enter table mode.

# 2. COLLECT RAW LINES ACROSS ALL PAGES
#    Page ends with a page-number line ("Page N of M") or footer text.
#    On page end → set in_table = False and CONTINUE (never break).
#    Pages 2, 3, 4+ all contain transactions — never stop at page 1.

# 3. SPLIT INTO INDIVIDUAL TRANSACTION BUFFERS  ← most important step
#    Walk raw_lines one by one. Maintain a `pending` buffer.
#    START a new buffer (flush pending first) when the line matches ANY of:
#      a) Starts with a date in the first 10 characters  (DD/MM/YYYY etc.)
#      b) Starts with "UPI_"  (even mid-page, even without a date)
#      c) Starts with a new RT-prefixed reference number  (RT + 15+ digits)
#    APPEND to pending buffer when the line matches NONE of the above.
#    This is the rule that separates transactions like Examples 2 and 4 above.

# 4. FOR EACH BUFFER — EXTRACT DATE
#    DATE_RE matches: DD/MM/YYYY  DD-MM-YYYY  DD Mon YYYY  D Mon YYYY
#    Normalise to DD/MM/YYYY. 2-digit year → prepend "20" (26 → 2026).
#    Buffer has no date in first 35 chars → skip this buffer entirely.

# 5. FOR EACH BUFFER — SKIP NON-TRANSACTIONS
#    Skip the whole buffer if it matches ANY of:
#    • FX annotation: after the date, content is ONLY "amount + 3-letter currency code"
#      with no merchant name  (e.g. "28/01/2026  23.60 USD")
#    • Summary label line: contains Total Amount Due / Min. Amt. Due / Credit Limit /
#      Payment Due Date / Opening Balance / Closing Balance / Reward Points etc.
#      AND has no leading date
#    • Promotional text: no amount anywhere in the buffer
#    • EMI breakdown row: contains "Principal" / "Interest" column values

# 6. FOR EACH BUFFER — EXTRACT AMOUNT AND DIRECTION
#    Observe which style this document uses (from the text above):

#    Dr/Cr suffix style:
#      SUFFIX_RE = re.compile(r'([\d,]+\.?\d*)\s+(Dr|Cr)\b', re.IGNORECASE)
#      Use the RIGHTMOST match. "Dr" → debit. "Cr" → credit.
#      Skip the match if digit-count after removing commas ≥ 9 (it is a ref number).

#    Single column + keyword style:
#      Rightmost valid amount. Direction from keywords:
#      CREDIT compounds first: TRANSFERRED TO EMI, FUEL SURCHARGE WAIVER, SURCHARGE REV,
#        SURCHARGE WAIVER, PAYMENT RECEIVED, petrol surcharge rev, CASHBACK
#      DEBIT keywords: FEE, CHARGE, CHARGES, GST, TAX, INSTALMENT, MARKUP,
#        FOREIGN CURRENCY, SUBSCRIPTION, SURCHARGE
#      CREDIT keywords: PAYMENT, REFUND, REVERSAL, WAIVER, REWARD
#      Default → debit. Never use UPI / PAY / CR as direction signals.

#    Amount exclusions (all styles):
#      Skip a matched number if: 4-digit 1900–2099 (year) / preceded by Ref|No:|#|RT /
#      9+ digits after removing commas / followed by 3-letter currency / followed by %

#    Exactly ONE of debit/credit is non-None per transaction. Never both.

# 7. FOR EACH BUFFER — CLEAN DETAILS
#    Start: text from end-of-date to start-of-rightmost-valid-amount.
#    Strip in order:
#      a) Ref numbers: "-? Ref No: \\w+"  and  "\\b\\d{{9,}}(?:-\\d+)*\\b"
#      b) Merchant categories: Miscellaneous Stores | Retail Outlet Services |
#         Transportation Services | Service Stations.*?ancillary.*?\\) |
#         "without ancillary services)" | Correspondence Schools
#      c) UPI Fuel Surcharge: "UPI\\s+Fuel\\s+Surcharge\\s+-[\\w-]+"
#      d) Trailing amount+direction: "[\\d,]+\\.\\d{{1,2}}\\s*(Dr|Cr)$"  and  "[\\d,]+\\.\\d{{1,2}}$"
#      e) Collapse spaces → one. Strip edges of  -,./()
#    Reject if result is empty / < 3 chars / > 70% non-space chars are digits.

# 8. ENGINEERING
#    • All re.compile() at top of function body
#    • Nested helpers: parse_amount(), is_new_transaction()
#    • try/except around every buffer — skip silently on any exception
#    • Deduplicate on (date, details, debit, credit) — keep first occurrence
#    • Return [] if text does not appear to be from {institution}
# """
# import re as _re


# def build_prompt(identifier_json: dict, text_sample: str) -> str:

#     family      = identifier_json.get("document_family", "CREDIT_CARD_STATEMENT")
#     institution = identifier_json.get("institution_name", "Unknown")
#     subtype     = identifier_json.get("document_subtype", "")

#     return f"""
# You are a financial document code generation engine.

# Write a Python function that extracts ALL transactions from the provided credit card statement text.

# ════════════════════════════════════════════
# DOCUMENT INFO
# ════════════════════════════════════════════
# Document Family : {family}
# Document Subtype: {subtype}
# Institution     : {institution}

# ════════════════════════════════════════════
# RULES
# ════════════════════════════════════════════

# 1. Extract EVERY real financial transaction. A transaction always starts with a date.

# 2. SKIP these entirely — they are NOT transactions:
#    - Column header rows (Date, Description, Amount, Debit, Credit etc.)
#    - Page number lines and footer text (bank contact, disclaimer, CIN number)
#    - Account summary labels (Total Amount Due, Min. Amt. Due, Credit Limit,
#      Payment Due Date, Opening Balance, Closing Balance, Reward Points)
#    - Promotional content (offers, cashback ads, app download prompts)
#    - EMI breakdown tables (Principal, Interest, GST columns)
#    - Foreign currency annotation rows — where after the date the ONLY content
#      is an amount + 3-letter currency code with no merchant name
#      e.g. "28/01/2026  23.60 USD" → SKIP
#           "28/01/2026  FOREIGN CURRENCY MARKUP FEE  59.57 Dr" → KEEP
#    - Two-column PDF interleaving — summary label lines that appear mixed with
#      transaction rows due to PDF extraction; skip any line that contains a
#      known summary label (Total Amount Due, Min. Amt. Due etc.) and has no
#      leading date

# 3. DETAILS field must contain ONLY the merchant name or transaction narration:
#    - Strip reference numbers: "Ref No: RT260190390001770000728", any 9+ digit sequence
#    - Strip merchant category labels: Miscellaneous Stores, Retail Outlet Services,
#      Transportation Services, Service Stations (with/without ancillary services),
#      Correspondence Schools
#    - Strip trailing amounts and Dr/Cr suffixes: "995.00 Dr", "6.07 Cr"
#    - Strip page footer fragments that got joined to the row
#    - Keep ONLY the merchant name: "UPI_HOTEL SHIVRAJ DHAB IND", "PAYMENT RECEIVED BBPS"

# 4. MULTI-LINE TRANSACTIONS — A single transaction can span multiple lines.
#    A NEW transaction starts when a line begins with a date OR begins with "UPI_"
#    OR begins with a new RT-prefixed reference number (RT + 15 or more digits).
#    All other lines are continuations — join them to the current transaction.

# 5. AMOUNT AND DIRECTION:
#    - Dr/Cr suffix style: "995.00 Dr" → debit, "6.07 Cr" → credit
#    - Two-column style: use Debit and Credit column positions from the header
#    - Single amount + keyword style: classify by keywords —
#      CREDIT: TRANSFERRED TO EMI, FUEL SURCHARGE WAIVER, SURCHARGE WAIVER,
#              SURCHARGE REV, PAYMENT RECEIVED, CASHBACK, petrol surcharge rev
#      DEBIT:  FEE, CHARGE, CHARGES, GST, TAX, INSTALMENT, MARKUP,
#              FOREIGN CURRENCY, SUBSCRIPTION, SURCHARGE
#      CREDIT: PAYMENT, REFUND, REVERSAL, WAIVER, REWARD
#      Default → debit
#    - Skip a matched number if: it is a year (1900-2099), preceded by Ref/No:/RT,
#      has 9+ digits after removing commas, followed by currency code or %
#    - Exactly ONE of debit/credit is non-None. Never both.

# 6. MULTI-PAGE — the statement has multiple pages.
#    When a page ends, use continue not break. Process every page.
#    The column header row repeats on each page — use it to re-enter table mode.

# 7. Normalise all dates to DD/MM/YYYY. 2-digit year → prepend "20" (26 → 2026).

# ════════════════════════════════════════════
# FUNCTION SIGNATURE AND OUTPUT FORMAT
# ════════════════════════════════════════════

# def extract_transactions(text: str) -> list:
#     # returns list of dicts:
#     # {{
#     #     "date":       str,          # DD/MM/YYYY
#     #     "details":    str,          # merchant name only
#     #     "debit":      float | None,
#     #     "credit":     float | None,
#     #     "balance":    float | None, # always None for credit cards
#     #     "confidence": float         # 0.95 normal, 0.92 fee/surcharge/tax, 0.90 EMI, 0.70 if header not found
#     # }}

# ════════════════════════════════════════════
# ENGINEERING REQUIREMENTS
# ════════════════════════════════════════════

# - Raw Python code only. No markdown fences. No explanation outside code.
# - No import statements (re is pre-injected).
# - All re.compile() at top of function body.
# - try/except around every transaction — skip silently on error.
# - Deduplicate on (date, details, debit, credit) — keep first occurrence.
# - Return [] if text does not appear to be from {institution}.

# ════════════════════════════════════════════
# DOCUMENT TEXT
# ════════════════════════════════════════════

# {text_sample}

# ════════════════════════════════════════════
# Return ONLY the Python function. No markdown. No explanation.
# """
# def build_prompt(identifier_json: dict, text_sample: str) -> str:

#     family      = identifier_json.get("document_family", "CREDIT_CARD_STATEMENT")
#     institution = identifier_json.get("institution_name", "Unknown")
#     subtype     = identifier_json.get("document_subtype", "")

#     return f"""
# You are a financial data extraction code generation engine.
# Read the ENTIRE document text below first. Then write a Python function to extract all transactions from it.
# ════════════════════════════════════════════
# DOCUMENT INFO
# ════════════════════════════════════════════
# Document Family : {family}
# Document Subtype: {subtype}
# Institution     : {institution}
# ════════════════════════════════════════════
# DOCUMENT TEXT
# ════════════════════════════════════════════
 
# {text_sample}

# ════════════════════════════════════════════
# RULES
# ════════════════════════════════════════════

# 1. Extract EVERY transaction row. A transaction starts with a date.
# 2. SKIP these entirely — they are NOT transactions:
#    - Headers (Date, Particulars, Debit, Credit, Balance)
#    - Footers (Page numbers, disclaimers, generated on...)
#    - Summary rows (Opening Balance, Closing Balance, Total Debit/Credit)
#    - Account info (Branch, IFSC, MICR, Account Number)
#    - Interest calculation table rows — identified by: the section heading
#      "Interestcalculation" or "Interest calculation" appears before these rows,
#      AND the row contains columns for "Period", "Number of Days", "Interest (₹)".
#      These rows have dates from years far before the statement period (e.g. 2018, 2019).
# 3. DETAILS field must contain ONLY the transaction narration/description:
#    - Do NOT include dates, amounts, page numbers, or header text in details.
#    - Do NOT include footer text, branch info, or account numbers in details.
#    - Example GOOD: "NEFT CR ACME CORP SALARY"
#    - Example GOOD: "UPI_HOTEL SHIVRAJ DHAB IND", "PAYMENT RECEIVED BBPS",
#      "UPI Fuel Surcharge -035064344102-012526"
#    - Example BAD: "01/01/2025 NEFT CR ACME CORP 50000.00 Page 1 of 3"
#    - NOTE: "UPI Fuel Surcharge -XXXXXX" IS a real transaction — do NOT skip it.
#      It appears as a separate dated row and must be extracted like any other transaction.
# 4. Handle Indian number formats (1,00,000.00).
# 5. Normalize dates to DD/MM/YYYY.
# 6. DEBIT/CREDIT: Every transaction MUST have either debit or credit filled (not both None).
#    - If running balance increases, the amount is credit.
#    - If running balance decreases, the amount is debit.
#    - If column headers say Withdrawal/Debit use those.
#    - If column headers say Deposit/Credit use those.
#    - If Dr/Cr suffix present: "100.00 Dr" → debit, "6.07 Cr" → credit.
#    - If no running balance and no Dr/Cr suffix: classify by keywords.
#      CREDIT: FUEL SURCHARGE WAIVER, petrol surcharge rev, PAYMENT RECEIVED,
#              TRANSFERRED TO EMI, CASHBACK, REFUND, REVERSAL, WAIVER, REWARD.
#      DEBIT: everything else including UPI Fuel Surcharge, FEE, CHARGE, GST, TAX.
# ════════════════════════════════════════════
# OUTPUT: Write EXACTLY this Python function
# ════════════════════════════════════════════

# def extract_transactions(text: str) -> list:
#     # returns:
#     # [{{
#     #     "date":       str,          # DD/MM/YYYY
#     #     "details":    str,          # transaction narration only, no noise
#     #     "debit":      float | None,
#     #     "credit":     float | None,
#     #     "balance":    float | None, # None if no running balance column
#     #     "confidence": float         # 0.0 to 1.0
#     # }}]

# ════════════════════════════════════════════
# ENGINEERING
# ════════════════════════════════════════════

# - Raw Python code only. No markdown. No explanation.
# - No import statements (re is pre-injected).
# - try/except around every row — skip silently on error.
# - Return [] if document is not from {institution}.

# ════════════════════════════════════════════
# Return ONLY the Python function. No markdown. No explanation.
# """

# def build_prompt(identifier_json: dict, text_sample: str) -> str:
#     institution = identifier_json.get("institution_name", "Unknown")

#     return f"""
# You are a Python engineer analysing an Indian financial document.
# Work in TWO STEPS. Do not skip Step 1. Do not write any code before completing Step 1.

# ════════════════════════════════════════════
# DOCUMENT TEXT  (read fully before writing anything)
# ════════════════════════════════════════════

# {text_sample}

# ════════════════════════════════════════════
# STEP 1 — REASON ABOUT THIS DOCUMENT
# ════════════════════════════════════════════

# Read the entire document above. Write your analysis for each dimension:

#   DATE FORMAT        : What format are dates in? Give one real example from the text.
#   AMOUNT FORMAT      : Dr/Cr suffix? Two columns? Keyword only? Give one real example.
#   NEW TXN SIGNAL     : What marks the START of a new transaction?
#                        Date? UPI_ prefix? RT-number? Give one real example.
#                        IMPORTANT — does the merchant name appear on the SAME line
#                        as the date and amount, or on the NEXT line?
#                        Show a real example from the text of exactly how it looks.
#   NOISE IN DETAILS   : List EVERY type of noise attached to merchant names.
#                        These are strings that appear alongside the merchant name
#                        but are NOT part of it. Examples of noise types:
#                          "- Ref No: RT260190390001770000728"
#                          "Miscellaneous Stores"
#                          "Retail Outlet Services"
#                        List ALL noise types you actually see in the text above.
#   NON-TXN ROWS       : What rows are NOT transactions and must be skipped?
#                        How do you identify each? Examples:
#                          "Interest calculation rows — dates from 2018/2019"
#                          "Summary labels — Opening Balance, Closing Balance"
#                          "Column headers — contain Date, Description, Amount"
#                        List ALL non-transaction rows you actually see.
#   DIRECTION EDGES    : Any transactions where debit/credit is not obvious?
#                        Examples:
#                          "TRANSFERRED TO EMI — looks like debit but is credit"
#                          "petrol surcharge rev — reversal keyword so credit"
#                          "FUEL SURCHARGE WAIVER — waiver so credit"
#                        List ALL edge cases you actually see.

# ════════════════════════════════════════════
# STEP 2 — WRITE CODE BASED ON YOUR ANALYSIS
# ════════════════════════════════════════════

# Now write extract_transactions() that implements EVERYTHING you described in Step 1.
# You must write code for:
#   - Every noise type you listed → strip it from details
#   - Every non-transaction row type you listed → skip it
#   - Every direction edge case you listed → correct it
#   - Every date format you identified → normalize to DD/MM/YYYY

# If you identified something in Step 1 and your code does not handle it, that is a bug.

# def extract_transactions(text: str) -> list:
#     \"\"\"
#     Returns list of dicts — one per transaction, in document order:
#     {{
#         "date"      : str,          # DD/MM/YYYY — always zero-padded, 4-digit year
#         "details"   : str,          # full merchant/payee descriptor as it appears
#                                     # in the document — preserve city, country, URLs,
#                                     # identifiers that are part of the merchant name.
#                                     # merchant/payee name ONLY — no ref numbers,
#                                     # no category labels, no amounts
#                                     # a separate column (e.g. "Miscellaneous Stores"),
#                                     # and trailing Dr/Cr amount suffixes.
#                                     # If details become empty after stripping,
#                                     # recover the first meaningful token from raw text.
#         "debit"     : float | None, # amount paid out — None if this is a credit
#         "credit"    : float | None, # amount received — None if this is a debit
#         "balance"   : float | None, # running balance if present in document, else None
#         "confidence": float         # 0.95 standard transaction
#                                     # 0.92 fee / surcharge / tax row
#                                     # 0.70 if transaction header line was not found
#     }}
#     \"\"\"

# ════════════════════════════════════════════
# ENGINEERING RULES
# ════════════════════════════════════════════

# - Raw Python only. No markdown fences. No imports outside re (pre-injected).
# - All re.compile() calls at the top of the function body.
# - try/except around every transaction buffer — skip silently on exception.
# - Exactly one of debit or credit must be set per transaction. Never both. Never neither.
# - Deduplicate on (date, details, debit, credit) — keep first occurrence, discard rest.
# - Return [] if the text is clearly not from {institution}.

# Return your Step 1 analysis first, then the complete Python function immediately after.
# The function must begin with exactly: def extract_transactions(text: str) -> list:
# """

def build_prompt(identifier_json: dict, text_sample: str) -> str:
    institution = identifier_json.get("institution_name", "Unknown")

    return f"""
You are a Python engineer. Write a Python function that extracts transactions
from an Indian credit card statement.

The function will be called with the raw extracted text of a real statement.
A sample of that text is provided below so you can observe its structure.

════════════════════════════════════════════
SAMPLE DOCUMENT TEXT
════════════════════════════════════════════

{text_sample}

════════════════════════════════════════════
YOUR TASK
════════════════════════════════════════════

Read the sample text above the way a human reads a bank statement.
You already understand what a credit card transaction is:
  - it has a date
  - it has a payee or description
  - it has an amount
  - it is either a debit (money spent) or a credit (payment/refund/waiver)

Using that understanding, write extract_transactions(text) so that when
called with any page of this statement it returns all transactions and
skips everything else (summaries, headers, offers, footers text, T&C text).

Do not write a rigid line-by-line regex parser.
Write code that finds transactions the same way you would find them by
reading — using context and meaning, not just pattern position.

def extract_transactions(text: str) -> list:
    \"\"\"
    Returns list of dicts — one per transaction, in document order:
    {{
        "date"      : str,          # DD/MM/YYYY — zero-padded, 4-digit year
        "details"   : str,          # payee/description exactly as it appears
                                    # in the statement — do not add or remove words
        "debit"     : float | None, # amount spent — None if this is a credit
        "credit"    : float | None, # amount received — None if this is a debit
        "balance"   : float | None, # always None for credit cards
        "confidence": float         # 0.95 normal transaction
                                    # 0.92 fee / surcharge / tax
                                    # 0.70 uncertain
    }}
    \"\"\"

════════════════════════════════════════════
RULES
════════════════════════════════════════════

- Raw Python only. No markdown fences. No imports except re (pre-injected).
- Exactly one of debit or credit per transaction. Never both. Never neither.
- Deduplicate on (date, details, debit, credit) — keep first occurrence.
- Return [] if text is clearly not from {institution}.

The function must begin with exactly: def extract_transactions(text: str) -> list:
"""