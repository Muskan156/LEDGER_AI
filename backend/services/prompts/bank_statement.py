
import re



def build_prompt(identifier_json: dict, text_sample: str) -> str:
    institution = identifier_json.get("institution_name", "Unknown")

    return f"""
You are a Python engineer. Write a Python function that extracts transactions
from an Indian bank account statement.

The function will be called with the raw extracted text of a real statement.
A sample of that text is provided below so you can observe its structure.

════════════════════════════════════════════
SAMPLE DOCUMENT TEXT
════════════════════════════════════════════

{text_sample}

════════════════════════════════════════════
WHAT YOU ARE LOOKING AT
════════════════════════════════════════════

The text has already been parsed from a PDF into pipe-separated tables.
Every page produces one or more tables that look like this:

    --- Table N (pipe-separated) ---
    | col1 | col2 | col3 | ... |

Some tables are junk (address blocks, account summaries, legend pages).
Exactly one table per page — the TRANSACTION TABLE — contains the real data.

The transaction table always has a header row with these kinds of column names:
    Date / Txn Date / Value Date
    Narration / Particulars / Description
    Withdrawal / Debit / WithdrawalAmt.
    Deposit / Credit / DepositAmt.
    Balance / Closing Balance / ClosingBalance

Identify it by looking for a row that has BOTH a date-like column AND a
balance column in its header. Ignore every other table completely.

════════════════════════════════════════════
ONE IMPORTANT PDF ARTIFACT TO HANDLE
════════════════════════════════════════════

The PDF extractor sometimes splits one logical transaction across two pipe rows.
The second row has an EMPTY first column (the date cell is blank).
It is a CONTINUATION of the row above — not a separate transaction.

When you see a row whose first column is empty or whitespace:
  - Append its narration text to the previous row's narration.
  - If it carries an amount or balance that the previous row is missing, use it.
  - Then discard the continuation row — it is not its own transaction.

════════════════════════════════════════════
YOUR TASK
════════════════════════════════════════════

Read the sample text above the way a human reads a bank statement.
You already understand what a bank transaction is:
  - it has a date
  - it has a description / narration
  - money either went out (debit/withdrawal) or came in (credit/deposit)
  - it has a running balance

Using that understanding, write extract_transactions(text) so that when
called with any page of this statement it returns all transactions and
skips everything else (headers, address blocks, summaries, footers, legends).

Do not write a rigid line-by-line regex parser.
Write code that finds transactions the same way you would find them by
reading — using context and meaning, not just pattern position.

def extract_transactions(text: str) -> list:
    \"\"\"
    Returns list of dicts — one per transaction, in document order:
    {{
        "date"      : str,          # YYYY-MM-DD — zero-padded, 4-digit year
        "details"   : str,          # narration exactly as it appears
                                    # do not add or remove words
        "debit"     : float | None, # money out — None if this row is a credit
        "credit"    : float | None, # money in  — None if this row is a debit
        "balance"   : float | None, # closing/running balance; None if absent
        "confidence": float         # 0.95 normal transaction
                                    # 0.92 fee / charge / tax
                                    # 0.70 uncertain
    }}
    \"\"\"

════════════════════════════════════════════
RULES
════════════════════════════════════════════

- Raw Python only. No markdown fences. No imports except re (pre-injected).
- Exactly one of debit or credit per transaction. Never both. Never neither.
- A cell containing 0 or "0" when the other side has a real amount
  means that cell is empty — treat it as None, not 0.0.
- Strip ₹, Rs., INR and commas from amounts before converting to float.
- Dates may appear as DD/MM/YY, DD/MM/YYYY, DD-MM-YYYY, DD MMM YYYY.
  Always output as YYYY-MM-DD with 4-digit year.
- Deduplicate on (date, details, debit, credit) — keep first occurrence.
- Skip rows whose narration is exactly "Opening Balance", "Closing Balance",
  "Grand Total", or "GRAND TOTAL".
- Return [] if text contains no recognisable transaction table.

The function must begin with exactly: def extract_transactions(text: str) -> list:
"""