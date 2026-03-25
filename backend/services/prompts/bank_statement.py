
import re

def build_prompt(identifier_json: dict, text_sample: str) -> str:
    """
    Optimized bank statement code generation prompt.
    Reduced from ~3500 to ~800 tokens while maintaining quality.
    """

    institution = identifier_json.get("institution_name", "Unknown")
    doc_family = identifier_json.get("document_family", "BANK_ACCOUNT_STATEMENT")

    # Get parsing hints to provide context
    hints = identifier_json.get("parsing_hints", {})
    layout = hints.get("layout_type", "SINGLE_COLUMN")
    skip_labels = hints.get("summary_section_labels", [])
    boundary_signals = hints.get("transaction_boundary_signals", ["DATE"])
    ref_pattern = hints.get("ref_no_pattern")

    skip_instruction = ""
    if skip_labels:
        skip_instruction = f"\n- Skip lines starting with: {', '.join(skip_labels[:10])}"

    ref_instruction = ""
    if ref_pattern:
        ref_instruction = f"\n- Strip reference numbers matching: {ref_pattern}"

    return f"""
Write extract_transactions(text: str) -> list for this bank statement.

DOCUMENT SAMPLE:
{text_sample}

PARSING CONTEXT:
- Layout: {layout}
- Institution: {institution}{skip_instruction}{ref_instruction}
- Transaction boundaries: {', '.join(boundary_signals)}

APPROACH:
1. Process ALL pages in the document:
   - The text contains multiple pages separated by "==== PAGE N ====" markers
   - Continue processing through ALL page separators until end of text
   - Do NOT stop after the first page or first table
2. Find transaction lines (have date + amounts, not headers/footers/summaries)
3. Handle multi-line transactions CAREFULLY:
   - Only merge if the next line is clearly a continuation (no date, no amount, indented or very short)
   - DO NOT merge if next line looks like a separate transaction or has amounts
   - DO NOT merge lines separated by page breaks or section dividers
   - When in doubt, treat as separate transactions rather than merging
4. Extract details field AS-IS:
   - Keep ALL prefixes (UPI-, IMPS-, NEFT-, RTGS-, ACHD-, etc.)
   - Keep reference numbers and transaction IDs
   - Preserve the raw text exactly as it appears in the statement
   - Do NOT clean, strip, or modify the details string
5. Classify debit vs credit (CRITICAL - use ALL methods):
   a) Column position: if statement has Debit/Credit or Withdrawal/Deposit columns
   b) Balance change: compare current vs previous balance
      - Balance DECREASED = debit (money out)
      - Balance INCREASED = credit (money in)
   c) Keywords in details:
      - Credit: PAYMENT, REFUND, CREDIT, REVERSAL, DEPOSIT, INTEREST EARNED
      - Debit: PURCHASE, WITHDRAWAL, FEE, CHARGE, TRANSFER OUT
6. Extract fields:
   - Date: normalize to YYYY-MM-DD (handle 2-digit years: 00-30→2000s, 31-99→1900s)
   - Amounts: handle Indian format (1,00,000.00), strip currency symbols
   - Balance: rightmost amount or calculate from previous

OUTPUT FORMAT:
[{{"date": "YYYY-MM-DD", "details": str, "debit": float|None, "credit": float|None, "balance": float|None, "confidence": float}}]

RULES:
- CRITICAL: Process the ENTIRE document text - do NOT stop after first page
- Page separators (==== PAGE N ====) are just markers - continue processing after them
- Exactly one of debit/credit per transaction (never both, never neither)
- If unsure about debit/credit, use balance change as tie-breaker
- Deduplicate on (date, details, debit, credit)
- Skip: headers, footers, summaries (Opening/Closing Balance, Total Debit/Credit)
- Confidence: 0.95 normal, 0.85 if debit/credit unclear, 0.70 if amount/date uncertain
- Raw Python only, no markdown
- Only use built-in types (dict, list, str, float, int, bool, None)
- Do NOT import typing, Optional, List, Dict - use lowercase dict, list instead
- Available imports: re, datetime, date, timedelta (already imported, just use them)
- Only import re if needed for regex operations

Write the function now.
"""
