def build_prompt(identifier_json: dict, text_sample: str) -> str:
    institution = identifier_json.get("institution_name", "Unknown")

    # Get parsing hints to provide context
    hints = identifier_json.get("parsing_hints", {})
    skip_labels = hints.get("summary_section_labels", [])
    layout = hints.get("layout_type", "SINGLE_COLUMN")

    skip_instruction = ""
    if skip_labels:
        skip_instruction = f"\n- Skip lines starting with: {', '.join(skip_labels[:10])}"

    return f"""
Write extract_transactions(text: str) -> list for {institution} credit card statements.

SAMPLE FROM ONE STATEMENT (your code must work for ALL similar statements, not just this one):
{text_sample}

PARSING CONTEXT:
- Layout: {layout}
- Institution: {institution}{skip_instruction}

CRITICAL: Your code must be ROBUST and handle variations:
- Different transaction descriptions and merchants
- Varying amounts of whitespace and alignment
- Extra text, noise, or formatting differences between statements
- Different page breaks or promotional content
- Use the sample to understand the PATTERN, not to hardcode specific text

TASK:
Extract all transactions (date + description + amount).
Skip: headers, footers, summaries, offers, T&C text, page numbers, noise.

STEPS:
1. Find transaction lines (date + description + amount) using PATTERN MATCHING
2. Handle multi-line: merge continuation lines without dates CAREFULLY (only if clearly a continuation)
   - DO NOT merge noise lines (page numbers, headers, footers, random text)
   - Use patterns to identify valid continuations, not exact strings
3. Extract details AS-IS:
   - Keep ALL prefixes (UPI-, IMPS-, etc.)
   - Keep reference numbers and transaction IDs
   - Preserve raw text exactly as it appears
   - Do NOT clean, strip, or modify the details string
4. Classify debit vs credit:
   - Credit (money in): PAYMENT, REFUND, CREDIT, REVERSAL, WAIVER, CASHBACK
   - Debit (money out): all purchases, fees, charges, interest
   - For credit cards: most transactions are debits (charges), payments are credits

OUTPUT:
[{{"date": "YYYY-MM-DD", "details": str, "debit": float|None, "credit": float|None, "balance": None, "confidence": float}}]

RULES:
- Write GENERIC code using patterns (regex, keywords), NOT hardcoded strings
- Exactly one of debit/credit per transaction (never both, never neither)
- Normalize dates to YYYY-MM-DD format
- Deduplicate on (date, details, debit, credit)
- Filter out lines that don't match transaction patterns (no date or no amount = not a transaction)
- Skip noise: page numbers, promotional text, terms & conditions, headers, footers
- Confidence: 0.95 normal, 0.85 if debit/credit unclear, 0.70 uncertain
- Raw Python only, no markdown
- Only use built-in types (dict, list, str, float, int, bool, None)
- Do NOT import typing, Optional, List, Dict - use lowercase dict, list instead
- Available imports: re, datetime, date, timedelta (already imported, just use them)
- Only import re if needed for regex operations

Write the function now.
"""