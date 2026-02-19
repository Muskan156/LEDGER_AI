import re
from typing import List, Dict
from repository.statement_category_repo import (
    get_under_review_formats,
    activate_statement_category
)
from services.validation_service import (
    extract_json_from_response,
    validate_transactions
)
from services.llm_parser import parse_with_llm
from services.extraction_service import extract_transactions_using_logic

# ---------------------------------------------------------
# Execute extraction logic stored in DB
# ---------------------------------------------------------
# def execute_db_parser(full_text: str,
#                       extraction_code: str,
#                       identifier_json: dict) -> List[Dict]:

#     cleaned_code = extraction_code.strip()

#     if "```" in cleaned_code:
#         parts = cleaned_code.split("```")
#         cleaned_code = parts[1] if len(parts) > 1 else parts[0]

#     cleaned_code = cleaned_code.strip()

#     if cleaned_code.lower().startswith("python"):
#         cleaned_code = cleaned_code[6:].strip()

#     # Inject required runtime variables
#     namespace = {
#         "re": re,
#         "List": List,
#         "Dict": Dict,

#         # REQUIRED BY YOUR GENERATED CODE
#         "DATE_PATTERN": identifier_json.get("transaction_anchor", {}).get("date_pattern", ""),
#         "MONEY_REGEX": identifier_json.get("amount_pattern", ""),
#         "METADATA_KEYWORDS": identifier_json.get("metadata_keywords", []),
#         "FOOTER_PATTERNS": identifier_json.get("footer_markers", {}).get("patterns", []),
#         "COLUMN_HEADERS": identifier_json.get("table_structure", {}).get("column_headers", []),
#     }

#     exec(cleaned_code, namespace)

#     if "extract_transactions" not in namespace:
#         raise ValueError("extract_transactions not found in DB logic.")

#     return namespace["extract_transactions"](full_text)

def execute_db_parser(full_text: str,
                      extraction_code: str) -> List[Dict]:
    """
    Executes stored extraction logic.
    Universal parser does NOT require identifier injection.
    """

    cleaned_code = extraction_code.strip()

    if "```" in cleaned_code:
        parts = cleaned_code.split("```")
        cleaned_code = parts[1] if len(parts) > 1 else parts[0]

    cleaned_code = cleaned_code.strip()

    if cleaned_code.lower().startswith("python"):
        cleaned_code = cleaned_code[6:].strip()

    namespace = {
        "re": re,
        "List": List,
        "Dict": Dict,
    }

    exec(cleaned_code, namespace)

    if "extract_transactions" not in namespace:
        raise ValueError("extract_transactions not found in DB logic.")

    return namespace["extract_transactions"](full_text)


# ---------------------------------------------------------
# Review Engine
# ---------------------------------------------------------
def run_review_engine(statement_id: int, pdf_path: str, full_text: str):

    statements = get_under_review_formats()
    statements = [s for s in statements if s["statement_id"] == statement_id]
    for stmt in statements:

        statement_id = stmt["statement_id"]
        extraction_logic = stmt["extraction_logic"]

        # Run DB Code
        # identifier_json = stmt["statement_identifier"]
        code_txns = extract_transactions_using_logic(full_text,extraction_logic)


        #Run LLM Parser
        llm_response = parse_with_llm(full_text)
        llm_txns = extract_json_from_response(llm_response)

        #Compare
        metrics = validate_transactions(code_txns, llm_txns)

        if not metrics:
            print("No transactions extracted for comparison.")
            continue

        score = metrics["overall_accuracy"]

        print(f"\nStatement ID {statement_id} Accuracy: {score}%")

        if score >= 90:
            activate_statement_category(statement_id)
            print(f"Statement ID{statement_id} VERIFIED and ACTIVATED")
        
        return {
            "code_transactions": code_txns,
            "llm_transactions": llm_txns,
            "metrics": metrics
        }

    return None