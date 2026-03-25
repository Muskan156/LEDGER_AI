# """
# services/extraction_service.py
# ──────────────────────────────
# STEP 4 — Generate extraction code via LLM (Gemini)
# and execute it safely against document text.

# This module is the thin orchestrator. All document-family-specific
# prompts live in services/prompts/*.py.
# """

# import re
# import logging
# from typing import List, Dict, Any
# from services.prompts import get_prompt
# from services.code_sandbox import execute_extraction_code, validate_code, clean_llm_code

# from config import OPENROUTER_API_KEY, OPENROUTER_MODEL_NAME
# from services.llm_retry import call_with_retry
# logger = logging.getLogger("ledgerai.extraction_service")


# # ═══════════════════════════════════════════════════════════
# # GENERATE EXTRACTION CODE VIA LLM
# # ═══════════════════════════════════════════════════════════

# def generate_extraction_logic_llm(
#     identifier_json: dict,
#     text_sample: str,
# ) -> str:
#     """
#     Generates extraction code using the family-specific prompt from services/prompts/.
#     Returns validated Python code string containing extract_transactions().
#     """
#     document_family = identifier_json.get("document_family", "BANK_ACCOUNT_STATEMENT")

#     prompt = get_prompt(document_family, identifier_json, text_sample)

#     logger.info(
#         "Generating extraction code: family=%s prompt_len=%d",
#         document_family, len(prompt),
#     )

#     response = call_with_retry(
#         OPENROUTER_API_KEY,
#         OPENROUTER_MODEL_NAME,
#         prompt
#     )

#     content = response["choices"][0]["message"]["content"].strip()
#     if not content:
#         raise ValueError("LLM returned empty extraction code.")

#     raw_output = _strip_markdown(content)

#     # Validate AST before returning — reject dangerous code immediately
#     validation_error = validate_code(raw_output)
#     if validation_error:
#         raise ValueError(f"Generated code failed security validation: {validation_error}")

#     logger.info("Generated + validated code: %d chars.", len(raw_output))
#     return raw_output



# # ═══════════════════════════════════════════════════════════
# # EXECUTE EXTRACTION CODE
# # ═══════════════════════════════════════════════════════════

# def extract_transactions_using_logic(
#     full_text: str,
#     extraction_code: str,
# ) -> List[Dict]:
#     """
#     Execute LLM-generated Python code safely via code_sandbox,
#     Returns cleaned transaction list.
#     """
#     try:
#         # Gap 3 fix: use code_sandbox (AST-validated exec) not raw exec
#         raw_transactions = execute_extraction_code(extraction_code, full_text)

#         if not isinstance(raw_transactions, list):
#             raise ValueError(
#                 f"Extraction returned {type(raw_transactions)}, expected List[Dict]."
#             )
        
#         logger.info("Code extraction success: %d transactions.", len(raw_transactions))
#         return raw_transactions

#     except Exception as e:
#         logger.error("Code extraction failed: %s", e)
#         raise RuntimeError(f"LLM extraction execution failed: {e}")


# # ═══════════════════════════════════════════════════════════
# # HELPERS
# # ═══════════════════════════════════════════════════════════

# def _strip_markdown(content: str) -> str:
#     """
#     Extract only the Python function from LLM output.

#     Handles three output shapes:
#       1. Wrapped in ```python ... ``` fences
#       2. Step 1 analysis prose followed by bare function (two-step prompt output)
#       3. Bare function only (old prompt style)

#     In all cases returns only the text starting from
#     'def extract_transactions' to end of output.
#     """
#     raw = content.strip()

#     # Case 1 — markdown fences present: pull the block containing the function
#     if "```" in raw:
#         parts = raw.split("```")
#         for part in parts:
#             if "def extract_transactions" in part:
#                 raw = part.strip()
#                 if raw.lower().startswith("python"):
#                     raw = raw[6:].strip()
#                 break

#     # Cases 2 & 3 — find where the function starts and discard everything before it
#     # This handles Step 1 prose sitting above the function
#     fn_marker = "def extract_transactions"
#     idx = raw.find(fn_marker)
#     if idx > 0:
#         # Content before function (Step 1 analysis) — strip it
#         raw = raw[idx:]
#     elif idx == -1:
#         # Function not found — return as-is and let exec() raise a clear error
#         logger.warning("_strip_markdown: 'def extract_transactions' not found in LLM output.")

#     return raw.strip()

"""
services/extraction_service.py
──────────────────────────────
STEP 4 — Generate extraction code via LLM (Claude/OpenRouter/9router)
and execute it safely against document text.

This module is the thin orchestrator. All document-family-specific
prompts live in services/prompts/*.py.
"""

import re
import logging
from typing import List, Dict, Any

from services.code_gen_client import get_code_gen_client
from services.prompts import get_prompt
from services.code_sandbox import execute_extraction_code, validate_code, clean_llm_code

logger = logging.getLogger("ledgerai.extraction_service")


# ═══════════════════════════════════════════════════════════
# GENERATE EXTRACTION CODE VIA LLM
# ═══════════════════════════════════════════════════════════

def generate_extraction_logic_llm(
    identifier_json: dict,
    text_sample: str,
) -> str:
    """
    Generates extraction code using the family-specific prompt from services/prompts/.
    Uses Claude (via Anthropic/OpenRouter/9router) for better code quality.
    Returns validated Python code string containing extract_transactions().
    """
    document_family = identifier_json.get("document_family", "BANK_ACCOUNT_STATEMENT")

    prompt = get_prompt(document_family, identifier_json, text_sample)

    logger.info(
        "Generating extraction code: family=%s prompt_len=%d",
        document_family, len(prompt),
    )

    # Get the configured code generation client (Claude via Anthropic/OpenRouter/9router)
    code_gen_client = get_code_gen_client()

    # Generate code with retry logic built into the client
    content = code_gen_client.generate(prompt, max_retries=3)

    if not content:
        raise ValueError("LLM returned empty extraction code.")

    raw_output = _strip_markdown(content)

    # Validate AST before returning — reject dangerous code immediately
    validation_error = validate_code(raw_output)
    if validation_error:
        raise ValueError(f"Generated code failed security validation: {validation_error}")

    logger.info("Generated + validated code: %d chars.", len(raw_output))
    return raw_output



# ═══════════════════════════════════════════════════════════
# EXECUTE EXTRACTION CODE
# ═══════════════════════════════════════════════════════════

def extract_transactions_using_logic(
    full_text: str,
    extraction_code: str,
) -> List[Dict]:
    """
    Execute LLM-generated Python code safely via code_sandbox,
    Returns cleaned transaction list.
    """
    try:
        # Gap 3 fix: use code_sandbox (AST-validated exec) not raw exec
        raw_transactions = execute_extraction_code(extraction_code, full_text)

        if not isinstance(raw_transactions, list):
            raise ValueError(
                f"Extraction returned {type(raw_transactions)}, expected List[Dict]."
            )
        
        logger.info("Code extraction success: %d transactions.", len(raw_transactions))
        return raw_transactions

    except Exception as e:
        logger.error("Code extraction failed: %s", e)
        raise RuntimeError(f"LLM extraction execution failed: {e}")


# ═══════════════════════════════════════════════════════════
# HELPERS
# ═══════════════════════════════════════════════════════════

def _strip_markdown(content: str) -> str:
    """
    Extract only the Python function from LLM output.

    Handles three output shapes:
      1. Wrapped in ```python ... ``` fences
      2. Step 1 analysis prose followed by bare function (two-step prompt output)
      3. Bare function only (old prompt style)

    In all cases returns only the text starting from
    'def extract_transactions' to end of output.
    """
    raw = content.strip()

    # Case 1 — markdown fences present: pull the block containing the function
    if "```" in raw:
        parts = raw.split("```")
        for part in parts:
            if "def extract_transactions" in part:
                raw = part.strip()
                if raw.lower().startswith("python"):
                    raw = raw[6:].strip()
                break

    # Cases 2 & 3 — find where the function starts and discard everything before it
    # This handles Step 1 prose sitting above the function
    fn_marker = "def extract_transactions"
    idx = raw.find(fn_marker)
    if idx > 0:
        # Content before function (Step 1 analysis) — strip it
        raw = raw[idx:]
    elif idx == -1:
        # Function not found — return as-is and let exec() raise a clear error
        logger.warning("_strip_markdown: 'def extract_transactions' not found in LLM output.")

    return raw.strip()