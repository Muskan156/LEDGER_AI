"""
services/account_detector.py
─────────────────────────────
Account linking for uploaded documents.

Account detection from PDF text is intentionally NOT done here.
Instead, all accounts the user has already added are fetched and
presented in a dropdown on the Review screen. The user picks which
account the uploaded document belongs to.

Public functions:
  get_user_accounts(user_id)
      → fetches all active accounts + their identifiers for this user
      → returns list of {account_id, account_name, institution_name,
                         account_number_last4, card_last4} dicts

  link_document_to_account(document_id, account_id)
      → sets documents.account_id = account_id
"""

import logging
from db.connection import get_client

logger = logging.getLogger("ledgerai.account_detector")


def get_user_accounts(user_id: str) -> list:
    """
    Fetch all active accounts for this user, joined with their
    account_identifiers so the dropdown can show bank name + last 4.

    Returns list of dicts:
    [
        {
            "account_id":            42,
            "account_name":          "SBI Savings",
            "institution_name":      "STATE BANK OF INDIA",
            "account_number_last4":  "1234",
            "card_last4":            None,
            "account_type":          "ASSET",
        },
        ...
    ]
    Empty list if none found or on error.
    """
    try:
        sb = get_client()

        # Only fetch user-added bank/financial accounts (is_system_generated=false).
        # The accounts table also contains system COA accounts (Bank Accounts,
        # Base Salary, etc. with is_system_generated=true) which must NOT appear
        # in this dropdown — they are chart-of-accounts categories, not real accounts.
        accounts_result = (
            sb.table("accounts")
            .select("account_id, account_name, account_type")
            .eq("user_id", user_id)
            .eq("is_active", True)
            .eq("is_system_generated", False)
            .order("account_name")
            .execute()
        )
        accounts = accounts_result.data or []

        if not accounts:
            logger.info("get_user_accounts: no user-added accounts for user=%s", user_id)
            return []

        account_ids = [a["account_id"] for a in accounts]

        # Fetch identifiers — drop is_primary filter because some valid accounts
        # may not have is_primary=true set, especially those added via older flows.
        idents_result = (
            sb.table("account_identifiers")
            .select(
                "account_id, institution_name, "
                "account_number_last4, card_last4, account_number_masked"
            )
            .in_("account_id", account_ids)
            .eq("is_active", True)
            .execute()
        )
        # Deduplicate by account_id — keep first identifier per account
        idents = {}
        for row in (idents_result.data or []):
            aid = row["account_id"]
            if aid not in idents:
                idents[aid] = row

        result = []
        for acct in accounts:
            aid   = acct["account_id"]
            ident = idents.get(aid, {})
            last4 = ident.get("account_number_last4") or ident.get("card_last4")
            result.append({
                "account_id":            aid,
                "account_name":          acct["account_name"],
                "account_type":          acct.get("account_type"),
                "institution_name":      ident.get("institution_name"),
                "account_number_last4":  ident.get("account_number_last4"),
                "account_number_masked": ident.get("account_number_masked"),
                "card_last4":            ident.get("card_last4"),
            })

        logger.info(
            "get_user_accounts: user=%s  found=%d user-added accounts",
            user_id, len(result),
        )
        return result

    except Exception as exc:
        logger.warning("get_user_accounts: failed — %s", exc)
        return []


def link_document_to_account(document_id: int, account_id: int) -> None:
    """
    Set documents.account_id for this document.
    Called when user selects an account from the dropdown.
    """
    try:
        sb = get_client()
        sb.table("documents").update(
            {"account_id": account_id}
        ).eq("document_id", document_id).execute()
        logger.info(
            "link_document_to_account: doc=%s → account_id=%s",
            document_id, account_id,
        )
    except Exception as exc:
        logger.warning(
            "link_document_to_account: failed for doc=%s: %s", document_id, exc
        )
        raise