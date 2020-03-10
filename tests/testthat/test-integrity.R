context("Integrity")

test_that(
  "Outgoing & Incoming msg codes in functionary have no elements in common.",
  expect_false(
    any(
      names(
        functionary$outgoing_msg_codes
      ) %in% names(
        functionary$incoming_msg_codes
      )
    )
  )
)

test_that(
  paste0(
    "The REQ_* calls that don't have a corresponding matching value in ",
    "functionary$incoming_msg_codes are the ones expected."
  ),
  expect_identical(
    setdiff(
      gsub(
        pattern     = "^REQ_",
        replacement = "",
        x           = functionary$function_calls_py[
          grep("^REQ_", functionary$function_calls_py)
          ]
      ),
      names(
        functionary$incoming_msg_codes
      )
    ),
    c(
      "MKT_DATA",              "OPEN_ORDERS",        "ACCT_DATA",
      "EXECUTIONS",            "IDS",                "MKT_DEPTH",
      "AUTO_OPEN_ORDERS",      "ALL_OPEN_ORDERS",    "FA",
      "SCANNER_SUBSCRIPTION",  "CALC_IMPLIED_VOLAT", "CALC_OPTION_PRICE",
      "GLOBAL_CANCEL",         "POSITIONS",          "POSITIONS_MULTI",
      "ACCOUNT_UPDATES_MULTI", "SEC_DEF_OPT_PARAMS", "MATCHING_SYMBOLS",
      "TICK_BY_TICK_DATA",     "COMPLETED_ORDERS"
    )
  )
)

test_that(
  "None of the constituents in the functionary's collate map are duplicates.",
  expect_false(
    any(
      duplicated(
        unlist(functionary$collate_map)
      )
    )
  )
)

test_that(
  "Every incoming message code as an entry in ib_polish.",
  # Add in the verifies later if necessary.
  expect_identical(
    setdiff(
      functionary$incoming_msg_codes %>% names() %>% sort(),
      functionary$ib_polish %>% names() %>% sort()
    ),
    c(
      "VERIFY_AND_AUTH_COMPLETED", "VERIFY_AND_AUTH_MESSAGE_API",
      "VERIFY_COMPLETED",          "VERIFY_MESSAGE_API"
    )
  )
)

test_that(
  "All functionary contract_args appear as vars in the contract env.",
  expect_true(
    all(
      functionary$contract_args$req_contract_details %in%
        ls(functionary$contract_vars$Contract)
    )
  )
)

test_that(
  "All functionary contract_args appear as vars in the contract env.",
  expect_true(
    all(
      functionary$combo_leg_args$req_mkt_data %in%
        ls(functionary$contract_vars$ComboLeg)
    )
  )
)
