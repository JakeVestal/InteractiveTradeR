#' Request Account Updates Multi
#'
#' This function either fetches or sets up a subscription to receive the data
#' that appear in the
#' \href{https://www.interactivebrokers.com/en/software/tws/usersguidebook/realtimeactivitymonitoring/the_account_window.htm}{Account
#' Window} window of Trader Workstation. `req_account_updates_multi`() may be
#' used to set up multiple simultaneous subscriptions to many accounts / model
#' codes. IB's documentation may be found on the
#' \href{https://interactivebrokers.github.io/tws-api/account_updates.html}{Account
#' Updates} page.
#'
#' @param account
#' Character vector of length 1 containing a valid **account code**.
#'
#' @param modelCode
#' Character vector of length 1 containing a valid **model code**.
#'
#' @param ledgerAndNLV
#' Boolean of length 1, defaults to `FALSE`. If TRUE, then the created
#' subscriptions are treated as "lightweight requests", meaning that the data
#' returned for the account or model codes supplied will include only currency
#' positions (as opposed to both account values and currency positions).
#'
#' @inheritParams req_current_time
#'
#' @inheritParams req_account_summary
#'
#' @details
#' **"All" Option**: If `account` is not specified,
#' `req_account_updates_multi`() will default to `account = "All"`, which will
#' return data for all accounts accessible by the signed-in user. This option is
#' not available for users who manage more than 50 accounts.
#'
#' **Models**: Model Portfolios, or "Models" for short, can be though of as
#' named templates that will allocate funds across investment products in a
#' specified manner. When passed to `req_account_updates_multi`() as
#' `model_code`, they behave like stocks -- they have postions, market prices,
#' values, etc. To use Model Portfolios you need to have a Financial Advisor
#' account, and the Model Portfolios feature needs to be activated. Learn more
#' on \href{https://www.interactivebrokers.com/en/index.php?f=20917}{Interactive
#' Brokers' Model Portfolios page}.
#'
#' **No Positions**: If you call `req_account_updates_multi`() on an account
#' that has cash but no positions, then data will be sent to the socket once
#' initially, but will not update thereafter.
#'
#' @inheritSection req_account_summary ACCOUNTS Treasury Object
#'
#' @inherit req_account_summary return
#'
#' @seealso
#' * \link{cancel_account_updates_multi}() for cancelling existing account
#' updates subscriptions.
#'
#' @export
#'
#' @example inst/examples/account_updates_multi_ex.R
#'
req_account_updates_multi <- function(
  account      = "All",
  modelCode    = NULL,
  ledgerAndNLV = FALSE,
  channel      = NULL,
  return_data  = is.null(channel)
){

  sock     <- select_sock_for_api_fun()
  req_id   <- fetch_and_bump("account_updates_multi")
  req_name <- paste(c(account, modelCode), collapse = ":")

  if(any(subscriptions$account_updates_multi$req_name == account)){
    usethis::ui_oops("An identical subscription already exists!")
    return(invisible(FALSE))
  }

  if(is.null(channel)){
    on.exit(
      writeBin(
        object = ib_encode_raw_msg(
          c(
            functionary$outgoing_msg_codes$
              CANCEL_ACCOUNT_UPDATES_MULTI,
            functionary$function_versions_py$
              cancelAccountUpdatesMulti,
            req_id

          )
        ),
        con    = sock,
        endian = "big"
      ),
      add   = TRUE,
      after = FALSE
    )
  } else {
    subscribe(
      fun_name = "account_updates_multi",
      req_name = req_name,
      req      = list(
        account      = account,
        modelCode    = modelCode,
        ledgerAndNLV = ledgerAndNLV
      ),
      sock     = sock,
      req_id   = req_id
    )
  }

  req_account_updates_multi_msg <- ib_encode_raw_msg(
    c(
      functionary$outgoing_msg_codes$
        REQ_ACCOUNT_UPDATES_MULTI,
      functionary$function_versions_py$
        reqAccountUpdatesMulti,
      req_id,
      account,
      make_field_handle_empty(modelCode),
      as.numeric(ledgerAndNLV)
    )
  )

  writeBin(
    object = req_account_updates_multi_msg,
    con    = sock,
    endian = "big"
  )

  account_updates_multi <- tryCatch(
    sock_seek(
      element_names   = "ACCOUNTS",
      socket          = sock,
      success_element = simple_encode(
        functionary$incoming_msg_codes$
          ACCOUNT_UPDATE_MULTI_END
      )
    ),
    error = function(e){
      if(grepl("TIMEOUT", account_updates_multi$e)){
        usethis::ui_oops("No account update received.")
        usethis::ui_info("Try waiting 3 minutes before your next attempt.")
      } else {
        stop(e)
      }
    }
  )

  assign(
    "ACCOUNTS",
    value = structure(
      functionary$ib_update$ACCOUNTS(
        account_updates_multi
      ),
      last_updated = Sys.time()
    ),
    envir = get("treasury")
  )

  if(return_data){
    account_updates_multi
  } else {
    invisible(
      tibble::is_tibble(account_updates_multi) && (
        is.null(channel) ||
          any(subscriptions$account_updates_multi$req_name == req_name)
      )
    )
  }

}
