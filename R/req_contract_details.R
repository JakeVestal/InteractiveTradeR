#' Request Contract Details
#'
#' @description
#' Fetch a wealth of information about contracts at Interactive Brokers that are
#' found to match parameters supplied in the `contract` argument.
#'
#' @inheritParams req_current_time
#'
#' @eval contract_param("req_contract_details", InteractiveTradeR::functionary$big_function_args$contract_args$req_contract_details)
#'
#' @param block
#' Logical, default = TRUE. `block` *must* be TRUE if `channel` is NULL. If
#' `block` == TRUE, then `req_contract_details`() will hold up programmatic
#' execution until it completes. Otherwise, `req_contract_details`() will write
#' the request for contract details to a socket and will return NULL, invisibly,
#' without reading any data. The contract details data, when it arrives from IB,
#' can then be picked up later when `read_sock_drawer`() is called.
#'
#' @details
#' **Contract Details**: A list of each column variable that can appear in the
#' output can be found here in the
#' \href{https://interactivebrokers.github.io/tws-api/classIBApi_1_1ContractDetails-members.html}{ContractDetails
#' Members} section of IB's documentation.
#'
#' **Contract Parameters**: A list of the contract parameters that may be passed
#' in via the `contract` object can be found in IB's
#' \href{https://interactivebrokers.github.io/tws-api/classIBApi_1_1Contract.html}{Contract
#' Class Reference}.
#'
#' **Pacing and Large Queries**: `req_contract_details`() is able to send
#' queries that involve the transfer of high amounts of data in cases where many
#' contracts are found to match the selection criteria. As a consequence,
#' Interactive Brokers may **pace** requests made by this function by placing
#' similar or identical requests on hold for one minute, with the amount of time
#' increasing each time subsequent similar/identical requests are made.
#'
#' The exact criteria regarding what constitutes a "similar" request, and the
#' rules governing pacing behavior, are not published. However, by following
#' three general rules of thumb, pacing should not be a problem when using
#' `req_contract_details`() in InteractiveTradeR:
#'
#' * **In SYNC mode (`channel` = NULL), bump up the timeout parameter for large
#' queries with \link{sync_timeout}()**. If a large number of contracts are
#' found to match the parameters in `contract`, then the function might return
#' an error if called with the default timeout because it needs bit more time
#' than the default 5 seconds in order to complete. Try using
#' `sync_timeout(10)` or `sync_timeout(15)`.
#'
#' * **In Shiny apps or scripts whose execution can't/shouldn't be held up while
#' `req_contract_details`() executes, use `block = FALSE`**. Specify "async" or
#' a named socket for `channel`. Have your script or app look for the updated
#' data in treasury$CONTRACT_DETAILS.
#'
#' * **Subsequent calls to `req_contract_details`() with `block = FALSE` will
#' overwrite the CONTRACT_DETAILS treasury object**. In other words, there is at
#' most one CONTRACT_DETAILS object in the treasury at any given time.
#'
#' **Using `conId` Only**: If the object passed in as `contract` has length = 1,
#' then `req_contract_details`() will assume that `contract` contains a `conId`,
#' which is sufficient to specify a unique contract. This shortcut can help
#' speed up usage.
#'
#' @section CONTRACT_DETAILS Treasury Object:
#' If `block == FALSE`, `req_contract_details`() will write its data request as
#' informed by the `channel` argument and will return NULL without reading any
#' incoming data from IB. At a later time, when the data arrives from the
#' server, the **CONTRACT_DETAILS** object will be updated in the treasury.
#'
#' The CONTRACT_DETAILS object is a \link[tibble]{tibble} in which each row
#' represents a contract found to match the supplied parameters. The columns of
#' CONTRACT_DETAILS are described in the
#' \href{https://interactivebrokers.github.io/tws-api/classIBApi_1_1ContractDetails.html}{Contract
#' Details Class Reference} page in IB's online documentation **with one minor
#' difference**: in InteractiveTradeR, the parameters "`validExchanges`" and
#' "`marketRuleIds`" are grouped together in the column "`exchange_info`" to
#' make it easy to read which market rule applies to which exchange.
#'
#' @return
#' If `block = TRUE`, a CONTRACT_DETAILS object will be returned; otherwise NULL
#' is returned invisibly.
#'
#' @export
#'
#' @example inst/examples/req_contract_details_ex.R
#'
req_contract_details <- function(
  contract = NULL,
  channel  = NULL,
  block    = TRUE
){
  
  if(is.null(channel) && !block){
    usethis::ui_oops(
      paste0(
        "To use ",
        crayon::italic("block = FALSE"),
        " in ",
        crayon::bold("req_contract_details"),
        "(), the function must be called with\n",
        crayon::italic("channel = \"async\""),
        " or ",
        crayon::italic("channel = (a named socket)"),
        "."
      )
    )
    usethis::ui_info(
      paste0(
        "In other words, if ",
        crayon::italic("block == FALSE"),
        ", channel can't be NULL."
      )
    )
    return(invisible())
  }
  
  sock   <- select_sock_for_api_fun()
  req_id <- fetch_and_bump("contract_details")
  
  if(length(contract) == 1){
    contract <- c(conId = unlist(contract, use.names = FALSE))
  }
  
  req_contract_details_msg <- mget(
    setdiff(
      InteractiveTradeR::functionary$big_function_args$contract_args$
        req_contract_details,
      c(tryCatch(names(contract), error = function(e){NULL}), ls())
    ),
    envir = InteractiveTradeR::functionary$contract_vars$Contract
  ) %>%
    c(contract) %$% {
      c(
        InteractiveTradeR::functionary$outgoing_msg_codes$REQ_CONTRACT_DATA,
        InteractiveTradeR::functionary$function_versions_py$reqContractDetails,
        get("req_id"),
        get("conId"),
        get("symbol"),
        get("secType"),
        get("lastTradeDateOrContractMonth"),
        get("strike"),
        get("right"),
        get("multiplier"),
        get("exchange"),
        get("primaryExchange"),
        get("currency"),
        get("localSymbol"),
        get("tradingClass"),
        as.numeric(as.logical(get("includeExpired"))),
        get("secIdType"),
        get("secId")
      )
    } %>%
    ib_encode_raw_msg()
  
  writeBin(
    object = req_contract_details_msg,
    con    = sock,
    endian = "big"
  )
  
  if(block){
    sock_seek(
      element_names   = c("BOND_CONTRACT_DETAILS", "CONTRACT_DETAILS"),
      socket          = sock,
      success_element = simple_encode(
        c(
          InteractiveTradeR::functionary$incoming_msg_codes$CONTRACT_DATA_END,
          1,
          req_id
        )
      ),
      stop_early      = simple_encode(
        c(
          InteractiveTradeR::functionary$incoming_msg_codes$ERR_MSG,
          "2",
          req_id
        )
      )
    ) %>%
      purrr::compact() %>%
      purrr::flatten() %>%
      tibble::as_tibble()
  }
  
}
