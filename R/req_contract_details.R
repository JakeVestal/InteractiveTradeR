#' Request Contract Details
#'
#' @description
#' Fetch a wealth of information about contracts at Interactive Brokers that are
#' found to match parameters supplied in the \code{contract} argument.
#'
#' @inheritParams req_current_time
#'
#' @eval contract_param("req_contract_details", functionary$big_function_args$contract_args$req_contract_details)
#'
#' @details
#' \strong{IB's documentation} describes each column variable that can appear in
#' the output of \strong{InteractiveTradeR}'s implementation of
#' \code{req_contract_details}() in the
#' \href{https://interactivebrokers.github.io/tws-api/classIBApi_1_1ContractDetails-members.html}{ContractDetails
#' Members} section. A list of the contract parameters that may be passed in via
#' the \code{contract} object can be found in IB's
#' \href{https://interactivebrokers.github.io/tws-api/classIBApi_1_1Contract.html}{Contract
#' Class Reference}.
#' 
#' \strong{Pacing and Large Queries}: \code{req_contract_details}() is able to
#' send queries that involve the transfer of high amounts of data in cases where
#' many contracts are found to match the selection criteria. As a consequence,
#' Interactive Brokers may \strong{pace} requests made by this function by
#' placing similar or identical requests on hold for one minute, with the amount
#' of time increasing each time subsequent similar/identical requests are made.
#'
#' The exact criteria regarding what constitutes a "similar" request, and the
#' rules governing pacing behavior, are not published. However, by following
#' three general rules of thumb, pacing should not be a problem when using
#' \code{req_contract_details}() in InteractiveTradeR:
#'
#' \itemize{
#'   \item \strong{In SYNC mode (} \code{channel = NULL}) \strong{, bump up the
#'   timeout parameter for large queries with \link{sync_timeout}()}. If a large
#'   number of contracts are found to match the parameters in \code{contract},
#'   then the function might return an error if called with the default timeout
#'   because it needs bit more time than the default 5 seconds in order to
#'   complete. Try using \code{sync_timeout(10)} or \code{sync_timeout(15)}.
#'   \item \strong{In Shiny apps or scripts whose execution can't/shouldn't be
#'   held up while \code{req_contract_details}() executes, use} \link{async}
#'   \strong{mode}. Have your script or app look for the updated data in
#'   \code{treasury$CONTRACT_DETAILS} at a later time.
#'   \item \strong{Subsequent calls to \code{req_contract_details}() will
#'   overwrite the} \code{CONTRACT_DETAILS} \strong{treasury object}. In other
#'   words, there is at most one \code{CONTRACT_DETAILS} object in the treasury
#'   at any given time.
#'   \item \strong{Using \code{conId} Only}: If the object passed in as
#'   \code{contract} has length = 1, then \code{req_contract_details}() will
#'   assume that \code{contract} contains a \code{conId}, which is sufficient to
#'   specify a unique contract. This shortcut can help speed up usage.
#' }
#'
#' @section \code{CONTRACT_DETAILS} Treasury Object:
#' The \code{CONTRACT_DETAILS} object is a \link[tibble]{tibble} in which each
#' row represents a contract found to match the supplied parameters. The columns
#' of \code{CONTRACT_DETAILS} are described in the
#' \href{https://interactivebrokers.github.io/tws-api/classIBApi_1_1ContractDetails.html}{Contract
#' Details Class Reference} page in IB's online documentation \strong{with one
#' minor difference}: in \strong{InteractiveTradeR}, the parameters
#' \code{validExchanges} and \code{marketRuleIds} are grouped together in the
#' column \code{exchange_info} to make it easy to read which market rule applies
#' to which exchange.
#'
#' @return
#' A \code{CONTRACT_DETAILS} if sync mode; otherwise NULL is returned invisibly
#' and the query results will be updated in \code{treasury$CONTRACT_DETAILS}
#' when \link{read_sock_drawer}() is called at a later time.
#'
#' @example inst/examples/req_contract_details_ex.R
#' @family asset info
#' @export
#'
req_contract_details <- function(contract = NULL, channel  = NULL){
  
  sock   <- select_sock_for_api_fun()
  req_id <- fetch_and_bump("contract_details")
  
  if(length(contract) == 1){
    contract <- c(conId = unlist(contract, use.names = FALSE))
  }
  
  req_contract_details_msg <- mget(
    setdiff(
      functionary$big_function_args$contract_args$
        req_contract_details,
      c(tryCatch(names(contract), error = function(e){NULL}), ls())
    ),
    envir = functionary$contract_vars$Contract
  ) %>%
    c(contract) %$% {
      c(
        functionary$outgoing_msg_codes$REQ_CONTRACT_DATA,
        functionary$function_versions_py$reqContractDetails,
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
  
  if(is.null(channel)){
    contract_details <- sock_seek(
      element_names   = c("BOND_CONTRACT_DETAILS", "CONTRACT_DETAILS"),
      socket          = sock,
      success_element = simple_encode(
        c(
          functionary$incoming_msg_codes$CONTRACT_DATA_END,
          1,
          req_id
        )
      ),
      stop_early      = simple_encode(
        c(
          functionary$incoming_msg_codes$ERR_MSG,
          "2",
          req_id
        )
      )
    ) %>%
      purrr::compact() %>%
      purrr::flatten() %>%
      tibble::as_tibble()
    
    ib_validate(contract_details)
  }
  
}
