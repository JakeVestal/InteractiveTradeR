#' Request Option Parameters (strike, expiry, etc) by Security
#'
#' @description
#' Use this function to request the strike prices and expiry dates for options
#' that are available for a specified underlying contract. While the same
#' information could be obtained via \link{req_contract_details}(),
#' \code{req_sec_def_opt_param}() is not subject to data pacing (throttling) and
#' is optimized for retreiving strike prices & expiry dates.
#'
#' @param underlyingSymbol
#' Character, length 1. Symbol of the underlying contract, e.g., "AAPL".
#'
#' @param futFopExchange
#' Character, length 1. Exchange where fetched options trade, e.g., "NASDAQOM"
#'
#' @param underlyingSecType
#' Character, length 1. The type of the underlying security. See \emph{SecType}
#' in the \link{contract} documentation.
#'
#' @param underlyingConId
#' Character or numeric, length 1. The \code{conId} of the underlying contract.
#'
#' @inheritParams req_current_time
#'
#' @return
#' A 1-row \link[tibble]{tibble} with the following columns:
#' \itemize{
#'   \item \strong{exchange} <chr>: See \link{contract}
#'   \item \strong{underlyingConId} <numeric>: Unique numeric \code{conId} of
#'   underlying contract; See \link{contract}
#'   \item \strong{tradingClass} <chr>: See \link{contract}
#'   \item \strong{multiplier} <numeric>: See \link{contract}
#'   \item \strong{expirations} <list of POSIXt Date Objects>: Expiry dates of
#'   put & call options traded on \code{exchange} for the contract whose
#'   \code{conId} is passed as argument \code{underlyingConId}
#'   \item \strong{expirations} <list of Numeric Objects>: Strike prices of put
#'   & call options traded on \code{exchange} for the contract whose
#'   \code{conId} is passed as argument \code{underlyingConId}
#' }
#'
#' @export
#'
#' @example inst/examples/req_sec_def_opt_params_ex.R
#' @family asset info
#' @export
#'
req_sec_def_opt_params <- function(
  underlyingSymbol  = "",
  futFopExchange    = "",
  underlyingSecType = "",
  underlyingConId   = "",
  channel           = NULL
){

  sock   <- select_sock_for_api_fun()
  req_id <- fetch_and_bump("sec_def_opt_params")

  req_sec_def_opt_params_msg <- ib_encode_raw_msg(
    c(
      functionary$outgoing_msg_codes$REQ_SEC_DEF_OPT_PARAMS,
      req_id,
      underlyingSymbol,
      futFopExchange,
      underlyingSecType,
      underlyingConId
    )
  )

  writeBin(object = req_sec_def_opt_params_msg, con = sock, endian = "big")

  sec_def_opt_params <- sock_seek(
    element_names   = "SECURITY_DEFINITION_OPTION_PARAMETER",
    socket          = sock,
    success_element = simple_encode(
      c(
        functionary$incoming_msg_codes$
          SECURITY_DEFINITION_OPTION_PARAMETER_END,
        req_id
      )
    ),
    stop_early = simple_encode(
      c(
        functionary$incoming_msg_codes$ERR_MSG,
        2,
        req_id
      )
    )
  )
  
  ib_validate(sec_def_opt_params)

}
