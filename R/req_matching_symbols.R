#' Request Matching Symbols
#'
#' Search Interactive Brokers' database for stocks (and \code{stocks} only)
#'  by specifying a search string \code{pattern}. \code{pattern} may be a ticker,
#'  e.g. "AAPL", or part of a stock's name, e.g. "App".
#'
#' @inheritParams req_current_time
#'
#' @param pattern Character vector of length 1. The search string for which
#'   matching stocks are sought. May be a ticker, e.g. "IBM", or part of a
#'   stock's name, e.g. "International". Not case sensitive.
#'
#' @details
#' \strong{Space out calls to \code{req_matching_symbols}() by at least 1
#' second.} IB will only allow at most 1 call to \code{req_matching_symbols} per
#' second \emph{per user}, meaning that even if you run seperate searches for
#' matching symbols on different sockets at the same time, you'll get an error
#' telling you that IB is busy processing the request for a different client.
#'
#' @return
#' A \link{tibble} object in which each row corresponds to an asset whose name
#' or ticker matched the \code{pattern} supplied. The tibble's columns are:
#' \itemize{
#'   \item \code{con_id} <chr>: Interactive Brokers' unique numeric ID
#'   \item \code{symbol} <chr>: Exchange symbol (i.e., ticker symbol)
#'   \item \code{sec_type} <chr>: Asset class ("STK" for stock)
#'   \item \code{primary_exchange} <chr>: Financial exchange on which most of
#'   the asset's trade volume takes place (NYSE, ARCA, etc).
#'   \item \code{currency} <chr>: Currency in which trades are made.
#'   \item \code{asset_types} <list>: A list of other financial products
#'   (options, bonds, warrants, etc) that derive from the asset and are
#'   available to trade through Interactive Brokers.
#' }
#'
#' @example inst/examples/req_matching_symbols_ex.R
#' 
#' @export
#'
req_matching_symbols <- function(pattern, channel = NULL){

  sock   <- select_sock_for_api_fun()
  req_id <- fetch_and_bump("matching_symbols")

  req_matching_symbols_msg <- ib_encode_raw_msg(
    c(
      InteractiveTradeR::functionary$outgoing_msg_codes$REQ_MATCHING_SYMBOLS,
      req_id,
      pattern
    )
  )

  writeBin(
    object = req_matching_symbols_msg,
    con    = sock,
    endian = "big"
  )

  matching_symbols <- sock_seek(
    element_names   = "MATCHING_SYMBOLS",
    socket          = sock,
    success_element = simple_encode(
      c(
        InteractiveTradeR::functionary$incoming_msg_codes$SYMBOL_SAMPLES,
        req_id
      )
    )
  )

  matching_symbols

}
