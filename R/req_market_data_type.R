#' Request Market Data Type
#'
#' @description
#' For a specific socket connection, set the market data type to one of
#' \strong{REALTIME} (initial setting for all new sockets), \strong{FROZEN},
#' \strong{DELAYED}, or \strong{DELAYED_FROZEN}. In InteractiveTradeR,
#' \strong{this function should not be called on its own}. Rather, it is used
#' internally by other \emph{req_} functions, such as \link{req_mkt_data}.
#' \code{req_market_data_type}() is included in InteractiveTradeR to cover the
#' hypothetical case in which the user might need it.
#'
#' @param mktDataType
#' Either:
#' \enumerate{
#'   \item String having one of the following values: "\strong{REALTIME}",
#'   "\strong{FROZEN}", "\strong{DELAYED}", or "\strong{DELAYED_FROZEN}".
#'   \item Integer from 1 to 4, inclusive, corresponding respectively to each of
#'   the above choices (e.g., \code{mktDataType = 3} will set the market data
#'   type to "\strong{DELAYED}").
#' }
#'
#' @inheritParams req_current_time
#'
#' @section The Market Data Types:
#' The type of market data retrieved on a given sock can be set to one of the
#' values below. Note that the difference between \strong{REALTIME}, \strong{FROZEN} and
#' \strong{DELAYED_REALTIME}, \strong{DELAYED_FROZEN} is that the
#' \strong{DELAYED} data may be obtained for free on an IB paper account without
#' a data subscription and with no actual funds deposited.
#' \describe{
#'   \item{\strong{REALTIME}:}{Streaming market data (live), fed to the socket
#'   in real time.
#'   \href{https://www.interactivebrokers.com/en/software/am3/am/settings/marketdatasubscriptions.htm}{Market
#'   Data Subscriptions} are required to make use of this setting.}
#'   \item{\strong{FROZEN}:}{Also requires
#'   \href{https://www.interactivebrokers.com/en/software/am3/am/settings/marketdatasubscriptions.htm}{Market
#'   Data Subscriptions}. If utilized, this mode will send the last quote
#'   available for an asset in the event that there is no \strong{REALTIME}
#'   price available at the time of of a call to \code{req_mkt_data}(); for
#'   example, if the call is made outside of trading hours.}
#'   \item{\strong{DELAYED}:}{Does not require any data subscriptions, but will
#'   return data delayed by 15 to 20 minutes.}
#'   \item{\strong{DELAYED_FROZEN}:}{Same behavior as \strong{FROZEN}, except
#'   applies to \strong{DELAYED} data as opposed to \strong{REALTIME} and does
#'   \strong{not} require any data subscriptions.}
#' }
#'
#' Market data types are described in IB's official documentation here:
#' \href{https://interactivebrokers.github.io/tws-api/market_data_type.html}{Market
#' Data Types}.
#'
#' By setting up subscriptions on sockets set to different market types, you may
#' have \strong{multiple market data subscriptions running simultaneously on
#' different market types}. This can be handy in use cases for which
#' \strong{REALTIME} market data is needed for a certain data stream, but
#' \strong{DELAYED} market data will suffice for a different stream for which a
#' subscription is not available/desired.
#'
#' @family market data
#' @export
#'
req_market_data_type <- function(mktDataType, channel = NULL){

  market_data_type <- format_market_data_type()
  sock             <- select_sock_for_api_fun()

  if(any(names(attributes(sock)) == "server_version")){
    attr(
      sock_drawer[[
        names(
          which(
            mget(ls(sock_drawer), envir = sock_drawer) %>%
              vapply(
                attr,
                numeric(1),
                which = "client_id"
              ) == attr(sock, "client_id")
          )
        )
        ]],
      which = "mktDataType"
    ) <- market_data_type
  }

  req_market_data_type_msg <- ib_encode_raw_msg(
    c(
      functionary$outgoing_msg_codes$REQ_MARKET_DATA_TYPE,
      functionary$function_versions_py$reqMarketDataType,
      market_data_type
    )
  )

  writeBin(
    object = req_market_data_type_msg,
    con    = sock,
    endian = "big"
  )

  invisible()

}
