#' Request Market Data
#'
#' @description 
#' Fetch live (or 15-minute delayed) market data.
#'
#' @details 
#' The attributes \emph{canAutoExecute}, \emph{pastLimit}, and \emph{preOpen}
#' are unnecessary in InteractiveTradeR because you can easily create your own
#' flags that accept market data as input. The aren't that much use anyway, and
#' are mostly holdovers from earlier versions & systems.
#'
#' @param data_name
#' Character vector of length 1. 
#' \strong{If not supplied}, \code{req_mkt_data()} will use the data name
#' "\strong{\emph{X}_{symbol}}" where \emph{X} is a simple counting integer and
#' \emph{symbol} is the value of the \emph{symbol} parameter in the
#' \emph{contract} object.
#' \strong{If supplied}, \code{data_name} becomes Your own personal identifier
#' for the market data subscription. Good choices might be the contract's
#' symbol, conId, or description (if combo). All market data received by a
#' mkt_data subscription will be stored in the \strong{mkt_data} environment
#' under the name \emph{data_name}. Therefore, no two mkt_data subscriptions may
#' have identical \emph{data_name}s.
#'
#' @inheritParams req_account_summary
#' @inheritParams req_market_data_type
#' @inheritParams place_order
#'
#' @param snapshot
#' If set to TRUE, the subscription created by \code{req_mkt_data()} will be
#' destroyed after a complete "snapshot" of market data is obtained for the
#' \emph{contract}. The default is \code{snapshot = FALSE}, which will cause the
#' subscription to stay opened until cancelled by the user making a call to
#' \link{cancel_mkt_data}().
#'
#' @param regulatorySnapshot
#' Not yet implemented, stay tuned
#' 
#' @param genericTickList
#' Not yet implemented, stay tuned
#' 
#' @section No Sync Mode:
#' \code{req_mkt_data()} can't be called in Sync Mode. Instead, this function
#' must be implimented as follows:
#' \enumerate{
#'   \item Create a connection to Interactive Brokers with 
#'     \link{create_new_connections}() if a connection doesn't already exist
#'   \item Call \code{req_mkt_data()} to start a market data subscription
#'   \item Call \link{read_sock_drawer}() to refresh the \link{mkt_data} slate. 
#'     Do this as often as you need to keep \link{mkt_data} up-to-date.
#'   \item Call \link{cancel_mkt_data}() when you're finished 
#' }
#' 
#' If \code{snapshot = TRUE}, then the subscription will destroy itself once a 
#' full market data snapshot has been obtained by \link{read_sock_drawer}(), so 
#' there's no need to call \link{cancel_mkt_data}().
#' 
#' 
#' @section The \link{mkt_data} Slate: 
#' Data fetched by \code{req_mkt_data()} is stored in the \link{mkt_data} slate
#' under the name specified in the \emph{data_name} argument (or the default
#' data name "\strong{\emph{X}_{symbol}}" if no \emph{data_name} not supplied).
#'
#' @inherit cancel_mkt_data examples
#' @family market data
#' @export
#'
req_mkt_data <- function(
  contract,
  data_name,
  channel            = "async",
  mktDataType        = "REALTIME",
  genericTickList    = NULL,
  snapshot           = FALSE,
  regulatorySnapshot = FALSE
){
  
  if(is.null(channel)){
    usethis::ui_oops(
      paste0(
        crayon::bold("req_mkt_data"),
        "() may not be called in SYNC mode."
      )
    )
    return(invisible())
  }
  
  sock             <- select_sock_for_api_fun()
  market_data_type <- format_market_data_type()
  
  if(!identical(attr(sock, "mktDataType"), market_data_type)){
    req_market_data_type(market_data_type, sock)
  }
  
  req_id <- fetch_and_bump("mkt_data")
  
  subscriptions <- get("subscriptions")
  
  if(missing(data_name)){
    if(any(names(contract) == "symbol")){
      data_name <- paste0(
        nrow(subscriptions$mkt_data) %>% {
          if(is.null(.)){
            1
          } else {
            . + 1
          }
        }, 
        "_", 
        contract["symbol"]
      )
    } else {
      data_name <- as.character(req_id)
    }
  }
  
  if(any(subscriptions$mkt_data$req_name == data_name)){
    usethis::ui_oops("An identical subscription already exists!")
    return(invisible(FALSE))
  }
  
  req_mkt_data_msg <- mget(
    setdiff(
      functionary$big_function_args$contract_args$
        req_mkt_data,
      names(contract)
    ),
    envir = functionary$contract_vars$Contract
  ) %>%
    c(contract) %$% {
      c(
        functionary$outgoing_msg_codes$REQ_MKT_DATA,
        functionary$function_versions_py$reqMktData,
        get("req_id"),
        get("conId"),
        get("symbol"),
        if(is.null(nrow(get("comboLegs"))) || nrow(get("comboLegs")) == 0){
          get("secType")
        } else {
          "BAG"
        },
        get("lastTradeDateOrContractMonth"),
        get("strike"),
        get("right"),
        get("multiplier"),
        get("exchange"),
        get("primaryExchange"),
        get("currency"),
        get("localSymbol"),
        get("tradingClass"),
        if(isTRUE(nrow(get("comboLegs")) > 0)){
          c(
            nrow(get("comboLegs")),
            trimws(c(t(get("comboLegs"))))
          )
        },
        if(get("deltaNeutralContract") == 0){
          0
        } else {
          c(
            1,
            get("deltaNeutralContract")["conId"],
            get("deltaNeutralContract")["delta"],
            get("deltaNeutralContract")["price"]
          )
        },
        make_field_handle_empty(get("genericTickList")),
        as.numeric(get("snapshot")),
        as.numeric(get("regulatorySnapshot")),
        ""
      )
    } %>%
    ib_encode_raw_msg()
  
  mkt_data <- get("mkt_data")
  
  if(!any(data_name == ls(mkt_data))){
    assign(
      data_name,
      value = new.env(parent = emptyenv()),
      envir = mkt_data
    )
  }
  
  writeBin(
    object = req_mkt_data_msg,
    con    = sock,
    endian = "big"
  )
  
  subscribe(
    fun_name = "mkt_data",
    req_name = data_name,
    req      = list(
      contract           = contract,
      mktDataType        = market_data_type,
      genericTickList    = genericTickList,
      snapshot           = snapshot,
      regulatorySnapshot = regulatorySnapshot
    ),
    sock     = sock,
    req_id   = req_id
  )
  
  if(snapshot){
    if(is.null(channel)){
      sock_seek(
        element_names   = "",
        socket          = sock,
        success_element = simple_encode(
          functionary$incoming_msg_codes$TICK_SNAPSHOT_END
        )
      )
      return(mkt_data[[data_name]] %>% mget(ls(.), envir = .))
    }
  } else if(regulatorySnapshot){
    usethis::ui_oops("Regulatory Snapshot not yet enabled")
  }
  
  return(invisible())
  
}
