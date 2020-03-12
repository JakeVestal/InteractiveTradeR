#' Request Market Data
#'
#' @description 
#' Fetch live (or 15-minute delayed) market data.
#'
#' @details 
#' attributes `canAutoExecute`, `pastLimit`, `preOpen` are unnecessary in
#' InteractiveTradeR because you can easily create your own flags that accept
#' the `trading_floor$pit` data as input. The aren't that much use anyway, and
#' are mostly holdovers from earlier versions & systems.
#'
#' @param data_name
#' Character vector of length 1. An identifier for the market data subscription.
#' Good choices could be the contract's symbol, conId, or description (if
#' combo). All market data received by a mkt_data subscription will be stored in
#' the \strong{mkt_data} environment under the name \emph{data_name}. Therefore,
#' no two mkt_data subscriptions may have identical \emph{data_name}s.
#'
#' @inheritParams req_account_summary
#' @inheritParams req_market_data_type
#' @inheritParams place_order
#'
#' @param snapshot
#' some biz
#'
#' @param regulatorySnapshot
#' some other biz
#' 
#' @param genericTickList
#' moar biz
#'
# #' @inherit cancel_mkt_data examples
#' @family market data
#' @export
#'
req_mkt_data <- function(
  contract,
  data_name,
  mktDataType        = "REALTIME",
  genericTickList    = NULL,
  snapshot           = TRUE,
  regulatorySnapshot = FALSE,
  channel            = NULL
){
  
  if(is.null(channel) && !(snapshot || regulatorySnapshot)){
    usethis::ui_oops(
      paste0(
        crayon::bold("req_mkt_data"),
        "() may not be called in SYNC mode (",
        crayon::italic("channel"),
        " = NULL) unless\n",
        crayon::italic("snapshot"),
        " or ",
        crayon::italic("regulatorySnapshot"),
        " is TRUE."
      )
    )
    return(invisible())
  }

  sock             <- select_sock_for_api_fun()
  market_data_type <- format_market_data_type()
  subscriptions    <- get("subscriptions")
  
  if(!identical(attr(sock, "mktDataType"), market_data_type)){
    req_market_data_type(market_data_type, sock)
  }

  req_id <- fetch_and_bump("mkt_data")

  if(missing(data_name)){
    if(any(names(contract) == "symbol")){
      data_name <- paste0(req_id, "_", contract["symbol"])
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
    sock_seek(
      element_names   = "",
      socket          = sock,
      success_element = simple_encode(
        functionary$incoming_msg_codes$TICK_SNAPSHOT_END
      )
    )
    if(is.null(channel)){
      cancel_mkt_data(data_name)
      return(mkt_data[[data_name]] %>% mget(ls(.), envir = .))
    }
  } else if(regulatorySnapshot){
    usethis::ui_oops("Regulatory Snapshot not yet enabled")
  }

  return(invisible())

}
