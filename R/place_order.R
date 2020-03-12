#' Place Order
#'
#' @description
#' Sends an order to IB's servers to buy or sell a financial instrument.
#'
#' @eval contract_param("place_order", functionary$big_function_args$contract_args$place_order)
#'
#' @inheritParams req_contract_details
#' 
#' @param order
#' describe that
#' 
#' @param deltaNeutralContract
#' that too
#' 
#' @param softDollarTier
#' and that
#' 
#' @param non_master_override
#' finally that
#'
#' @section Conditional Orders:
#' \href{https://www.interactivebrokers.com/en/index.php?f=584}{Conditional
#' Orders}
#'
#' @section Bracket Orders:
#' \href{https://www.interactivebrokers.com/en/index.php?f=583}{Bracket Orders}
#'
#' @section Attaching (Hedging) Orders:
#' \itemize{
#'   \item \href{https://www.interactivebrokers.com/en/software/tws/usersguidebook/ordertypes/attach_a_beta_hedge_order.htm}{Beta Hedge}: 
#'   \item \href{https://www.interactivebrokers.com/en/software/tws/usersguidebook/ordertypes/attach_a_delta_hedge_order.htm}{Delta Hedge}:
#'   \item \href{https://www.interactivebrokers.com/en/software/tws/usersguidebook/ordertypes/attach_an_fx_order.htm}{FX Order}: 
#'   \item \href{https://www.interactivebrokers.com/en/software/tws/usersguidebook/ordertypes/attach_a_pair_trade.htm}{Pair Trade}:
#'   \item \href{https://www.interactivebrokers.com/en/software/tws/usersguidebook/ordertypes/autocombolimit.htm}{Combo Limit}:
#'   \item \href{https://www.interactivebrokers.com/en/software/tws/usersguidebook/ordertypes/autocombomarket.htm}{Combo Market}:
#' }
#'
# #' @example inst/examples/place_order_ex.R
#' @family orders
#' @export
#'
place_order <- function(
  contract,
  order,
  deltaNeutralContract = NULL,
  softDollarTier       = NULL,
  channel              = NULL,
  non_master_override  = TRUE
){
  
  sock    <- select_sock_for_api_fun()
  orderId <- if(any(names(order) == "orderId")){
    as.character(order["orderId"])
  } else {
    req_ids(channel = sock)
  }
  if(!isTRUE(channel == "master") && any(ls(sock_drawer) == "master")){
    if(package_state$place_order_on_non_master){
      package_state$place_order_on_non_master <- FALSE
      cat("\n")
      usethis::ui_info(
        paste0(
          "You have called ",
          crayon::bold("place_order"),
          "() on a non-master socket while a ",
          crayon::bold("master socket"),
          " is\ncurrently open. Because, if open, the master socket ",
          "automatically recieves a\nduplicate of all data sent by IB in ",
          "response to a call to ",
          crayon::bold("place_order"),
          "() on any\nother socket, it is advisable -- unless you have a ",
          "compelling reason to do\notherwise -- to use the ",
          crayon::bold("master socket"),
          " exclusively when calling ",
          crayon::bold("place_order"),
          "."
        )
      )
      if(non_master_override){
        usethis::ui_info(
          paste0(
            "The ",
            crayon::bold("master socket"),
            " will be used for placing this order.\nTo override this default ",
            "behavior and force ",
            crayon::bold("place_order"),
            "\nto use a different socket when called while a ",
            crayon::bold("master socket"),
            "\nis open, call ",
            crayon::bold("place_order"),
            " with the ",
            crayon::italic("non_master_override"),
            "\nargument set to ",
            crayon::italic("FALSE"),
            "."
          )
        )
      } else {
        usethis::ui_info(
          paste0(
            "The default behavior of using the ",
            crayon::bold("master socket"),
            " has been overridden, and the client specified by ",
            crayon::bold(channel),
            " will be used for placing this order."
          )
        )
      }
      usethis::ui_info("This message will display once per session.")
      cat("\n")
    }
    if(non_master_override){channel <- "master"}
  }
  
  if(is.null(names(contract))){
    if(length(contract) == 1){
      contract <- c(conId = unlist(contract, use.names = FALSE))
    } else if(length(contract) == 2){
      contract <- suppressWarnings(as.numeric(contract)) %>% {
        c(
          conId    = contract[!is.na(.)],
          exchange = contract[is.na(.)]
        )
      }
    } else {
      usethis::ui_oops(
        paste0(
          "The ",
          crayon::bold("contract"),
          " argument must have names. See ",
          crayon::bold("?contract"),
          " for details."
        )
      )
      return(invisible(FALSE))
    }
  }
  
  header_fields <- c(
    functionary$outgoing_msg_codes$PLACE_ORDER,
    orderId
  )
  
  contract_fields <- mget(
    setdiff(
      functionary$big_function_args$contract_args$
        place_order,
      names(contract)
    ),
    envir = functionary$contract_vars$Contract
  ) %>%
    c(contract) %$% {
      c(
        get("conId"),
        get("symbol"),
        if(get("comboLegs") == 0){
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
        get("secIdType"),
        get("secId")
      )
    }
  
  order_fields <- mget(
    setdiff(
      functionary$big_function_args$order_args$place_order,
      names(order)
    ),
    envir = functionary$order_vars
  ) %>%
    c(order) %$% {
      c(
        get("action"),
        get("totalQuantity"),
        get("orderType"),
        make_field_handle_empty(get("lmtPrice")),
        make_field_handle_empty(get("auxPrice")),
        get("tif"),
        get("ocaGroup"),
        get("account"),
        get("openClose"),
        get("origin"),
        get("orderRef"),
        as.numeric(as.logical(get("transmit"))),
        get("parentId"),
        get("blockOrder"),
        get("sweepToFill"),
        get("displaySize"),
        get("triggerMethod"),
        get("outsideRth"),
        get("hidden"),
        if(any(names(contract) == "comboLegs")){
          c(
            nrow(contract$comboLegs),
            if(nrow(contract$comboLegs) > 0){
              apply(
                contract$comboLegs,
                MARGIN = 1,
                function(comboLeg){
                  mget(
                    setdiff(
                      ls(functionary$contract_vars$ComboLeg),
                      names(comboLeg)
                    ),
                    envir = functionary$contract_vars$
                      ComboLeg
                  ) %>%
                    c(comboLeg) %$% {
                      list(
                        get("conId"),
                        get("ratio"),
                        get("action"),
                        get("exchange"),
                        get("openClose"),
                        get("shortSaleSlot"),
                        get("designatedLocation"),
                        get("exemptCode")
                      )
                    }
                }
              ) %>%
                unlist(use.names = FALSE)
            },
            if(identical(get("orderComboLegs"), "")){
              0
            } else {
              c(
                length(get("orderComboLegs")),
                lapply(
                  get("orderComboLegs"),
                  function(orderComboLeg){
                    make_field_handle_empty(as.numeric(get("orderComboLeg")))
                  }
                )
              ) %>%
                unlist(use.names = FALSE)
            },
            if(identical(get("smartComboRoutingParams"), "")){
              0
            } else {
              c(
                length(get("smartComboRoutingParams")),
                {
                  y <- rep("", length(get("smartComboRoutingParams")) * 2)
                  y[seq(1, length(y), by = 2)] <- names(
                    get("smartComboRoutingParams")
                  )
                  y[seq(2, length(y), by = 2)] <- unlist(
                    get("smartComboRoutingParams"),
                    use.names = FALSE
                  )
                  y
                }
              )
            }
          )
        },
        "",
        get("discretionaryAmt"),
        get("goodAfterTime"),
        get("goodTillDate"),
        get("faGroup"),
        get("faMethod"),
        get("faPercentage"),
        get("faProfile"),
        get("modelCode"),
        # institutional short sale slot fields
        get("shortSaleSlot"),
        get("designatedLocation"),
        get("exemptCode"),
        # Fields for server v19 and up
        get("ocaType"),
        get("rule80A"),
        get("settlingFirm"),
        get("allOrNone"),
        make_field_handle_empty(get("minQty")),
        make_field_handle_empty(get("percentOffset")),
        get("eTradeOnly"),
        get("firmQuoteOnly"),
        make_field_handle_empty(get("nbboPriceCap")),
        get("auctionStrategy"),
        make_field_handle_empty(get("startingPrice")),
        make_field_handle_empty(get("stockRefPrice")),
        make_field_handle_empty(get("delta")),
        make_field_handle_empty(get("stockRangeLower")),
        make_field_handle_empty(get("stockRangeUpper")),
        get("overridePercentageConstraints"),
        make_field_handle_empty(get("volatility")),
        make_field_handle_empty(get("volatilityType")),
        get("deltaNeutralOrderType"),
        make_field_handle_empty(get("deltaNeutralAuxPrice")),
        if(!identical(get("deltaNeutralOrderType"), "")){
          c(
            get("deltaNeutralConId"),
            get("deltaNeutralSettlingFirm"),
            get("deltaNeutralClearingAccount"),
            get("deltaNeutralClearingIntent"),
            get("deltaNeutralOpenClose"),
            get("deltaNeutralShortSale"),
            get("deltaNeutralShortSaleSlot"),
            get("deltaNeutralDesignatedLocation")
          )
        },
        get("continuousUpdate"),
        make_field_handle_empty(get("referencePriceType")),
        make_field_handle_empty(get("trailStopPrice")),
        make_field_handle_empty(get("trailingPercent")),
        make_field_handle_empty(get("scaleInitFillQty")),
        make_field_handle_empty(get("scaleSubsLevelSize")),
        make_field_handle_empty(get("scalePriceIncrement")),
        if(
          isTRUE(get("scalePriceIncrement") > 0) && (
            get("scalePriceIncrement") != package_state$UNSET_DOUBLE
          )
        ){
          c(
            make_field_handle_empty(get("scalePriceAdjustValue")),
            make_field_handle_empty(get("scalePriceAdjustInterval")),
            make_field_handle_empty(get("scaleProfitOffset")),
            get("scaleAutoReset"),
            make_field_handle_empty(get("scaleInitPosition")),
            make_field_handle_empty(get("scaleInitFillQty")),
            get("scaleRandomPercent")
          )
        },
        get("scaleTable"),
        get("activeStartTime"),
        get("activeStopTime"),
        get("hedgeType"),
        if(isTRUE(as.logical(get("hedgeType")))){get("hedgeParam")},
        get("optOutSmartRouting"),
        get("clearingAccount"),
        get("clearingIntent"),
        get("notHeld"),
        if(is.null(deltaNeutralContract)){
          0
        } else {
          c(
            1,
            deltaNeutralContract["conId"],
            deltaNeutralContract["delta"],
            deltaNeutralContract["price"]
          )
        },
        make_field_handle_empty(get("algoParams")),
        if(!identical(get("algoParams"), "")){
          y <- rep("", length(get("algoParams")) * 2)
          y[seq(1, length(y), by = 2)] <- names(get("algoParams"))
          y[seq(2, length(y), by = 2)] <- unlist(
            get("algoParams"), 
            use.names = FALSE
          )
          y
        },
        get("algoId"),
        get("whatIf"),
        make_field_handle_empty(get("orderMiscOptions")),
        if(!identical(get("orderMiscOptions"), "")){
          y <- rep("", length(get("orderMiscOptions")) * 2)
          y[seq(1, length(y), by = 2)] <- names(get("orderMiscOptions"))
          y[seq(2, length(y), by = 2)] <- unlist(
            get("orderMiscOptions"),
            use.names = FALSE
          )
          y
        },
        get("solicited"),
        get("randomizeSize"),
        get("randomizePrice"),
        if(get("orderType") == "PEG BENCH"){
          c(
            get("referenceContractId"),
            get("isPeggedChangeAmountDecrease"),
            get("peggedChangeAmount"),
            get("referenceChangeAmount"),
            get("referenceExchangeId")
          )
        },
        if(identical(get("conditions"), "")){
          0
        } else {
          c(
            length(get("conditions")),
            if(length(get("conditions")) > 0){
              y <- rep("", length(get("orderMiscOptions")) * 2)
              y[seq(1, length(y), by = 2)] <- names(get("orderMiscOptions"))
              y[seq(2, length(y), by = 2)] <- unlist(
                get("orderMiscOptions"),
                use.names = FALSE
              )
              
              c(
                y,
                get("conditionsIgnoreRth"),
                get("conditionsCancelOrder")
              )
              
            }
          )
        },
        get("adjustedOrderType"),
        sprintf("%f", get("triggerPrice")),
        sprintf("%f", get("lmtPriceOffset")),
        sprintf("%f", get("adjustedStopPrice")),
        sprintf("%f", get("adjustedStopLimitPrice")),
        sprintf("%f", get("adjustedTrailingAmount")),
        get("adjustableTrailingUnit"),
        get("extOperator"),
        make_field_handle_empty(names(get("softDollarTier"))),
        make_field_handle_empty(get("softDollarTier")),
        sprintf("%f", get("cashQty")),
        get("mifid2DecisionMaker"),
        get("mifid2DecisionAlgo"),
        get("mifid2ExecutionTrader"),
        get("mifid2ExecutionAlgo"),
        get("dontUseAutoPriceForHedge"),
        get("isOmsContainer"),
        get("discretionaryUpToLimitPrice"),
        get("usePriceMgmtAlgo")
      )
    } %>%
    trimws()
  
  order_fields <<- order_fields
  
  place_order_msg <- c(header_fields, contract_fields, order_fields)
  place_order_msg <- ib_encode_raw_msg(place_order_msg)
  
  writeBin(object = place_order_msg, con = sock, endian = "big")
  
  place_order_response <- sock_seek(
    element_names   = "ORDER_STATUSES",
    socket          = sock,
    success_element = simple_encode(
      c(
        functionary$incoming_msg_codes$ORDER_STATUS,
        orderId
      )
    )
  )
  
  # Need something else to read executions and commission if trade executes?
  # No, becaause executions are something else. They dont belong in place_order
  
  # } else {
  #   simple_encode(
  #     functionary$incoming_msg_codes$COMMISSION_REPORT
  #   )
  # }
  # ,
  # stop_early      = if(orderType != "LMT"){
  #   simple_encode(
  #     c(
  #       functionary$incoming_msg_codes$ERR_MSG,
  #       "2",
  #       orderId
  #     )
  #   )
  # }
  
  assign(
    "ORDER_STATUSES",
    structure(
      dplyr::bind_rows(
        functionary$ib_update$ORDER_STATUSES(
          place_order_response
        ),
        get("treasury")$ORDER_STATUSES
      ) %>%
        unique(),
      last_updated = Sys.time()
    ),
    envir = get("treasury")
  )
  
  invisible(
    all(place_order_response$permId %in% get("treasury")$ORDER_STATUSES$permId)
  )
  
}
