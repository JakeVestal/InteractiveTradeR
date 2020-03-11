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
#' @example inst/examples/place_order_ex.R
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
  
  # sock    <- select_sock_for_api_fun()
  # orderId <- if(any(names(order) == "orderId")){
  #   as.character(order["orderId"])
  # } else {
  #   req_ids(channel = sock)
  # }
  # if(!isTRUE(channel == "master") && any(ls(sock_drawer) == "master")){
  #   if(package_state$place_order_on_non_master){
  #     package_state$place_order_on_non_master <- FALSE
  #     cat("\n")
  #     usethis::ui_info(
  #       paste0(
  #         "You have called ",
  #         crayon::bold("place_order"),
  #         "() on a non-master socket while a ",
  #         crayon::bold("master socket"),
  #         " is\ncurrently open. Because, if open, the master socket ",
  #         "automatically recieves a\nduplicate of all data sent by IB in ",
  #         "response to a call to ",
  #         crayon::bold("place_order"),
  #         "() on any\nother socket, it is advisable -- unless you have a ",
  #         "compelling reason to do\notherwise -- to use the ",
  #         crayon::bold("master socket"),
  #         " exclusively when calling ",
  #         crayon::bold("place_order"),
  #         "."
  #       )
  #     )
  #     if(non_master_override){
  #       usethis::ui_info(
  #         paste0(
  #           "The ",
  #           crayon::bold("master socket"),
  #           " will be used for placing this order.\nTo override this default ",
  #           "behavior and force ",
  #           crayon::bold("place_order"),
  #           "\nto use a different socket when called while a ",
  #           crayon::bold("master socket"),
  #           "\nis open, call ",
  #           crayon::bold("place_order"),
  #           " with the ",
  #           crayon::italic("non_master_override"),
  #           "\nargument set to ",
  #           crayon::italic("FALSE"),
  #           "."
  #         )
  #       )
  #     } else {
  #       usethis::ui_info(
  #         paste0(
  #           "The default behavior of using the ",
  #           crayon::bold("master socket"),
  #           " has been overridden, and the client specified by ",
  #           crayon::bold(channel),
  #           " will be used for placing this order."
  #         )
  #       )
  #     }
  #     usethis::ui_info("This message will display once per session.")
  #     cat("\n")
  #   }
  #   if(non_master_override){channel <- "master"}
  # }
  # 
  # if(is.null(names(contract))){
  #   if(length(contract) == 1){
  #     contract <- c(conId = unlist(contract, use.names = FALSE))
  #   } else if(length(contract) == 2){
  #     contract <- suppressWarnings(as.numeric(contract)) %>% {
  #       c(
  #         conId    = contract[!is.na(.)],
  #         exchange = contract[is.na(.)]
  #       )
  #     }
  #   } else {
  #     usethis::ui_oops(
  #       paste0(
  #         "The ",
  #         crayon::bold("contract"),
  #         " argument must have names. See ",
  #         crayon::bold("?contract"),
  #         " for details."
  #       )
  #     )
  #     return(invisible(FALSE))
  #   }
  # }
  # 
  # header_fields <- c(
  #   functionary$outgoing_msg_codes$PLACE_ORDER,
  #   orderId
  # )
  # 
  # contract_fields <- mget(
  #   setdiff(
  #     functionary$big_function_args$contract_args$
  #       place_order,
  #     names(contract)
  #   ),
  #   envir = functionary$contract_vars$Contract
  # ) %>%
  #   c(contract) %$% {
  #     c(
  #       conId,
  #       symbol,
  #       if(comboLegs == 0){
  #         secType
  #       } else {
  #         "BAG"
  #       },
  #       lastTradeDateOrContractMonth,
  #       strike,
  #       right,
  #       multiplier,
  #       exchange,
  #       primaryExchange,
  #       currency,
  #       localSymbol,
  #       tradingClass,
  #       secIdType,
  #       secId
  #     )
  #   }
  # 
  # order_fields <- mget(
  #   setdiff(
  #     functionary$big_function_args$order_args$place_order,
  #     names(order)
  #   ),
  #   envir = functionary$order_vars
  # ) %>%
  #   c(order) %$% {
  #     c(
  #       action,
  #       totalQuantity,
  #       orderType,
  #       make_field_handle_empty(lmtPrice),
  #       make_field_handle_empty(auxPrice),
  #       tif,
  #       ocaGroup,
  #       account,
  #       openClose,
  #       origin,
  #       orderRef,
  #       as.numeric(as.logical(transmit)),
  #       parentId,
  #       blockOrder,
  #       sweepToFill,
  #       displaySize,
  #       triggerMethod,
  #       outsideRth,
  #       hidden,
  #       if(any(names(contract) == "comboLegs")){
  #         c(
  #           nrow(contract$comboLegs),
  #           if(nrow(contract$comboLegs) > 0){
  #             apply(
  #               contract$comboLegs,
  #               MARGIN = 1,
  #               function(comboLeg){
  #                 mget(
  #                   setdiff(
  #                     ls(functionary$contract_vars$ComboLeg),
  #                     names(comboLeg)
  #                   ),
  #                   envir = functionary$contract_vars$
  #                     ComboLeg
  #                 ) %>%
  #                   c(comboLeg) %$% {
  #                     list(
  #                       conId,
  #                       ratio,
  #                       action,
  #                       exchange,
  #                       openClose,
  #                       shortSaleSlot,
  #                       designatedLocation,
  #                       exemptCode
  #                     )
  #                   }
  #               }
  #             ) %>%
  #               unlist(use.names = FALSE)
  #           },
  #           if(identical(orderComboLegs, "")){
  #             0
  #           } else {
  #             c(
  #               length(orderComboLegs),
  #               lapply(
  #                 orderComboLegs,
  #                 function(orderComboLeg){
  #                   make_field_handle_empty(as.numeric(orderComboLeg))
  #                 }
  #               )
  #             ) %>%
  #               unlist(use.names = FALSE)
  #           },
  #           if(identical(smartComboRoutingParams, "")){
  #             0
  #           } else {
  #             c(
  #               length(smartComboRoutingParams),
  #               {
  #                 y <- rep("", length(smartComboRoutingParams) * 2)
  #                 y[seq(1, length(y), by = 2)] <- names(
  #                   smartComboRoutingParams
  #                 )
  #                 y[seq(2, length(y), by = 2)] <- unlist(
  #                   smartComboRoutingParams,
  #                   use.names = FALSE
  #                 )
  #                 y
  #               }
  #             )
  #           }
  #         )
  #       },
  #       "",
  #       discretionaryAmt,
  #       goodAfterTime,
  #       goodTillDate,
  #       faGroup,
  #       faMethod,
  #       faPercentage,
  #       faProfile,
  #       modelCode,
  #       # institutional short sale slot fields
  #       shortSaleSlot,
  #       designatedLocation,
  #       exemptCode,
  #       # Fields for server v19 and up
  #       ocaType,
  #       rule80A,
  #       settlingFirm,
  #       allOrNone,
  #       make_field_handle_empty(minQty),
  #       make_field_handle_empty(percentOffset),
  #       eTradeOnly,
  #       firmQuoteOnly,
  #       make_field_handle_empty(nbboPriceCap),
  #       auctionStrategy,
  #       make_field_handle_empty(startingPrice),
  #       make_field_handle_empty(stockRefPrice),
  #       make_field_handle_empty(delta),
  #       make_field_handle_empty(stockRangeLower),
  #       make_field_handle_empty(stockRangeUpper),
  #       overridePercentageConstraints,
  #       make_field_handle_empty(volatility),
  #       make_field_handle_empty(volatilityType),
  #       deltaNeutralOrderType,
  #       make_field_handle_empty(deltaNeutralAuxPrice),
  #       if(!identical(deltaNeutralOrderType, "")){
  #         c(
  #           deltaNeutralConId,
  #           deltaNeutralSettlingFirm,
  #           deltaNeutralClearingAccount,
  #           deltaNeutralClearingIntent,
  #           deltaNeutralOpenClose,
  #           deltaNeutralShortSale,
  #           deltaNeutralShortSaleSlot,
  #           deltaNeutralDesignatedLocation
  #         )
  #       },
  #       continuousUpdate,
  #       make_field_handle_empty(referencePriceType),
  #       make_field_handle_empty(trailStopPrice),
  #       make_field_handle_empty(trailingPercent),
  #       make_field_handle_empty(scaleInitFillQty),
  #       make_field_handle_empty(scaleSubsLevelSize),
  #       make_field_handle_empty(scalePriceIncrement),
  #       if(
  #         isTRUE(scalePriceIncrement > 0) && (
  #           scalePriceIncrement != package_state$UNSET_DOUBLE
  #         )
  #       ){
  #         c(
  #           make_field_handle_empty(scalePriceAdjustValue),
  #           make_field_handle_empty(scalePriceAdjustInterval),
  #           make_field_handle_empty(scaleProfitOffset),
  #           scaleAutoReset,
  #           make_field_handle_empty(scaleInitPosition),
  #           make_field_handle_empty(scaleInitFillQty),
  #           scaleRandomPercent
  #         )
  #       },
  #       scaleTable,
  #       activeStartTime,
  #       activeStopTime,
  #       hedgeType,
  #       if(isTRUE(as.logical(hedgeType))){hedgeParam},
  #       optOutSmartRouting,
  #       clearingAccount,
  #       clearingIntent,
  #       notHeld,
  #       if(is.null(deltaNeutralContract)){
  #         0
  #       } else {
  #         c(
  #           1,
  #           deltaNeutralContract["conId"],
  #           deltaNeutralContract["delta"],
  #           deltaNeutralContract["price"]
  #         )
  #       },
  #       make_field_handle_empty(algoParams),
  #       if(!identical(algoParams, "")){
  #         y <- rep("", length(algoParams) * 2)
  #         y[seq(1, length(y), by = 2)] <- names(algoParams)
  #         y[seq(2, length(y), by = 2)] <- unlist(algoParams, use.names = FALSE)
  #         y
  #       },
  #       algoId,
  #       whatIf,
  #       make_field_handle_empty(orderMiscOptions),
  #       if(!identical(orderMiscOptions, "")){
  #         y <- rep("", length(orderMiscOptions) * 2)
  #         y[seq(1, length(y), by = 2)] <- names(orderMiscOptions)
  #         y[seq(2, length(y), by = 2)] <- unlist(
  #           orderMiscOptions,
  #           use.names = FALSE
  #         )
  #         y
  #       },
  #       solicited,
  #       randomizeSize,
  #       randomizePrice,
  #       if(orderType == "PEG BENCH"){
  #         c(
  #           referenceContractId,
  #           isPeggedChangeAmountDecrease,
  #           peggedChangeAmount,
  #           referenceChangeAmount,
  #           referenceExchangeId
  #         )
  #       },
  #       if(identical(conditions, "")){
  #         0
  #       } else {
  #         c(
  #           length(conditions),
  #           if(length(conditions) > 0){
  #             y <- rep("", length(orderMiscOptions) * 2)
  #             y[seq(1, length(y), by = 2)] <- names(orderMiscOptions)
  #             y[seq(2, length(y), by = 2)] <- unlist(
  #               orderMiscOptions,
  #               use.names = FALSE
  #             )
  # 
  #             c(
  #               y,
  #               conditionsIgnoreRth,
  #               conditionsCancelOrder
  #             )
  # 
  #           }
  #         )
  #       },
  #       adjustedOrderType,
  #       sprintf("%f", triggerPrice),
  #       sprintf("%f", lmtPriceOffset),
  #       sprintf("%f", adjustedStopPrice),
  #       sprintf("%f", adjustedStopLimitPrice),
  #       sprintf("%f", adjustedTrailingAmount),
  #       adjustableTrailingUnit,
  #       extOperator,
  #       make_field_handle_empty(names(softDollarTier)),
  #       make_field_handle_empty(softDollarTier),
  #       sprintf("%f", cashQty),
  #       mifid2DecisionMaker,
  #       mifid2DecisionAlgo,
  #       mifid2ExecutionTrader,
  #       mifid2ExecutionAlgo,
  #       dontUseAutoPriceForHedge,
  #       isOmsContainer,
  #       discretionaryUpToLimitPrice,
  #       usePriceMgmtAlgo
  #     )
  #   } %>%
  #   trimws()
  # 
  # order_fields <<- order_fields
  # 
  # place_order_msg <- c(header_fields, contract_fields, order_fields)
  # place_order_msg <- ib_encode_raw_msg(place_order_msg)
  # 
  # writeBin(object = place_order_msg, con = sock, endian = "big")
  # 
  # place_order_response <- sock_seek(
  #   element_names   = "ORDER_STATUSES",
  #   socket          = sock,
  #   success_element = simple_encode(
  #     c(
  #       functionary$incoming_msg_codes$ORDER_STATUS,
  #       orderId
  #     )
  #   )
  # )
  # 
  # # Need something else to read executions and commission if trade executes?
  # # No, becaause executions are something else. They dont belong in place_order
  # 
  # # } else {
  # #   simple_encode(
  # #     functionary$incoming_msg_codes$COMMISSION_REPORT
  # #   )
  # # }
  # # ,
  # # stop_early      = if(orderType != "LMT"){
  # #   simple_encode(
  # #     c(
  # #       functionary$incoming_msg_codes$ERR_MSG,
  # #       "2",
  # #       orderId
  # #     )
  # #   )
  # # }
  # 
  # assign(
  #   "ORDER_STATUSES",
  #   structure(
  #     dplyr::bind_rows(
  #       functionary$ib_update$ORDER_STATUSES(
  #         place_order_response
  #       ),
  #       treasury$ORDER_STATUSES
  #     ) %>%
  #       unique(),
  #     last_updated = Sys.time()
  #   ),
  #   envir = treasury
  # )
  # 
  # invisible(
  #   all(place_order_response$permId %in% treasury$ORDER_STATUSES$permId)
  # )
  
  "yolo"
  
}
