decode_contract_fields <- function(x){
  y <- tibble::tibble(
    conId   = as.numeric(x[1]),
    symbol  = x[2],
    secType = x[3],
    lastTradeDateOrContractMonth = x[4],
    strike        = as.numeric(x[5]),
    right         = x[6],
    multiplier    = as.numeric(x[7]),
    exchange      = x[8],
    currency      = x[9],
    localSymbol   = x[10],
    tradingClass  = x[11]
  )
  assign("x", x[-(1:ncol(y))], parent.frame())
  y
}

decode_fa_params <- function(x){
  y <- tibble::tibble(
    faGroup      = x[1],
    faMethod     = x[2],
    faPercentage = x[3],
    faProfile    = x[4]
  )
  assign("x", x[-(1:ncol(y))], parent.frame())
  y
}

decode_short_sale_params <- function(x){
  y <- tibble::tibble(
    shortSaleSlot      = as.numeric(x[1]),
    designatedLocation = x[2],
    exemptCode         = as.numeric(x[3])
  )
  assign("x", x[-(1:ncol(y))], parent.frame())
  y
}

decode_box_order_params <- function(x){
  y <- tibble::tibble(
    startingPrice = as.numeric(x[1]),
    stockRefPrice = as.numeric(x[2]),
    delta         = as.numeric(x[3])
  )
  assign("x", x[-(1:ncol(y))], parent.frame())
  y
}

decode_peg_to_stk_or_vol_order_params <- function(x){
  y <- tibble::tibble(
    stockRangeLower = as.numeric(x[1]),
    stockRangeUpper = as.numeric(x[2])
  )
  assign("x", x[-(1:ncol(y))], parent.frame())
  y
}

decode_vol_order_params <- function(x, readOpenOrderAttribs){
  y <- dplyr::bind_cols(
    tibble::tibble(
      volatility            = as.numeric(x[1]),
      volatilityType        = as.numeric(x[2]),
      deltaNeutralOrderType = x[3],
      deltaNeutralAuxPrice  = as.numeric(x[4]),
      deltaNeutralConId     = as.numeric(x[5])
    ),
    if(readOpenOrderAttribs){
      tibble::tibble(
        deltaNeutralSettlingFirm       = x[6],
        deltaNeutralClearingAccount    = x[7],
        deltaNeutralClearingIntent     = x[8],
        deltaNeutralOpenClose          = x[9],
        deltaNeutralShortSale          = as.logical(as.numeric(x[10])),
        deltaNeutralShortSaleSlot      = as.numeric(x[11]),
        deltaNeutralDesignatedLocation = x[12],
        continuousUpdate               = as.logical(as.numeric(x[13])),
        referencePriceType             = as.numeric(x[14])
      )
    } else {
      tibble::tibble(
        deltaNeutralShortSale          = as.logical(as.numeric(x[6])),
        deltaNeutralShortSaleSlot      = as.numeric(x[7]),
        deltaNeutralDesignatedLocation = x[8],
        continuousUpdate               = as.logical(as.numeric(x[9])),
        referencePriceType             = as.numeric(x[10])
      )
    }
  )
  assign("x", x[-(1:ncol(y))], parent.frame())
  y
}

decode_trail_params <- function(x){
  y <- tibble::tibble(
    trailStopPrice  = as.numeric(x[1]),
    trailingPercent = as.numeric(x[2])
  )
  assign("x", x[-(1:ncol(y))], parent.frame())
  y
}

decode_basis_points <- function(x){
  y <- tibble::tibble(
    basisPoints     = as.numeric(x[1]),
    pasisPointsType = as.numeric(x[2])
  )
  assign("x", x[-(1:ncol(y))], parent.frame())
  y
}

decode_combo_legs <- function(x){

  x_offset          <- 0
  comboLeg_colnames <- c(
    "conId",     "ratio",         "action",             "exchange",
    "openClose", "shortSaleSlot", "designatedLocation", "exemptCode"
  )

  y <- suppressWarnings(
    dplyr::bind_cols(
      tibble::tibble(
        comboLegsDescrip = x[1],
        comboLegsCount   = as.numeric(x[2])
      ),
      {
        if(isTRUE(as.numeric(x[2]) > 0)){
          yy <- tibble::tibble(
            comboLegs      = list(
              tibble::as_tibble(
                matrix(
                  x[3:(2 + length(comboLeg_colnames) * as.numeric(x[2]))],
                  nrow     = as.numeric(x[2]),
                  ncol     = length(comboLeg_colnames),
                  dimnames = list(list(), comboLeg_colnames),
                  byrow    = TRUE
                )
              )
            )
          )
          x_offset <- x_offset + (
            nrow(yy$comboLegs[[1]]) * ncol(yy$comboLegs[[1]])
          )
          yy
        }
      },
      tibble::tibble(
        orderComboLegsCount = as.numeric(x[(3 + x_offset)])
      ),
      {
        if(isTRUE(as.numeric(x[3 + x_offset]) > 0)){
          tibble::tibble(
            orderComboLeg_prices = list(
              as.numeric(
                x[(7 + x_offset):(4 + as.numeric(x[3 + x_offset]))]
              )
            )
          )
        }
      }
    )
  )

  assign(
    "x",
    x[-(1:(ncol(y) + x_offset + as.numeric(x[3 + x_offset]) - 1))],
    parent.frame()
  )
  y

}

decode_smart_combo_routing_params <- function(x){
  y <- dplyr::bind_cols(
    tibble::tibble(
      smartComboRoutingParamsCount = as.numeric(x[1])
    ),
    if(isTRUE(as.numeric(x[1]) > 0)){
      tibble::tibble(
        smartComboRoutingParams = stats::setNames(
          as.list(x[seq(from = 3, to = 1 + as.numeric(x[1]) * 2, by = 2)]),
          x[seq(from = 2, to = 1 + as.numeric(x[1]) * 2, by = 2)]
        )
      )
    }
  )
  assign("x", x[-(1:ncol(y))], parent.frame())
  y
}

decode_scale_order_params <- function(x){
  y <- dplyr::bind_cols(
    tibble::tibble(
      scaleInitLevelSize  = as.numeric(x[1]),
      scaleSubsLevelSize  = as.numeric(x[2]),
      scalePriceIncrement = as.numeric(x[3])
    ),
    if(
      isTRUE(as.numeric(x[3]) > 0) &&
      isTRUE(as.numeric(x[3]) != package_state$UNSET_DOUBLE)
    ){
      tibble::tibble(
        scalePriceAdjustValue    = as.numeric(x[4]),
        scalePriceAdjustInterval = as.numeric(x[5]),
        scaleProfitOffset        = as.numeric(x[6]),
        scaleAutoReset           = as.logical(as.numeric(x[7])),
        scaleInitPosition        = as.numeric(x[8]),
        scaleInitFillQty         = as.numeric(x[9]),
        scaleRandomPercent       = as.logical(as.numeric(x[10]))
      )
    }
  )
  assign("x", x[-(1:ncol(y))], parent.frame())
  y
}

decode_delta_neutral <- function(x){
  y <- dplyr::bind_cols(
    deltaNeutralContractPresent = as.logical(as.numeric(x[1])),
    if(as.logical(as.numeric(x[1]))){
      tibble::tibble(
        deltaNeutralContract_conId = as.numeric(x[2]),
        deltaNeutralContract_delta = as.numeric(x[3]),
        deltaNeutralContract_price = as.numeric(x[4])
      )
    }
  )
  assign("x", x[-(1:ncol(y))], parent.frame())
  y
}

decode_algo_params <- function(x){
  y <- dplyr::bind_cols(
    tibble::tibble(
      algoStrategy    = x[1],
      algoParamsCount = as.numeric(x[2])
    ),
    if(isTRUE(make_numeric_handle_empty(as.numeric(x[2]) > 0))){
      tibble::tibble(
        algoParams = list(
          tibble::as_tibble(
            matrix(
              x[3:(2 + 2 * as.numeric(x[2]))],
              nrow     = as.numeric(x[2]),
              ncol     = 2,
              dimnames = list(list(), c("tag", "value")),
              byrow    = TRUE
            )
          )
        )
      )
    }
  )
  if(is.na(make_numeric_handle_empty(as.numeric(x[2])))){
    assign("x", x[-(1:2)], parent.frame())
  } else {
    assign("x", x[-(1:(1 + 2 * as.numeric(x[2])))], parent.frame())
  }
  y
}

decode_conditions <- function(x){
  y <- dplyr::bind_cols(
    tibble::tibble(
      conditionsSize = as.numeric(x[1])
    ),
    {
      if(as.numeric(x[1]) > 0){
        stop("decode conditions.")
      }
    }
  )
  assign("x", x[-(1:ncol(y))], parent.frame())
  y
}

