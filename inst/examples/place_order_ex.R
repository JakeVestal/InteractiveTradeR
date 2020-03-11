# 1a) Basic Limit Order for a stock
place_order(
  contract = c(
    symbol   = "IBKR",
    secType  = "STK",
    currency = "USD",
    exchange = "SMART"
  ),
  order    = c(
    action        = "BUY",
    orderType     = "LMT",
    totalQuantity = 1,
    lmtPrice      = 45,
    account       = req_account_summary(tags = "AvailableFunds") %>%
      dplyr::filter(as.numeric(tag_value) > 5000) %>%
      dplyr::select(account) %>%
      unlist(use.names = FALSE) %>%
      sample(1)
  )
)

# 1b) Basic Limit Order for a stock's lowest-strike in-the-money call option
place_order(
  contract = c(
    symbol   = "IBKR",
    secType  = "STK",
    currency = "USD",
    exchange = "SMART"
  ),
  order    = c(
    action        = "BUY",
    orderType     = "LMT",
    totalQuantity = 1,
    lmtPrice      = 45,
    account       = req_account_summary(tags = "AvailableFunds") %>%
      dplyr::filter(as.numeric(tag_value) > 5000) %>%
      dplyr::select(account) %>%
      unlist(use.names = FALSE) %>%
      sample(1)
  )
)

# 2) Market Combo Order
place_order(
  contract = list(
    symbol    = "IBKR,MCD",
    secType   = "BAG",
    currency  = "USD",
    exchange  = "SMART",
    comboLegs = tibble::tibble(
      conId    = c(43645865, 9408),
      ratio    = c(1,1),
      action   = c("BUY","SELL"),
      exchange = c("SMART", "SMART")
    )
  ),
  order = list(
    action        = "BUY",
    orderType     = "MKT",
    totalQuantity = 1,
    account       = req_account_summary(tags = "AvailableFunds") %>%
      dplyr::filter(as.numeric(tag_value) > 5000) %>%
      dplyr::select(account) %>%
      unlist(use.names = FALSE) %>%
      sample(1),
    smartComboRoutingParams = c(NonGuaranteed = 1)
  )
)

### 3) Limit Combo Order, Combo Price Specified
place_order(
  contract = list(
    symbol    = "IBKR,MCD",
    secType   = "BAG",
    currency  = "USD",
    exchange  = "SMART",
    comboLegs = tibble::tibble(
      conId    = c(43645865, 9408),
      ratio    = c(1,1),
      action   = c("BUY","SELL"),
      exchange = c("SMART", "SMART")
    )
  ),
  order = list(
    action        = "BUY",
    orderType     = "LMT",
    lmtPrice      = 150,
    totalQuantity = 1,
    account       = req_account_summary(tags = "AvailableFunds") %>%
      dplyr::filter(as.numeric(tag_value) > 5000) %>%
      dplyr::select(account) %>%
      unlist(use.names = FALSE) %>%
      sample(1),
    smartComboRoutingParams = c(NonGuaranteed = 1)
  )
)

### 4) Limit Combo Order, Individual Leg Prices Specified
place_order(
  contract = list(
    symbol    = "IBKR,MCD",
    secType   = "BAG",
    currency  = "USD",
    exchange  = "SMART",
    comboLegs = tibble::tibble(
      conId    = c(43645865, 9408),
      ratio    = c(1,1),
      action   = c("BUY","SELL"),
      exchange = c("SMART", "SMART")
    )
  ),
  order = list(
    action         = "BUY",
    orderType      = "LMT",
    orderComboLegs = c(45,210),
    totalQuantity  = 1,
    account        = req_account_summary(tags = "AvailableFunds") %>%
      dplyr::filter(as.numeric(tag_value) > 5000) %>%
      dplyr::select(account) %>%
      unlist(use.names = FALSE) %>%
      sample(1),
    smartComboRoutingParams = c(NonGuaranteed = 1)
  )
)

### 5) Delta Neutral Order
place_order(
  contract = list(
    symbol     = "GOOG",
    secType    = "OPT",
    currency   = "USD",
    exchange   = "SMART",
    right      = "C",
    multiplier = 100,
    strike     = 1170
  ),
  order = list(
    action         = "BUY",
    orderType      = "MKT",
    totalQuantity  = 1,
    account        = req_account_summary(tags = "AvailableFunds") %>%
      dplyr::filter(as.numeric(tag_value) > 5000) %>%
      dplyr::select(account) %>%
      unlist(use.names = FALSE) %>%
      sample(1)
  )
)
