# Fetch a delayed market data snapshot for Exxon (XOM)
req_mkt_data(
  contract = c(
    symbol = "XOM", secType = "STK", currency = "USD", exchange = "SMART"
  ),
  data_name   = "Exxon_delayed",
  mktDataType = "DELAYED"
)

# Set up a market data subscription to Exxon
req_mkt_data(
  contract = c(
    symbol   = "XOM",
    secType  = "STK",
    currency = "USD",
    exchange = "SMART"
  ),
  mktDataType = "DELAYED",
  snapshot    = FALSE,
  channel     = "async"
)
# Wait a second or two for data
Sys.sleep(2)

read_sock_drawer()

# combo
req_mkt_data(
  contract  = list(
    symbol    = "GOOG",
    secType   = "BAG",
    currency  = "USD",
    exchange  = "SMART",
    comboLegs = tibble::tibble(
      conId    = c(208813720, 107113386),
      ratio    = c(1,3),
      action   = c("BUY","SELL"),
      exchange = c("SMART", "SMART")
    )
  )
)

create_new_connections(1)

# 1: Contract by conId (AAPL US STK)
req_mkt_data(265598, mktDataType = 3, channel = "async")

read_sock_drawer()

cancel_mkt_data(265598)



last_incoming$simple <- mkt_data_simple[[length(mkt_data_simple)]]




#2: Stock Combo
ibkr_mcd_contract <- c(
  symbol   = "IBKR,MCD",
  secType  = "BAG",
  currency = "USD"
)
ibkr_mcd_combo <- tibble::tibble(
  conId = c(43645865, 9408),
  ratio = c(1,1),
  action = c("BUY","SELL"),
  exchange = c("SMART", "SMART")
)
req_mkt_data(contract = ibkr_mcd_contract, combo = ibkr_mcd_combo)
ibkr_mcd_data <- ib_parsed_raw_list
last_incoming$ibkr_mcd_data <- ibkr_mcd_data[[length(ibkr_mcd_data)]]

cbind(fun_calls_py$reqMktData$itr_decoded_api_msg[[7]], req_mkt_data_msg)

#3: Future Combo Contract
future_combo_contract <- c(
  symbol = "VIX", secType = "BAG", currency = "USD", exchange = "CFE"
)
future_combo <- tibble::tibble(
  # Use req_contract_details() to pick three random VIX futures:
  conId = req_contract_details(
    contract = c(
      symbol = "VIX", exchange = "CFE", currency = "USD", secType = "FUT"
    )
  )$conId %>%
    sample(3),
  ratio    = c(1,1,2),
  action   = c("BUY","SELL", "BUY"),
  exchange = c("CFE", "CFE", "CFE")
)
req_mkt_data(contract = future_combo_contract, combo = future_combo)
future_combo_data <- ib_parsed_raw_list
last_incoming$future_combo_data <- future_combo_data[[length(future_combo_data)]]

#4: Smart Future Combo Contract
smart_future_combo_contract <- c(
  symbol = "WTI", secType = "BAG", currency = "USD", exchange = "SMART"
)
smart_future_combo <- tibble::tibble(
  # Use req_contract_details() with vapply() to pick a random WTI and and a
  # random COIL future conId.
  conId = vapply(
    c("WTI", "COIL"),
    function(symb){
      req_contract_details(
        contract = c(
          symbol = symb, exchange = "IPE", currency = "USD", secType = "FUT"
        )
      )$conId %>%
        sample(1)
    },
    character(1)
  ),
  ratio    = c(1,1),
  action   = c("BUY","SELL"),
  exchange = c("CFE", "CFE")
)
req_mkt_data(contract = future_combo_contract, combo = future_combo)
smart_future_combo_data <- ib_parsed_raw_list
last_incoming$smart_future_combo_data <- smart_future_combo_data[[length(smart_future_combo_data)]]

#5: Inter Commodity Futures Contract
inter_cmdty_futures_contract <- c(
  symbol = "CL.BZ", secType = "BAG", currency = "USD", exchange = "NYMEX"
)
inter_cmdty_futures_combo <- tibble::tibble(
  # Use req_contract_details() with vapply() to pick a random WTI and and a
  # random COIL future conId.
  conId = vapply(
    c("CL", "BZ"),
    function(symb){
      req_contract_details(
        contract = c(
          symbol = symb, exchange = "NYMEX", currency = "USD", secType = "FUT"
        )
      )$conId %>%
        sample(1)
    },
    character(1)
  ),
  ratio    = c(1,1),
  action   = c("BUY","SELL"),
  exchange = c("NYMEX", "NYMEX")
)
req_mkt_data(
  contract = inter_cmdty_futures_contract,
  combo    = inter_cmdty_futures_combo
)
inter_cmdty_futures_data <- ib_parsed_raw_list
last_incoming$inter_cmdty_futures_data <- inter_cmdty_futures_data[[
  length(inter_cmdty_futures_data)
  ]]

# News Feed for Query
news_feed_for_query_data <- req_mkt_data(
  contract = c(symbol = "BRFG:BRFG_ALL", secType = "NEWS", exchange = "BRFG")
)
news_feed_for_query_data <- ib_parsed_raw_list
last_incoming$news_feed_for_query_data <- news_feed_for_query_data[[
  length(news_feed_for_query_data)
  ]]
