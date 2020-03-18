\dontrun{
  # This example assumes that you're connected to a default PAPER Account
  # having five (5) trading accounts (prefix "DU") and an admin (prefix "DF"),
  # for six (6) accounts total.



  #### 1) EXAMPLE SETUP
  #         Create 10 example orders to experiment with.

  # Start from a clean slate by wiping all IB connections
  disconnect()

  # Open up 5 connections: "sock_1", "sock_2", and "sock_3", plus a "master" and
  # a "tws" socket.
  create_new_connections(5, include_master = TRUE, include_tws = TRUE)

  # Contract object; Interactive Brokers stock
  ibkr_stock <- c(
    symbol   = "IBKR",
    secType  = "STK",
    currency = "USD",
    exchange = "SMART"
  )

  # Limit order to buy 1 share of IBKR for $5 (not likely to ever fill)
  ibkr_lmt_order <- c(
    action          = "BUY",
    # Randomly picks an account with more than $5000 in AvailableFunds
    account         = req_account_summary(tags = "AvailableFunds") %>%
      dplyr::filter(as.numeric(tag_value) > 5000) %>%
      dplyr::select(account) %>%
      unlist(use.names = FALSE) %>%
      sample(1),
    orderType       = "LMT",
    totalQuantity   = 1,
    lmtPrice        = 5
  )

  # Place 15 orders (or as many as you want!) randomly on the 5 open clients
  for(client in sample(socks(), 15, replace = TRUE)){
    place_order(
      ibkr_stock,
      ibkr_lmt_order,
      channel = client
    )
  }

  # Take a look at the orders you've opened:
  req_all_open_orders()

  #### Setup complete!


  # 1) Cancel By Row
  # Cancel the first 3 rows reported by req_all_open_orders() simply by passing
  # them to cancel_order():
  cancel_by_row_response <- req_all_open_orders() %>%
    head(3) %>%
    cancel_order()

  # View open orders
  req_all_open_orders()

  # 2) Cancel by permId
  # Any vector or column passed in to cancel_order() is assumed to contain
  # permIds. You may either create a vector of permIds yourself using c(); e.g.,
  # c(864268603, 864268605, 864268605), or by following along the below which
  # selects 3 random permIds to cancel:
  cancel_by_row_response <- req_all_open_orders() %>% {
    .[sample(nrow(.), 3), ]
  } %>%
    dplyr::select(permId) %>%
    cancel_order()




}
