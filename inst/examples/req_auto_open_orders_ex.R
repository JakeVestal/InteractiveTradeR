\dontrun{
  # This example assumes that you're connected to a default PAPER Account
  # having five (5) trading accounts (prefix "DU") and an admin (prefix "DF"),
  # for six (6) accounts total.

  #### Step 1:
  # a) Open TWS and use it to create some open orders that are unlikely to fill
  #    (e.g., limit orders with a low BUY price).
  # b) Create a socket connection to TWS on Client ID 0:
  create_new_connections(1, include_tws = TRUE)

  #### Step 2:
  # Within InteractiveTradeR, run:
  req_all_open_orders()
  # You should see that the order(s) you placed within the TWS itself are
  # visible within InteractiveTradeR, but have no order ID (or all orderId == 0)
  # and therefore cannot be modified / cancelled.

  #### Step 3:
  # Within InteractiveTradeR, run:
  req_auto_open_orders()

  #### Step 4:
  # Open a few more orders within TWS

  #### Step 5:
  # Retrieve open orders again:
  req_all_open_orders()
  # This time, you should see that the unfilled orders created after the call to
  # req_auto_open_orders() do have orderIds.

  #### Orders created in the TWS will continue to be assigned orderIds until the
  #### connection opened in Step 1b) is closed, or a call is made to stop
  #### binding new orders via req_auto_open_orders(FALSE).
}
