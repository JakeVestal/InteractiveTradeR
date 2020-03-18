\dontrun{
  # This example assumes that you're connected to a default PAPER Account
  # having five (5) trading accounts (prefix "DU") and an admin (prefix "DF"),
  # for six (6) accounts total.

  #### Step 1:
  # Open TWS and use it to create some open orders that are unlikely to fill
  # (e.g., limit orders with a low BUY price)

  #### Step 2:
  # Within InteractiveTradeR, run:
  req_all_open_orders()
  # You should see that the order(s) you placed within the TWS itself are
  # visible within InteractiveTradeR, but have no order ID (or all orderId == 0)
  # and therefore cannot be modified / cancelled.

  #### Step 3:
  # Within InteractiveTradeR, run:
  req_open_orders()
  # Now, the orders you created within TWS will have IDs that can be used by
  # cancel_order() or place_order to cancel or modify, respectively.
}
