#### NOTE: don't forget to first create some open orders -- either via
#### InteractiveTradeR or the TWS -- before trying the examples below.
#### Otherwise, they will return empty values!

# 1) Fetch all open orders, store the updated data in open_orders_sync
open_orders_sync <- req_all_open_orders()
open_orders_sync

# Note that the treasury objects are also updated
dplyr::glimpse(treasury$OPEN_ORDERS)
treasury$ORDER_STATUSES

### Clean slate for next example
clean_slate("treasury")

#2) Using a specific socket
create_new_connections(1)
req_all_open_orders("async")
dplyr::glimpse(treasury$OPEN_ORDERS)
treasury$ORDER_STATUSES
