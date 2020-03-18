# --> This example may be run on a paper trading account with no market data
# feed. This will be the case if you start up a new IB account without
# purchasing a subscription. In that case, you'll see a "Market Data not
# subscribed..." message, which means everything is operating normally. If you
# purchase a data subscription, this message will not appear.

# Create a connection to IB
create_new_connections()

#### Snapshot Example: Exxon stock
################################################################################
# Fetch a delayed market data snapshot for Exxon (XOM)
req_mkt_data(
  contract = c(
    symbol   = "XOM",
    secType  = "STK", 
    currency = "USD", 
    exchange = "SMART"
  ),
  snapshot    = TRUE,
  data_name   = "Exxon_delayed",
  mktDataType = "DELAYED"
) 

# Note that a "mkt_data" subscription now appears:
subscriptions$mkt_data

# For the sake of this example, wait 3 seconds or so to be sure that the data
# has time to arrive.
Sys.sleep(3)

# Read the data off the sock and update "mkt_data"
read_sock_drawer()

# Now you have a market data snapshot in your mkt_data slate! You can access it
# with the "$" operator, like this:
mkt_data$Exxon_delayed$TICK_PRICE
mkt_data$Exxon_delayed$TICK_SIZE
mkt_data$Exxon_delayed$`Delayed Last Timestamp`
mkt_data$Exxon_delayed$TICK_REQ_PARAMS
# ...etc

# Note that the mkt_data subscription has been destroyed. This is because you 
# set snapshot to TRUE -- when read_sock_drawer() finds a completed snapshot,
# it automatically updates the subscription.
subscriptions$mkt_data 


#### Subscription Example: Five stocks
################################################################################
# Snapshots are great if you just want to quickly fetch market data for a
# contract and move on. Often, however, you'll want to stay up-to-date on market
# data by creating a persistent subscription, causing the data in the mkt_data
# slate to update every time read_sock_drawer() is called.

# To do this, first create a sock for yourself (if you didn't already):
create_new_connections()

# Now, set up market data subscriptions to Tesla, Apple, Facebook, 3M,
# Interactive Brokers and whatever else you want to follow. Below, the walk()
# function from package "purrr" is used to call req_mkt_data on each stock.
c("TSLA", "AAPL", "FB", "MMM", "IBKR") %>%
  purrr::walk(
    function(stock_symbol){
      req_mkt_data(
        contract = c(
          symbol   = stock_symbol,
          secType  = "STK",
          currency = "USD",
          exchange = "SMART"
        ),
        mktDataType = "DELAYED",
        snapshot    = FALSE,
        channel     = "async"
      ) 
    }
  )

# Note that the mkt_data subscriptions you just created appear in the
# subscriptions variable:
subscriptions$mkt_data

# Wait a bit for the data to come through for example's sake...
Sys.sleep(3)

# Read the sock drawer as many times as you want
read_sock_drawer()

# Access your data:
mkt_data$`1_TSLA`$TICK_PRICE
mkt_data$`4_MMM`$TICK_PRICE
# ...etc

# Because these subscriptions aren't for snapshots, they'll stay active until
# cancelled. When you're ready, cancel any/all of your subscriptions:
cancel_mkt_data()

# Subscriptions are gone! 
subscriptions$mkt_data
