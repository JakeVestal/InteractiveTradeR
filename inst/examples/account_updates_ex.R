# Clear out the treasury, sock drawer, and subscriptions
clean_slate()

####
#### Example 1: Fetch account update for one account
####

# Pick a random account for this example or specify one yourself,
#   e.g., acct <- "DU1234567"
acct <- sample(req_managed_accts(), 1)
acct

# Fetch account updates for the account, without starting a subscription
req_account_updates(acct)

# Print the ACCOUNTS
treasury$ACCOUNTS

# Print the PORTFOLIO_VALUE
treasury$PORTFOLIO_VALUE # empty list if no positions

####
#### Example 2: Fetch account update data for many accounts
####

clean_slate() # clean slate (optional)

treasury$ACCOUNTS      # should return NULL
treasury$PORTFOLIO_VALUE # should return NULL

# Fetch account updates for all of your accounts using the walk() function
# from the purrr package:
req_managed_accts() %>%
  purrr::walk(
    function(account){
      # Sys.sleep(1/50)
      req_account_updates(acctCode = account)
    }
  )

# Uncomment the "Sys.sleep(1/50)" to pause execution for one fiftieth of a
# second between each call to req_account_updates(). The reason you may want
# to do this is that Interactive brokers only allows, at max, 50 API calls per
# second. If you have more than 50 accounts and a fast computer & connection,
# the "Sys.sleep(1/50)" prevents you from exceeding the limit.

# Check that all accounts are represented in ACCOUNTS:
identical(
  sort(unique(treasury$ACCOUNTS$account)),
  sort(req_managed_accts())
)

####
#### Example 3: Persistent account update subscriptions
####

# To create an ongoing subscription that continuously collects account
# updates, make the same calls to req_account_updates() as above, but use a
# persistent socket.

clean_slate() # clean slate, optional

treasury$ACCOUNTS        # should return NULL
treasury$PORTFOLIO_VALUE # should return NULL

# Open up a socket
create_new_connections(1)

# Pick another random account, or replace the "sample()" with an account code
# (e.g., "DU7654321") as desired
acct2 <- sample(req_managed_accts(), 1)

# Make the call, this time with channel = "async"
req_account_updates(acct2, channel = "async")

# Within three minutes of starting the subscription, take a look at the
# ACCOUNTS and PORTFOLIO_VALUE objects in the treasury:
treasury$ACCOUNTS
treasury$PORTFOLIO_VALUE
# See when they were last updated:
acc_val_update_time  <- attr(treasury$ACCOUNTS,      "last_updated")
acc_val_update_time
port_val_update_time <- attr(treasury$PORTFOLIO_VALUE, "last_updated")
port_val_update_time

# Soon after creating the subscription, try to update the treasury objects by
# calling read_sock_drawer():
read_sock_drawer()

# If you're quick enough, you won't get any updated information because IB has
# not sent updated data to the socket.

# Wait a little over 3 minutes
Sys.sleep(200)

# Keep calling...
read_sock_drawer()
# ...a few times, waiting 10 or 20 seconds in between calls. After 3 minutes
# have passed -- but probably before that -- you should see either or both of
# the ACCOUNTS and PORTFOLIO_VALUE objects update.

# After updating, take a look in the treasury:
treasury$ACCOUNTS
treasury$PORTFOLIO_VALUE

# And compare update times:
acc_val_update_time
attr(treasury$ACCOUNTS,"last_updated")

port_val_update_time
attr(treasury$PORTFOLIO_VALUE, "last_updated")

#### Example 4: Cancelling Subscriptions

# Cancel the subscription created in Example 3 with:
req_account_updates(acct2, subscribe = FALSE)

