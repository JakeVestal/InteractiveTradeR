# # The difference between ledgerAndNLV == TRUE vs. FALSE Fetch an update for
# # default group "All" with default ledgerAndNLV (TRUE)
# req_account_updates_multi()
# # Save the treasury object in a variable to use for comparing later:
# aum_true <- treasury$ACCOUNTS
# 
# # Clear out the treasury so that the two aum_* vars are easier to compare
# clean_slate()
# 
# # Do the same thing again, but with ledgerAndNLV = FALSE:
# req_account_updates_multi(ledgerAndNLV = FALSE)
# aum_false <- treasury$ACCOUNTS
# 
# # Compare the two:
# #   3.1) Same account:
# unique(aum_true$account)
# unique(aum_false$account)
# #   3.2) ledger_and_NLV = FALSE includes more parameters:
# unique(aum_true$tag)
# unique(aum_false$tag)
# 
# 
# ####
# #### Subscriptions
# ####
# 
# # Clean slate
# clean_slate()
# 
# # Open a socket
# create_new_connections()
# 
# # Fetch the account IDs of your six paper trading accounts and use walk() from
# # the purrr package to subscribe to each one
# req_managed_accts() %>%
#   purrr::walk(
#     req_account_updates_multi,
#     channel = "async"
#   )
# 
# # Verify that you're now subscribed to the six paper trading accounts:
# view_subscriptions("account_updates_multi")
# 
# # Print the retrieved updates to the console, or use View():
# treasury$ACCOUNTS
# View(treasury$ACCOUNTS)
# 
# # You should have all six paper account codes represented in the "account"
# # column of the ACCOUNTS treasury object.
# 
# # This information will update every 3 minutes -- and probably more frequently
# # than that in practice -- for those accounts that have positions in financial
# # instruments. You can wait for at least one cycle and call read_sock_drawer()
# # again to see this for yourself.
# 
# # Save the treasury object to use for comparing later
# before_cancel <- treasury$ACCOUNTS
# 
# # When you're ready, cancel a subscription or two: how about the 1st and 3rd?
# cancel_accounts <- subscriptions$account_updates_multi$req_name[c(1,3)]
# cancel_account_updates_multi(cancel_accounts)
# 
# # Check that the two accounts are indeed removed from subscriptions:
# view_subscriptions("account_updates_multi")
# any(cancel_accounts %in% subscriptions$account_updates_multi$req_name)
# 
# # From this point on, the sock drawer will no longer get updated data for the
# # two accounts that were unsubscribed.
# 
# # To convince yourself, first read off any data that might have gotten sent
# # in the time between the last read and the call to cancel:
# read_sock_drawer()
# 
# # From this point on, the canceled accounts' treasury data -- which can be
# # selected using the following code:
# treasury$ACCOUNTS %>%
#   dplyr::filter(account %in% cancel_accounts)
# # -- will not update, no matter how many times you call read_sock_drawer(),
# # unless you subscribe to them again.
# 
# ####
# #### CANCELLING accounts_multi Subscriptions
# ####
# 
# # Clear out the treasury & subscriptions for this example
# clean_slate(c("treasury", "subscriptions"))
# 
# # Open a socket
# create_new_connections()
# 
# # Fetch the account IDs of your six paper trading accounts and use walk() from
# # the purrr package to subscribe to each one
# req_managed_accts() %>%
#   purrr::walk(
#     req_account_updates_multi,
#     channel = "async"
#   )
# 
# # Verify that you're now subscribed to the six paper trading accounts:
# view_subscriptions("account_updates_multi")
# 
# # Print the retrieved updates to the console, or use View():
# treasury$ACCOUNTS
# View(treasury$ACCOUNTS)
# 
# # You should have all six paper account codes represented in the "account"
# # column of the ACCOUNTS treasury object.
# 
# # This information will update every 3 minutes -- and probably more frequently
# # than that in practice -- for those accounts that have positions in financial
# # instruments. You can wait for at least one cycle and call read_sock_drawer()
# # again to see this for yourself.
# 
# # Save the treasury object to use for comparing later
# before_cancel <- treasury$ACCOUNTS
# 
# # When you're ready, cancel a subscription or two: how about the 1st and 3rd?
# cancel_accounts <- subscriptions$account_updates_multi$req_name[c(1,3)]
# cancel_account_updates_multi(cancel_accounts)
# 
# # Check that the two accounts are indeed removed from subscriptions:
# view_subscriptions("account_updates_multi")
# any(cancel_accounts %in% subscriptions$account_updates_multi$req_name)
# 
# # From this point on, the sock drawer will no longer get updated data for the
# # two accounts that were unsubscribed.
# 
# # To convince yourself, first read off any data that might have gotten sent
# # in the time between the last read and the call to cancel:
# read_sock_drawer()
# 
# # From this point on, the canceled accounts' treasury data -- which can be
# # selected using the following code:
# treasury$ACCOUNTS %>%
#   dplyr::filter(account %in% cancel_accounts)
# # -- will not update, no matter how many times you call read_sock_drawer(),
# # unless you subscribe to them again.
