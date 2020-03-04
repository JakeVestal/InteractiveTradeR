\dontrun{
  # This example assumes that you're connected to a default PAPER Account
  # having five (5) trading accounts (prefix "DU") and an admin (prefix "DF"),
  # for six (6) accounts total.

  # Fetch the current time
  current_time <- req_current_time()

  # Verify that current_time is a POSIXct object
  class(current_time)

  # Print current_time
  current_time
}
