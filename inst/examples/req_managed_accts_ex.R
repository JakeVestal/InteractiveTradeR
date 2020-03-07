\dontrun{
  # This example assumes that you're connected to a default PAPER Account
  # having five (5) trading accounts (prefix "DU") and an admin (prefix "DF"),
  # for six (6) accounts total.

  # Fetch managed accounts
  managed_accts <- req_managed_accts()
  # Print managed accounts
  managed_accts
}
