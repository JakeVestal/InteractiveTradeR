# Query Option Parameters for IBM Stock
ibm_opt_params <- req_sec_def_opt_params(
  underlyingSymbol  = "IBM",
  underlyingSecType = "STK",
  underlyingConId   = 8314
)

# Print the params:
ibm_opt_params

# Print just the strike prices:
ibm_opt_params$strikes

# median strike price:
median(ibm_opt_params$strikes[[1]])

# Print just the expirations:
ibm_opt_params$expirations

# Find latest expiration:
max(ibm_opt_params$expirations[[1]])
