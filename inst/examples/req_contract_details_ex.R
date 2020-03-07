#### Example: supplying more parameters gives more specific results.
#### Goal: We would like to retrieve the contract details of IBM common stock,
####       which has conId = 8314.

# The fastest and simplest way to get IBM's contract details is by conId only
contract_details <- req_contract_details(8314)
contract_details

# You can select, manipulate, and view the fetched details:
contract_details$exchange_info # View the exchange info
contract_details$orderTypes    # See what order types are available
contract_details$conId         # Check the conId
# ... and so on.

# You can also use glimpse() to print the information in what may be an
# easier-to-read format:
dplyr::glimpse(contract_details)

# But what if you didn't already know the conId?

# 2) You could try to get IBM's contract details by symbol only, but because
# there are may securities across many exchanges that have the symbol "IBM"
# this query won't work -- IB responds by asking for more info.
contract_details_1 <- req_contract_details(contract = c(symbol = "IBM"))

# 3) So, try providing a valid security type:
contract_details_2 <- req_contract_details(
  contract = c(symbol = "IBM", secType  = "STK")
)
contract_details_2
# This call will work, but should return quite a few matching contracts. Only
# one of these is the contract of interest.

# 4) Narrow things down by specifying a currency:
contract_details_3 <- req_contract_details(
  contract = c(
    symbol   = "IBM",
    secType  = "STK",
    currency = "USD"
  )
)
contract_details_3
# This helped a bit, but still have a large number of matching contracts.

# 5) Specify an exchange:
contract_details_4 <- req_contract_details(
  contract = c(
    symbol   = "IBM",
    secType  = "STK",
    currency = "USD",
    exchange = "SMART"
  )
)
contract_details_4
# Success! For IBM, these four exchanges are enough to specify the contract.

# Bond details return slightly different parameters. See the "ContractDetails"
# documentation on IB's website at the link provided in the "Value" section.
broadcom_bond <- req_contract_details(359401413)
broadcom_bond
