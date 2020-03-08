# Fetch matching symbols for pattern "international"
matching_symbols <- req_matching_symbols("international")
# Print matching symbols
matching_symbols

# If that doesn't satisfy, try a few more search strings. Wait 3 seconds after
# each one so as to be kind to IB's servers.
req_matching_symbols("ibm")
Sys.sleep(3)
req_matching_symbols("business")
Sys.sleep(3)
req_matching_symbols("machines")
Sys.sleep(3)

# Say you only wanted to know what asset types -- futures, options, warrants,
# etc -- that were available for the stock traded on the NYSE under the symbol
# "IBM". You could pipe the output of req_matching_symbols() to a series of
# functions with the end result of returning only the info you want:
req_matching_symbols("ibm") %>%
  dplyr::filter(symbol == "IBM" & primary_exchange == "NYSE") %>%
  dplyr::select(asset_types) %>%
  unlist(use.names = FALSE)
