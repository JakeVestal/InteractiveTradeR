#' The Functionary: A 'dictionary' for functions
#'
#' Coded values needed for communicating with IB's servers over socket.
#'
#' @keywords internal
"functionary"

mcid_format <- function(
  rox_list = list(
    "@format integer between 1 and ",
    formattable::comma(
      InteractiveTradeR::functionary$max_client_id, digits = 0
    ),
    ", inclusive"
  )
){ do.call("paste0", rox_list) }

#' Master Client ID
#'
#' @description
#' A particular client ID that the user identifies within Trader Workstation or
#' IB Gateway. Within \link{InteractiveTradeR}, a socket that connects to TWS or
#' IBG using the Master Client ID is considered a *master socket*.
#'
#' The Master Client ID is special becayse any call made **LIST CALLS HERE**, if
#' made on a master socket, will ---, whereas for a normal one, ---.
#'
#' @eval mcid_format()
#'
"master_client_id"

#' Client ID 0: The Trader Workstation Client
#'
#' @description
#' A socket that connects to TWS or IBG using Client ID = 0 is considered a *TWS
#' socket*. TWS sockets have the unique property of being able to view orders
#' placed within the TWS app itself.
#'
"client_id_0"

#' The `contract` Object and Contract Parameters
#'
#' @description
#' In InteractiveTradeR, a `contract` object is simply a named character vector
#' or list whose elements correspond to one or more of the `contract_parameters`
#' below. For example:
#'
#' **AMD US Stock** \cr
#' `c(symbol = "AMD", secType  = "STK", currency = "USD", exchange = "SMART")`,
#'
#' **EUR - GBP currency pair** \cr
#' `c(symbol = "EUR", secType  = "CASH", currency = "GBP", exchange =
#' "IDEALPRO")`,
#'
#'  **US T-bill, 2.25\%, 31 March 2021** \cr
#' `c(symbol= "912828C57", secType = "BOND", exchange = "SMART", currency =
#' "USD")`, and
#'
#'  **E-mini S&P 500 Futures** \cr
#' `c(symbol = "ES", secType = "FUT", exchange = "GLOBEX", currency = "USD",
#' lastTradeDateOrContractMonth = "201903")`
#'
#' ...are all valid `contract` objects.
#'
#' @section `conId` and `exchange` are Special:
#'
#' **If a `contract` object is un-named and has only a single element**
#' (`length(contract) == 1`), then InteractiveTradeR will treat the contract
#' object as though it contained a `conId`.
#'
#' **If a `contract` object is un-named and contains exactly two elements**
#' (`length(contract) == 2`), then InteractiveTradeR will treat the contract
#' object as though it contained a `conId` and an `exchange` (in any order).
#'
#' Because a `conId` is the unique identifier of any contract in IB's system,
#' this convention makes a few useful shortcuts possible in InteractiveTradeR:
#'
#' **Functions** (except for `place_order`(), below) **that accept a `contract`
#' argument require only that the `conId` be specified**. In other words,
#' `req_contract_details(8314)` will return the contract details for IBM US
#' Stock, whose `conId = 8314`. This syntax is much simpler than having to pass
#' in the full **basic contract** information: `c(symbol = "IBM", secType =
#' "STK", currency = "USD", exchange = "SMART")`.
#'
#' **For `place_order`(), the `contract` object must also include an exchange.**
#' Because orders require the user to specify the exchange on which the order is
#' to be placed, the command
#'
#'  `place_order(
#'     8314,
#'     c(
#'       action        = "BUY",
#'       account       = "DU1234567",
#'       orderType     = "MKT",
#'       totalQuantity = 50
#'     )
#'   )`
#'
#' would **not** work to buy 50 shares of IBM because the exchange isn't
#' specified. Instead, the trader would need to include an exchange; for
#' example, the SMART exchange:
#'
#' `place_order(
#'     c(8314, "SMART"),
#'     c(
#'       action        = "BUY",
#'       account       = "DU1234567",
#'       orderType     = "MKT",
#'       totalQuantity = 50
#'     )
#'   )`
#'
#'
#' @section Basic Contract Definitions:
#'
#'
#' * **comboLegs** <\link[tibble]{tibble}>: Defines the individual contract legs
#' for placing a
#' \href{https://www.interactivebrokers.com/php/whiteLabel/Interoperability/Socket_Client_Java/java_combo.htm}{Combination
#' Order}. Each row in the tibble corresponds to a contract leg. Only those
#' columns necessary to completely specify a combo leg's identity, exchange, and
#' order weight need to be specified -- not all columns are required.
#'
#' For example, the following `comboLegs` tibble is sufficient to place a 1:1
#' combo order for the two stocks identified by `conId`:
#' \tabular{cccc}{
#' **`conId`** \tab **`ratio`** \tab **`action`**  \tab **`exchange`** \cr
#' `<numeric>` \tab `<numeric>` \tab `<character>` \tab `<character>`  \cr
#' `43645865`  \tab `1`         \tab `"BUY"`       \tab `"SMART"`      \cr
#' `9408`      \tab `1`         \tab `"SELL"`      \tab `"SMART"`      \cr
#' }
#'
#' `comboLegs` may have the following columns:
#'
#'   * **conId** <numeric>: IB's unique numeric identifier for the contract
#'
#'   * **ratio** <numeric>: The contract's weight in the combo order in terms of
#'   relative number of contracts; e.g., in a combo order to buy 25% A and 75%
#'   B, the `ratio` of A and B might be 1 and 3, respectively.
#'
#'   * **action** <character>: "BUY" or "SELL".
#'
#'   * **openClose** <numeric>: Only necessary for institutional (non-retail)
#'   customers. If you don't know what that is, then you're probably a retail
#'   customer and don't need to worry about this. Only takes the values **1**,
#'   **2**, or **3**, which, respectively, identify an order as **opening** or
#'   **closing** a position, or **unknown**.
#'
#'   * **shortSaleSlot** <numeric>: Only takes the values **1** or **2**
#'   specifying the short sale service as **clearing broker** or **third
#'   party**, respectively.
#'
#'   * **designatedLocation** <character>: If `shortSaleSlot == 2` (third
#'   party), specifies the third party that will service the short.
#'
#' * **comboLegsDescrip**
#' * **conID** <numeric>: Interactive Brokers' unique numeric identifier for a
#' specified contract
#' * **symbol** <chr>: The exchange symbol under which the asset is traded,
#' e.g., "FB", "AAPL", "IBM"
#' * **secType** <chr>: Abbreviation describing security's type:
#' \tabular{cccccc}{
#' **BAG** \tab **BOND** \tab **CASH**   \tab **CMDTY** \tab **FOP**        \tab **FUND**    \cr
#' combo   \tab bond     \tab forex pair \tab commodity \tab futures option \tab mutual fund
#' }
#' \tabular{cccccc}{
#' **FUT** \tab **IND** \tab **NEWS** \tab **OPT** \tab **STK**      \tab **WAR**  \cr
#' futures \tab index   \tab news     \tab option  \tab stock or ETF \tab warrant
#' }
#' * **lastTradeDateOrContractMonth** For options & futures, the last
#' trading day or contract month (as applicable), in YYYYMMDD ("\\%Y\\%m\\%d"
#' format in R) and YYYYMM ("\\%Y\\%m" format in R) format, respectively.
#' * **strike** <numeric>: Strike price of asset, if applicable.
#' * **right** <chr>: If applicable, the "right" parameter of a contract, e.g.
#' "C" (right to buy, for a call), "P" (right to sell, put).
#' * **multiplier** <numeric>: The "lot size" of a contract, if applicable;
#' e.g., an options contract that affects 100 shares of underlying. Numeric.
#' * **exchange** <chr>: Destination exchange; i.e., the exchange on which the
#' contract is to be traded.
#'  * **currency** <chr>: 3-letter abbreviation of the currency in which the
#'  contract is traded, e.g. "USD", "HKD".
#' * **localSymbol** <chr>: The symbol under which the contract is traded in its
#' primary exchange.
#' * **primaryExchange** <chr>: Main exchange on which a contract is traded,
#' e.g., "NASDAQ", "NYSE".
#' * **tradingClass** <chr>: Code that classifies an asset based on the manner
#' in which it is traded. Stocks, for example, have `trading_class` = "NMS"
#' denoting the United States'
#' \href{https://en.wikipedia.org/wiki/National_Market_System}{National Market
#' System} for equities.
#'
#' @name contract_parameters
#'
#' @aliases contract
#'
NULL
