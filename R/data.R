mcid_format <- function(
  rox_list = list(
    "@format integer between 1 and ",
    formattable::comma(
      functionary$max_client_id, digits = 0
    ),
    ", inclusive"
  )
){ do.call("paste0", rox_list) }


#' Master Client ID
#'
#' @description
#' A particular client ID that the user identifies within Trader Workstation or
#' IB Gateway. Within \link{InteractiveTradeR}, a socket that connects to TWS or
#' IBG using the Master Client ID is considered a \strong{master socket}.
#'
#' @name Master Client ID
#' 
#' @aliases master_client_id
#'
#' @eval mcid_format()
#'
NULL

#' Client ID 0: The Trader Workstation Client
#'
#' @description
#' A socket that connects to TWS or IBG using Client ID = 0 is considered a *TWS
#' socket*. TWS sockets have the unique property of being able to view orders
#' placed within the TWS app itself.
#'
#' @name Client ID 0
#' 
#' @aliases client_id_0
#' 
NULL

#' Subscriptions
#'
#' @description
#' The \code{subscriptions} object is a \link[tibble]{tibble} that keeps track
#' of what your sockets are subscribed to. Once InteractiveTradeR has been
#' loaded, active subscriptions can be fetched at any time simply by calling the
#' object by name: \code{subscriptions}.
#'
#' @name subscriptions
#' 
NULL

#' The Treasury
#' 
#' @description 
#' Contains up-to-date data pertaining to account balances, positions, P/L, and
#' other financials.
#' 
#' @name treasury
#' 
NULL

#' The Archives
#' 
#' @description 
#' Historical data is stored in the archives.
#' 
#' @name archives
#' 
NULL

#' The Error Log
#' 
#' @description 
#' Where to go to find out what went wrong.
#' 
#' @name error_log
#' 
NULL

#' mkt_data
#' 
#' @description 
#' Where live market data is kept
#' 
#' @name mkt_data
#' 
NULL

#' The \code{contract} Object
#'
#' @description
#' In InteractiveTradeR, a \code{contract} object is simply a named character
#' vector or list whose elements correspond to one or more of the
#' contract parameters described below. For example:
#'
#' \strong{AMD US Stock} \cr
#' \code{c(symbol = "AMD", secType  = "STK", currency = "USD", exchange =
#' "SMART")},
#'
#' \strong{EUR - GBP currency pair} \cr
#' \code{c(symbol = "EUR", secType  = "CASH", currency = "GBP", exchange =
#' "IDEALPRO")},
#'
#' \strong{US T-bill, 2.25\%, 31 March 2021} \cr
#' \code{c(symbol= "912828C57", secType = "BOND", exchange = "SMART", currency =
#' "USD")} , and
#'
#' \strong{E-mini S&P 500 Futures} \cr
#' \code{c(symbol = "ES", secType = "FUT", exchange = "GLOBEX", currency = "USD",
#' lastTradeDateOrContractMonth = "201903")} 
#'
#' ...are all valid \code{contract} objects.
#'
#' @section \code{conId} and \code{exchange} are Special:
#'
#' \strong{If a} \code{contract} \strong{object is un-named and has only a single element} 
#' (\code{length(contract) == 1} ), then InteractiveTradeR will treat the contract
#' object as though it contained a \code{conId} .
#'
#' \strong{If a} \code{contract} \strong{object is un-named and contains exactly two elements} 
#' (\code{length(contract) == 2} ), then InteractiveTradeR will treat the contract
#' object as though it contained a \code{conId} and an \code{exchange} (in any order).
#'
#' Because a \code{conId} is the unique identifier of any contract in IB's system,
#' this convention makes a few useful shortcuts possible in InteractiveTradeR:
#'
#' \strong{Functions} (except for \code{place_order} (), below) \strong{that accept a \code{contract}
#' argument require only that the \code{conId} be specified} . In other words,
#' \code{req_contract_details(8314)} will return the contract details for IBM US
#' Stock, whose \code{conId = 8314} . This syntax is much simpler than having to pass
#' in the full \strong{basic contract} information: \code{c(symbol = "IBM", secType =
#' "STK", currency = "USD", exchange = "SMART")} .
#'
#' \strong{For \code{place_order} (), the \code{contract} object must also include an exchange.} 
#' Because orders require the user to specify the exchange on which the order is
#' to be placed, the command
#'
#'  \code{place_order(
#'     8314,
#'     c(
#'       action        = "BUY",
#'       account       = "DU1234567",
#'       orderType     = "MKT",
#'       totalQuantity = 50
#'     )
#'   )} 
#'
#' would \strong{not} work to buy 50 shares of IBM because the exchange isn't
#' specified. Instead, the trader would need to include an exchange; for
#' example, the SMART exchange:
#'
#' \code{place_order(
#'     c(8314, "SMART"),
#'     c(
#'       action        = "BUY",
#'       account       = "DU1234567",
#'       orderType     = "MKT",
#'       totalQuantity = 50
#'     )
#'   )} 
#'
#'
#' @section Basic Contract Definitions:
#' The elements that may be used to create contract objects are defined below. 
#' \strong{Order of elements does not matter} in a \code{contract} object.
#' 
#' \itemize{
#'   \item \strong{conID} <numeric>: Interactive Brokers' unique numeric
#'   identifier for a specified contract
#'   \item \strong{symbol} <chr>: The exchange symbol under which the asset is
#'   traded, e.g., "FB", "AAPL", "IBM"
#'   \item \strong{secType} <chr>: Abbreviation describing security's type:
#'     \tabular{cccccc}{
#'       \strong{BAG} \tab \strong{BOND} \tab \strong{CASH}   \tab \strong{CMDTY} \tab \strong{FOP}   \tab \strong{FUND} \cr
#'       combo        \tab bond          \tab forex pair      \tab commodity      \tab futures option \tab mutual fund
#'     }
#'     \tabular{cccccc}{
#'       \strong{FUT} \tab \strong{IND}  \tab \strong{NEWS}   \tab \strong{OPT}   \tab \strong{STK}   \tab \strong{WAR}  \cr
#'       futures      \tab index         \tab news            \tab option         \tab stock or ETF   \tab warrant
#'     }
#'   \item \strong{lastTradeDateOrContractMonth} For options & futures, the last
#'   trading day or contract month (as applicable), in YYYYMMDD and YYYYMM
#'   format, respectively.
#'   \item \strong{strike} <numeric>: Strike price of asset, if applicable.
#'   \item \strong{right} <chr>: If applicable, the "right" parameter of a
#'   contract, e.g. "C" (right to buy, for a call), "P" (right to sell, put).
#'   \item \strong{multiplier} <numeric>: The "lot size" of a contract, if
#'   applicable; e.g., an options contract for 100 shares of underlying.
#'   \item \strong{exchange} <chr>: Destination exchange; i.e., the exchange on
#'   which the contract is to be traded.
#'   \item \strong{currency} <chr>: 3-letter abbreviation of the currency in
#'   which the contract is traded, e.g. "USD", "HKD".
#'   \item \strong{localSymbol} <chr>: The symbol under which the contract is
#'   traded in its primary exchange.
#'   \item \strong{primaryExchange} <chr>: Main exchange on which a contract is
#'   traded, e.g., "NASDAQ", "NYSE".
#'   \item \strong{tradingClass} <chr>: Code that classifies an asset based on
#'   the manner in which it is traded. Stocks, for example, have
#'   \code{tradingClass} = "NMS" denoting the United States'
#'   \href{https://en.wikipedia.org/wiki/National_Market_System}{National Market
#'   System} for equities.
#'   \item \strong{includeExpired} <logical>: If TRUE, expired options contracts 
#'   may be included in the output. Does not work for expired options or other
#'   financial instruments; e.g, delisted stocks.
#'   \item \strong{SecIdType} <chr>: Specifies the type of security ID provided 
#'   in \emph{SecId}, the next argument; for example, ISIN or CUSIP.
#'   \item \strong{SecId} <chr>: Gives the value of the security ID whose type
#'   you specified via \emph{SecIdType}. For example, a \code{contract} object
#'   specifying IBM by ISIN would contain the elements \emph{SecIdType} = "ISIN
#'   and \emph{SecId} = "US4592001014".
#'   \item \strong{comboLegsDescrip} <chr>: short description of combo legs, if
#'   a combo contract is being defined.
#'   \item \strong{comboLegs} <\link[tibble]{tibble}>: Defines the individual
#'   contract legs for placing a
#'   \href{https://www.interactivebrokers.com/php/whiteLabel/Interoperability/Socket_Client_Java/java_combo.htm}{Combination
#'   Order}. Each row in the tibble corresponds to a contract leg. Only those
#'   columns necessary to completely specify a combo leg's identity, exchange,
#'   and order weight need to be specified -- not all columns are required. For
#'   example, the following \code{comboLegs} tibble is sufficient to place a 1:1
#'   combo order for the two stocks identified by \code{conId}:
#'   \tabular{cccc}{
#'     \strong{conId}   \tab \strong{ratio}   \tab \strong{action}    \tab \strong{exchange}  \cr
#'     \code{<numeric>} \tab \code{<numeric>} \tab \code{<character>} \tab \code{<character>} \cr
#'     \code{43645865}  \tab \code{1}         \tab \code{"BUY"}       \tab \code{"SMART"}     \cr
#'     \code{9408}      \tab \code{1}         \tab \code{"SELL"}      \tab \code{"SMART"}     \cr
#'   }
#'   \code{comboLegs} may have the following columns:
#'     \itemize{
#'       \item \strong{conId} <numeric>: IB's unique identifier for the contract
#'       \item \strong{ratio} <numeric>: The contract's weight in the combo
#'       order in terms of relative number of contracts; e.g., in a combo order
#'       to buy 25% A and 75% B, the \code{ratio} of A and B is 1 and 3.
#'       \item \strong{action} <character>: "BUY" or "SELL".
#'       \item \strong{exchange} <character>: Specifies exchange on which the
#'       order is intended to be placed.
#'       \item \strong{openClose} <numeric>: Only necessary for institutional
#'       (non-retail) customers. If you don't know what that is, then you're
#'       probably a retail customer and don't need to worry about this. Only
#'       takes the values \strong{1} \strong{2}, or \strong{3}, to signify
#'       \strong{opening}, \strong{closing}, or \strong{unknown}.
#'       \item \strong{shortSaleSlot} <numeric>: Only takes the values
#'       \strong{1} or \strong{2} specifying the short sale service as
#'       \strong{clearing broker} or \strong{third party}, respectively.
#'       \item \strong{designatedLocation} <character>: If \code{shortSaleSlot
#'       == 2} (third party), specifies the third party to service the short.
#'   }
#'   \strong{deltaNeutralContract} <\link[tibble]{tibble}>: Defines the
#'   individual contract legs for what IB considers a delta neutral contract.
#'   Each row in the tibble corresponds to an \strong{underlying} contract,
#'   which may be STK or FUT, upon which derivative instruments are constructed.
#'   The tibble must have three columns:
#'   \itemize{
#'     \item \emph{conId}: IB's \code{conId} for the contract
#'     \item \emph{delta}: The delta value for the underlying contract
#'     \item \emph{price}: Price of the underlying
#'   }
#' }
#'
#' @name contracts
#'
#' @aliases contract
#'
NULL
