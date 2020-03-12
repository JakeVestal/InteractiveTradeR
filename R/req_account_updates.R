#' Request Account Updates
#'
#' @description
#' This function either fetches or sets up a subscription to receive the data
#' that appear in the
#' "\href{https://institutions.interactivebrokers.com/en/software/tws/usersguidebook/realtimeactivitymonitoring/the_account_window.htm}{Account
#' Window}" window of Trader Workstation for the single account specified in the
#' \emph{acctCode} input. Unlike \link{req_account_updates_multi}(),
#' \code{req_account_updates}() can only subscribe to one account at a time.
#'
#' @section Behavior:
#' IB's documentation states that subscribed socket's account updates data will
#' be updated \strong{every three (3) minutes} unless there is a position
#' change, in which case updates will be sent immediately.  However, during
#' market hours, you may observe much shorter, more frequent update intervals.
#' You can explore your actual observed update frequency by experimenting with
#' Example 3 in the \strong{"Examples"} section below.
#'
#' @param acctCode
#' Character vector of length 1 containing the Account ID of the account for
#' which updates are sought. Possible choices include any one of the elements of
#' the output of \link{req_managed_accts}().
#'
#' @inheritParams req_account_summary
#'
#' @param subscribe
#' Boolean. If TRUE, then a subscription is started and the socket passed in as
#' \emph{channel} will continue to receive updated account info from Interactive
#' Brokers every time \link{read_sock_drawer}() is called and an update is
#' available on the socket.
#'
#' @details
#' \strong{Account Keys}: Several values of \emph{param} in the
#' \strong{ACCOUNTS} element of the output may or may not display suffixes; for
#' example, the \emph{param}
#' \strong{AccruedDividend}. The meaning of these suffixes is as follows:
#' \itemize{
#' \item "\strong{-C}": Applies to commodities
#' \item "\strong{-S}": Applies to stocks
#' \item \strong{no suffix}: Values reported are totals
#' }
#'
#' \strong{"All" Option}: If you use a Financial Advisory (FA) account
#' structure, then you have the option of requesting account updates for all of
#' the sub-accounts that fall under the master account. To do this, append the
#' letter \strong{"A"} to the end of the master account's ID and pass this value
#' to \code{req_account_updates}() as \emph{acctCode}; for example, "F7654321A".
#'
#' \strong{No "\code{cancel_}" function, use \code{subscribe = FALSE}}: Unlike
#' \link{req_account_summary}() and \link{req_account_updates_multi}(),
#' \code{req_account_updates}() does not have a companion function that cancels
#' the subscription. Instead, subscriptions are canceled on a socket by calling
#' \code{req_account_updates()} with \code{subscribe = FALSE}. See the
#' \strong{Examples} section below.
#'
#' \strong{One account at a time / Overwriting subscriptions}: Even if you use
#' different sockets, IB's API protocol is set up in such a way that you may
#' only have one active \strong{req_account_updates}() subscription running at
#' any given time. If you request another subscription, the new one will simply
#' overwrite the old one without an error message.
#'
#' \strong{Single Account}: If the user has access to only one account, then
#' supplying a value for \emph{acctCode} is not necessary and may be left blank
#' in the function call.
#'
#' \strong{Time Zone}: Interactive Brokers' servers use a very simple
#' hour:minute formatted timestamp the response data reported by
#' \code{req_account_updates}() in the \emph{acct_update_time} column of the
#' output. The timestamp does not include a time zone because it is understood
#' that the times reported are denominated in terms of the time zone set by
#' clicking the "More Options" link in the login window of either TWS or IBG.
#'
#' \strong{Cancelling subscriptions}: Use the command
#' \code{req_account_updates(subscribe = FALSE)}. When cancelling a subscription
#' made by \code{req_account_updates}() in InteractiveTradeR, any value passed
#' in as \emph{channel} is ignored, so it can be left out entirely. This
#' shortcut follows as consequence of the fact that IB only allows one
#' \code{req_account_updates}() subscription at a time.
#'
#' @section ACCOUNTS and PORTFOLIO VALUE Treasury Objects:
#'   \code{req_account_updates}() updates the \strong{ACCOUNTS} and
#'   \strong{PORTFOLIO VALUE} objects in the \strong{\code{treasury}}. Their
#'   structure is set forth as follows:
#'
#' \strong{ACCOUNTS}: A \link[tibble]{tibble} in which each row represents a
#' parameter pertaining to a particular account. Has the following columns:
#' \itemize{
#'   \item \strong{tag} <chr>: Name of account parameter, (e.g., "DayTradesRemaining")
#'   \item \strong{tag_value} <chr>: Value of the \emph{param} (e.g., "3")
#'   \item \strong{currency} <chr>: 3-letter currency abbreviation if \emph{tag_value} is a
#'   monetary amount, "" otherwise.
#'   \item \strong{account} <chr>: Account ID of the account to which the data applies.
#'   Included so that \emph{ACCOUNTS} data returned for different accounts can be
#'   combined without losing track of which account applies to which data.
#' }
#'
#' \strong{PORTFOLIO_VALUE}: A \link{tibble} in which every row represents an asset
#' held in an account. Has the following columns:
#' \itemize{
#'   \item \strong{account} <chr>: Account ID of the account to which the data
#'   applies. Included so that \emph{PORTFOLIO_VALUE} data returned for
#'   different accounts can be combined without losing track of which account
#'   applies to which data.
#'   \item \strong{con_id} <chr>: Interactive Brokers' unique contract ID for
#'   the asset.
#'   \item \strong{symbol} <chr>: The exchange symbol under which the asset is
#'   traded, e.g., "FB", "AAPL", "IBM".
#'   \item \strong{sec_type} <chr>: Three-letter abbreviation for the class of
#'   the asset in the row, e.g., "STK", "CFD", "BOND", and so on.
#'   \item \strong{last_trade_date_or_contract_month} For options & futures, the
#'   last trading day or contract month (as applicable), in YYYYMMDD
#'   ("\\%Y\\%m\\%d" in R) and YYYYMM ("\\%Y\\%m" in R) format, respectively.
#'   \item \strong{strike} <numeric>: Strike price of asset, if applicable.
#'   \item \strong{right} <chr>: If applicable, the "right" parameter of a
#'   contract, e.g. "C" (right to buy, for a call), "P" (right to sell, put).
#'   \item \strong{multiplier} <dbl>: The "lot size" of a contract, if
#'   applicable; e.g., an options contract that affects 100 shares of
#'   underlying. Numeric.
#'   \item \strong{primary_exchange} <chr>: Main exchange on which a contract is
#'   traded, e.g., "NASDAQ", "NYSE".
#'   \item \strong{currency} <chr>: 3-letter abbreviation of the currency in
#'   which the contract is traded, e.g. "USD", "HKD".
#'   \item \strong{local_symbol} <chr>: The symbol under which the contract is
#'   traded in its primary exchange.
#'   \item \strong{trading_class} <chr>: Code that classifies an asset based on
#'   the manner in which it is traded. Stocks, for example, have
#'   \emph{trading_class} = "NMS" denoting the United States'
#'   \href{https://en.wikipedia.org/wiki/National_Market_System}{National Market
#'   System} for equities.
#'   \item \strong{position} <dbl>: Numeric, long or short. Denotes how many of
#'   the contracts are held. Don't forget \emph{multiplier}, if applicable!
#'   \item \strong{market_price} <numeric>: The market price of the contract on
#'   a per-contract basis as measured at time = \emph{acct_update_time}.
#'   \item \strong{market_value} <numeric>: \emph{market_price} *
#'   \emph{position}; i.e., market value of the account's position in a
#'   particular contract.
#'   \item \strong{average_cost} <numeric>: The effective price of the account's
#'   overall position in the asset, including transaction costs.
#'   \item \strong{unrealized_PNL} <numeric>: Equals \emph{market_value} -
#'   \emph{average_cost} * \emph{position} and gives the profit or loss that
#'   would result if the position were closed at \emph{market_price}. Does
#'   \strong{not} take into account transction costs of closing the position.
#'   \item \strong{realized_PNL} <numeric>: Gives the real-world, historical
#'   profit or loss earned on positions that are now closed. Includes
#'   transaction costs.
#' }
#'
#' @inherit req_account_summary return
#'
#' @example inst/examples/account_updates_ex.R
#' @family treasury
#' @export
#'
req_account_updates <- function(
  acctCode    = "All",
  subscribe   = TRUE,
  channel     = NULL,
  return_data = is.null(channel) && subscribe
){
  
  if(!subscribe && is.null(subscriptions$account_updates)){
    usethis::ui_oops(
      paste0(
        "No ",
        crayon::bold("account updates"),
        " subscriptions exist to unsubscribe."
      )
    )
    return(invisible())
  }
  
  if(!subscribe){
    channel <- subscriptions$account_updates$client_id
  }
  
  sock   <- select_sock_for_api_fun()
  req_id <- fetch_and_bump("account_updates")
  
  if(subscribe){
    subscribe(
      fun_name = "account_updates",
      req_id   = req_id,
      req_name = acctCode,
      sock     = sock
    )
  } else {
    rm("account_updates", envir = subscriptions)
  }
  
  account_updates_msg <- ib_encode_raw_msg(
    c(
      functionary$outgoing_msg_codes$REQ_ACCT_DATA,
      functionary$function_versions_py$reqAccountUpdates,
      as.numeric(subscribe),
      acctCode
    )
  )
  
  writeBin(
    object = account_updates_msg,
    con    = sock,
    endian = "big"
  )
  
  account_updates <- if(subscribe){
    sock_seek(
      element_names   = c("ACCOUNTS", "PORTFOLIO_VALUE"),
      socket          = sock,
      success_element = simple_encode(
        functionary$incoming_msg_codes$ACCT_DOWNLOAD_END
      )
    ) %>% 
      purrr::compact()
  } else {
    return(invisible(TRUE))
  }
  
  if(any(names(account_updates) == "ACCOUNTS")){
    assign(
      "ACCOUNTS",
      value = structure(
        functionary$ib_update$ACCOUNTS(
          account_updates$ACCOUNTS
        ),
        last_updated = Sys.time()
      ),
      envir = get("treasury")
    )
  }
  
  account_updates <<- account_updates
  
  if(any(names(account_updates) == "PORTFOLIO_VALUE")){
    assign(
      "PORTFOLIO_VALUE",
      value = structure(
        functionary$ib_update$PORTFOLIO_VALUE(
          account_updates$PORTFOLIO_VALUE
        ),
        last_updated = Sys.time()
      ),
      envir = get("treasury")
    )
  }
  
  if(return_data){
    ib_validate(account_updates)
  } else {
    if(subscribe){
      invisible(
        is.list(account_updates) && (
          is.null(channel) || any(
            subscriptions$account_updates$req_name == acctCode
          )
        )
      )
    } else {
      invisible(is.null(account_updates))
    }
  }
  
}
