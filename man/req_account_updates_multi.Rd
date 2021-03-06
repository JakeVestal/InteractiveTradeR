% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/req_account_updates_multi.R
\name{req_account_updates_multi}
\alias{req_account_updates_multi}
\title{Request Account Updates Multi}
\usage{
req_account_updates_multi(
  account = "All",
  modelCode = NULL,
  ledgerAndNLV = FALSE,
  channel = NULL,
  return_data = is.null(channel)
)
}
\arguments{
\item{account}{Character vector of length 1 containing a valid \strong{account code}.}

\item{modelCode}{Character vector of length 1 containing a valid \strong{model code}.}

\item{ledgerAndNLV}{Boolean of length 1, defaults to FALSE. If TRUE, then the created
subscriptions are treated as "lightweight requests", meaning that the data
returned for the account or model codes supplied will include only currency
positions (as opposed to both account values and currency positions).}

\item{channel}{One of the following:

\itemize{
  \item \strong{Not Specified} (Default): Opens a new connection to IB, uses
  it to issue the request and retrieve the response, and closes connection
  behind itself upon completion.
  \item \strong{The Name of a Sock}: Character vector, length 1. The name of
  an open, connected socket in the \link{sock_drawer}; e.g., "master", "tws",
  or "sock_123"
  \item \strong{Numeric Client ID}: Numeric, length 1. The client ID for
  which open orders are to be retrieved; e.g., 0, 874, 123. If a client ID is
  passed, and no socket in the \link{sock_drawer} is connected on that ID,
  then a new socket will be opened on that ID, and closed upon function exit.
  \item \strong{A \code{sockconn} Connection}: An open connection object of
  class "sockconn", connected to the IB API; e.g., \code{sock_drawer$tws}
}}

\item{return_data}{Boolean of length 1. Defaults to TRUE unless argument \emph{channel} is
specified. If FALSE, data retrieved by the function will be returned as the
funciton's output. If TRUE, then a Boolean succeses flag will be returned as
the function's output indicating the success (TRUE) or failure (FALSE) of the
function's attempt to transceive data to/from IB. Data in the
\strong{\code{treasury}} is always updated regardless of the value passed as
\emph{return_data} in the function call.}
}
\value{
This function is called for its side effect of updating the
\strong{\code{treasury}}, which takes place every time the function executes.
Additionally, the function's return value depends upon the value passed in as
\emph{return_data} as follows:

\itemize{
  \item \strong{If \code{return_data == FALSE}}: A Boolean success flag,
  returned invisibly, indicating that the function executed correctly and
  updated the \strong{\code{treasury}} with any new data retrieved.
  \item \strong{If \code{return_data == TRUE}}: Any new data retrieved will
  be returned in a \link[tibble]{tibble} in addition to being added to the
  \strong{\code{treasury}}. If no new data is available, returns NULL.
}

\emph{return_data} defaults to TRUE unless \emph{channel} is specified.
}
\description{
This function either fetches or sets up a subscription to receive the data
that appear in the
\href{https://www.interactivebrokers.com/en/software/tws/usersguidebook/realtimeactivitymonitoring/the_account_window.htm}{Account
Window} window of Trader Workstation. \code{req_account_updates_multi}() may
be used to set up multiple simultaneous subscriptions to many accounts /
model codes. IB's documentation may be found on the
\href{https://interactivebrokers.github.io/tws-api/account_updates.html}{Account
Updates} page.
}
\details{
\strong{"All" Option}: If \emph{account} is not specified,
\code{req_account_updates_multi}() will default to \code{account = "All"},
which will return data for all accounts accessible by the signed-in user.
This option is not available for users who manage more than 50 accounts.

\strong{Models}: Model Portfolios, or "Models" for short, can be though of as
named templates that will allocate funds across investment products in a
specified manner. When passed to \code{req_account_updates_multi}() as
\emph{model_code}, they behave like stocks -- they have postions, market
prices, values, etc. To use Model Portfolios you need to have a Financial
Advisor account, and the Model Portfolios feature needs to be activated.
Learn more on
\href{https://www.interactivebrokers.com/en/index.php?f=20917}{Interactive
Brokers' Model Portfolios page}.

\strong{No Positions}: If you call \code{req_account_updates_multi}() on an
account that has cash but no positions, then data will be sent to the socket
once initially, but will not update thereafter.
}
\section{ACCOUNTS Treasury Object}{

\code{req_account_summary}() updates the \strong{ACCOUNTS} object in the
treasury. ACCOUNTS is a \link{tibble} in which each row represents a
parameter pertaining to a particular account. Has the following columns:
\itemize{
  \item \strong{account} <chr>: Account ID (e.g., "DF1234567")
  \item \strong{tag}  <chr>: Name of account tag (e.g., "BuyingPower")
  \item \strong{tag_value} <chr>: Value of tag (e.g., "500000")
  \item \strong{currency} <chr>: 3-letter currency abbreviation if
    \code{tag_value} is a monetary amount, "" otherwise.
}
}

\examples{
################################################################################
#### Effect of ledgerAndNLV ####################################################
################################################################################

# Let's look at the difference between calling req_account_updates_multi with
#  ledgerAndNLV = TRUE vs. setting it to FALSE.

#1) Fetch an update using req_account_updates_multi()'s default values for 
#   for account and ledgerAndNLV ("ALL" and TRUE, respectively).
req_account_updates_multi()

# The update printed to screen but it's not lost -- it's in the treasury. 

# 2) Save the treasury object in a variable to use for comparing later:
aum_true <- treasury$ACCOUNTS

# 3) Clear out the treasury. Without this step, InteractiveTradeR would update
# the new ACCOUNTS object when we call req_account_updates_multi() again with
# ledgerAndNLV = FALSE, making it hard to tell exactly what effect our changing
# of the value of ledgerAndNLV had.
clean_slate()

# Call req_account_updates_multi() again, but with ledgerAndNLV = FALSE:
req_account_updates_multi(ledgerAndNLV = FALSE)
aum_false <- treasury$ACCOUNTS

# Compare the two:
#   3.1) Same account:
unique(aum_true$account)
unique(aum_false$account)
#   3.2) See that ledger_and_NLV = FALSE includes more parameters:
setdiff(unique(aum_false$tag), unique(aum_true$tag))

################################################################################
#### Subscriptions #############################################################
################################################################################

# Clean slate
clean_slate()

# Open a socket
create_new_connections()

# Fetch the account IDs of your six paper trading accounts and use walk() from
# the purrr package to subscribe to each one
req_managed_accts() \%>\%
  purrr::walk(
    req_account_updates_multi,
    channel = "async"
  )

# Verify that you're now subscribed to the six paper trading accounts:
subscriptions$account_updates_multi

# Access the retrieved updates:
treasury$ACCOUNTS

# You should have all six paper account codes represented in the "account"
# column of the ACCOUNTS treasury object.

# This information will update every 3 minutes -- and probably more frequently
# than that in practice -- for those accounts that have positions in financial
# instruments. You can wait for at least one cycle and call read_sock_drawer()
# again to see this for yourself.

# Save the treasury object to use for comparing later
before_cancel <- treasury$ACCOUNTS

# When you're ready, cancel a subscription or two: how about the 1st and 3rd?
cancel_accounts <- subscriptions$account_updates_multi$req_name[c(1,3)]
cancel_account_updates_multi(cancel_accounts)

# Check that the two accounts are indeed removed from subscriptions:
subscriptions$account_updates_multi
any(cancel_accounts \%in\% subscriptions$account_updates_multi$req_name)

# From this point on, the sock drawer will no longer get updated data for the
# two accounts that were unsubscribed.

# To convince yourself, first read off any data that might have gotten sent
# in the time between the last read and the call to cancel:
read_sock_drawer()

# From this point on, the canceled accounts' treasury data -- which can be
# selected using the following code:
treasury$ACCOUNTS \%>\%
  dplyr::filter(account \%in\% cancel_accounts)
# -- will not update, no matter how many times you call read_sock_drawer(),
# unless you subscribe to them again.

################################################################################
#### CANCELLING Subscriptions ##################################################
################################################################################

# Clear out the treasury & subscriptions for this example
clean_slate(c("treasury", "subscriptions"))

# Open a socket
create_new_connections()

# Fetch the account IDs of your six paper trading accounts and use walk() from
# the purrr package to subscribe to each one
req_managed_accts() \%>\%
  purrr::walk(
    req_account_updates_multi,
    channel = "async"
  )

# Verify that you're now subscribed to the six paper trading accounts:
subscriptions$account_updates_multi

# Access the retrieved updates:
treasury$ACCOUNTS

# You should have all six paper account codes represented in the "account"
# column of the ACCOUNTS treasury object.

# This information will become available every 3 minutes -- and probably more
# frequently than that in practice -- for those accounts that have positions in
# financial instruments. 

# You can wait for at least one cycle and see this for yourself; just call
# read_sock_drawer() as many times as you'd like to refresh the data.

# Save the treasury object to use for comparing later
before_cancel <- treasury$ACCOUNTS

# When you're ready, cancel a subscription or two: how about the 1st and 3rd?
cancel_accounts <- subscriptions$account_updates_multi$req_name[c(1,3)]
cancel_account_updates_multi(cancel_accounts)

# Check that the two accounts are indeed removed from subscriptions:
subscriptions$account_updates_multi
any(cancel_accounts \%in\% subscriptions$account_updates_multi$req_name)

# From this point on, the sock drawer will no longer get updated data for the
# two accounts that were unsubscribed.

# To convince yourself, first read off any data that might have gotten sent
# in the time between the last read and the call to cancel:
read_sock_drawer()

# From this point on, the canceled accounts' treasury data -- which can be
# selected using the following code:
treasury$ACCOUNTS \%>\%
  dplyr::filter(account \%in\% cancel_accounts)
# -- will not update, no matter how many times you call read_sock_drawer(),
# unless you subscribe to them again.
}
\seealso{
Other treasury: 
\code{\link{cancel_account_summary}()},
\code{\link{cancel_account_updates_multi}()},
\code{\link{req_account_summary}()},
\code{\link{req_account_updates}()}
}
\concept{treasury}
