% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/req_family_codes.R
\name{req_family_codes}
\alias{req_family_codes}
\title{Request Family Codes}
\usage{
req_family_codes(channel = NULL)
}
\arguments{
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
}
\value{
A named character vector containing all of the family codes for the accounts
that are visible to the user. The value of each element is a family code, and
the name of each element is the account ID to which that code applies.
}
\description{
Retrieve the family codes for all accounts accessible by the current user.
}
\section{Family Codes}{

Every account at Interactive Brokers is associated with one \strong{family
code} in order to cluster accounts into groups according to management. By
default, the family code for an account equals that account's ID suffixed
with the letter "A"; for example, a an individual account having an ID =
"U1234567" would have a Family Code of "U1234567A".

If, however, Account U1234567 were managed by a Financial Advisor running an
account under ID = "U7654321", then the Family Code of account U1234567 would
be "U7654321A".
}

\examples{
# Fetch family codes
family_codes <- req_family_codes()
# Print family codes
family_codes
}
\seealso{
Other quick fetch: 
\code{\link{req_current_time}()},
\code{\link{req_managed_accts}()}
}
\concept{quick fetch}
