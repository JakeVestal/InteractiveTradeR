#' Request Current Time
#'
#' @description
#' Fetch the current time from IB's servers.
#'
#' @details
#' IB servers use the UTC timezone, but \code{req_current_time}() returns the
#' time in terms of whatever time zone is set in your current session of R. To
#' check your timezone, run the basic R command
#' \code{\link[base]{Sys.timezone}}().
#'
#' @param channel
#' One of the following:
#' 
#' \itemize{
#'   \item \strong{Not Specified} (Default): Opens a new connection to IB, uses
#'   it to issue the request and retrieve the response, and closes connection
#'   behind itself upon completion.
#'   \item \strong{The Name of a Sock}: Character vector, length 1. The name of
#'   an open, connected socket in the \link{sock_drawer}; e.g., "master", "tws",
#'   or "sock_123"
#'   \item \strong{Numeric Client ID}: Numeric, length 1. The client ID for
#'   which open orders are to be retrieved; e.g., 0, 874, 123. If a client ID is
#'   passed, and no socket in the \link{sock_drawer} is connected on that ID,
#'   then a new socket will be opened on that ID, and closed upon function exit.
#'   \item \strong{A \code{sockconn} Connection}: An open connection object of
#'   class "sockconn", connected to the IB API; e.g., \code{sock_drawer$tws}
#' }
#'
#' @return
#' A POSIXct date-time object. See \link[base]{DateTimeClasses} for details.
#'
#' @export
#'
#' @example inst/examples/req_current_time_ex.R
#'
req_current_time <- function(channel = NULL){
  
  sock <- select_sock_for_api_fun()
  
  writeBin(
    object = functionary$fixed_api_msgs$req_current_time,
    con    = sock,
    endian = "big"
  )
  
  current_time <- sock_seek(
    element_names   = "CURRENT_TIME",
    socket          = sock,
    success_element = simple_encode(
      functionary$incoming_msg_codes$CURRENT_TIME
    )
  )
  
  ib_validate(current_time)
  
}
