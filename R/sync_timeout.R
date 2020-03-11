#' Set/Get Sync Timeout
#'
#' Set or retrieve the number of seconds that API calls in \strong{Sync Mode}
#' will wait for a response from the IB app before giving up. The default value
#' is set to 5 seconds.
#'
#' @param timeout
#' Numeric, length 1, specifying the time in seconds that API
#' calls made in SYNC mode will wait for a response from IB before giving up.
#' The default value is 5 seconds.
#'
#' If the argument \emph{timeout} is not specified in the call, then
#' sync_timeout() will return the current timeout setting.
#'
#' @export
#'
#' @example inst/examples/sync_timeout_ex.R
#'
sync_timeout <- function(timeout){
  if(missing(timeout)){return(package_state$sync_time_out)}
  if(!isTRUE(timeout > 0)){
    usethis::ui_oops(
      paste0(
        crayon::bold("timeout"),
        " must be a numeric value greater than 0."
      )
    )
    return(invisible())
  }
  assign(
    "sync_time_out",
    value = as.numeric(timeout),
    envir = package_state
  )
}
