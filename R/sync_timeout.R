#' Set/Get Sync Timeout
#'
#' Set or retrieve the number of seconds that API calls in \strong{Sync Mode}
#' will wait for a response from the IB app before giving up. The default value
#' is set to 5 seconds.
#'
#' @param timeout
#' Numeric, length 1, specifying the time in seconds that API calls made in Sync
#' Mode will wait for a response from IB before giving up. The default value is
#' 5 seconds. \code{sync_timeout()} is called with argument \emph{timeout} is
#' not specified in the call, then \code{sync_timeout()} will return the current
#' timeout setting (in seconds) as a numeric.
#'
#'
#' @example inst/examples/sync_timeout_ex.R
#' @family utilities
#' @export
#'
sync_timeout <- function(timeout){
  if(!missing(timeout)){
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
    usethis::ui_done(
      paste0(
        crayon::bold("sync timeout"),
        "set to: ",
        timeout,
        " secs."
      )
    )
    return(invisible())
  }
  return(package_state$sync_time_out)
}
