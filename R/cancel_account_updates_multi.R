#' Cancel Account Updates Multi
#'
#' Stop receiving 3-minute updated Account Update Multi data for an account or
#' model for which a subscription has been started using
#' \link{req_account_updates_multi}().
#'
#' @param requests
#' Either a character or a numeric vector containing the names (`req_name`s) or
#' the IDs (`req_id`s), respectively, of the requests that you wish to cancel.
#' Valid `req_name`s and `req_id`s may be be viewed or retrieved using the
#' \link{subscriptions} environment.
#'
#' @details
#' If `requests` is not supplied, then `cancel_account_updates_multi`() will
#' cancel **all** Account Updates Multi subscriptions.
#'
#' @return
#' TRUE upon success (invisible), FALSE or error otherwise.
#'
#' @export
#'
#' @example inst/examples/account_updates_multi_ex.R
#'
cancel_account_updates_multi <- function(requests = NULL){
  cancellator(
    requests   = requests,
    cancel_msg = expression(
      ib_encode_raw_msg(
        c(
          functionary$outgoing_msg_codes$
            CANCEL_ACCOUNT_UPDATES_MULTI,
          functionary$function_versions_py$
            cancelAccountUpdatesMulti,
          req_id
        )
      )
    )
  )
}
