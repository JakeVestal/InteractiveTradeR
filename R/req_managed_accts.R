#' Request Managed Accounts
#'
#' Retrieve the account ID codes for all IB accounts your username can access.
#'
#' @inheritParams req_current_time
#'
#' @return
#' character vector in which each element is the ID of an account accessible by
#' the currently logged-in user.
#'
#' @export
#'
#' @example inst/examples/req_managed_accts_ex.R
#'
req_managed_accts <- function(channel = NULL){

  sock <- select_sock_for_api_fun()

  writeBin(
    object = InteractiveTradeR::functionary$fixed_api_msgs$req_managed_accts,
    con    = sock,
    endian = "big"
  )

  managed_accts <- sock_seek(
    element_names   = "MANAGED_ACCTS",
    socket          = sock,
    success_element = simple_encode(
      InteractiveTradeR::functionary$incoming_msg_codes$MANAGED_ACCTS
    )
  )

  managed_accts

}
