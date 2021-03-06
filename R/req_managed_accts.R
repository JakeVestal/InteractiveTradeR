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
#' @example inst/examples/req_managed_accts_ex.R
#' @family quick fetch
#' @export
#' 
req_managed_accts <- function(channel = NULL){

  sock <- select_sock_for_api_fun()

  writeBin(
    object = functionary$fixed_api_msgs$req_managed_accts,
    con    = sock,
    endian = "big"
  )

  managed_accts <- sock_seek(
    element_names   = "MANAGED_ACCTS",
    socket          = sock,
    success_element = simple_encode(
      functionary$incoming_msg_codes$MANAGED_ACCTS
    )
  )

  ib_validate(managed_accts)

}
