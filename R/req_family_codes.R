#' Request Family Codes
#'
#' Retrieve the family codes for all accounts accessible by the current user.
#'
#' @inheritParams req_current_time
#'
#' @return
#' A named character vector containing all of the family codes for the accounts
#' that are visible to the user. The value of each element is a family code, and
#' the name of each element is the account ID to which that code applies.
#'
#' @section Family Codes:
#' Every account at Interactive Brokers is associated with one \strong{family
#' code} in order to cluster accounts into groups according to management. By
#' default, the family code for an account equals that account's ID suffixed
#' with the letter "A"; for example, a an individual account having an ID =
#' "U1234567" would have a Family Code of "U1234567A".
#'
#' If, however, Account U1234567 were managed by a Financial Advisor running an
#' account under ID = "U7654321", then the Family Code of account U1234567 would
#' be "U7654321A".
#'
#' @example inst/examples/req_family_codes_ex.R
#' @family quick fetch
#' @export
#'
req_family_codes <- function(channel = NULL){

  sock <- select_sock_for_api_fun()

  writeBin(
    object = functionary$fixed_api_msgs$req_family_codes,
    con    = sock,
    endian = "big"
  )

  family_codes <- sock_seek(
    element_names   = "FAMILY_CODES",
    socket          = sock,
    success_element = simple_encode(
      functionary$incoming_msg_codes$FAMILY_CODES
    )
  )

  ib_validate(family_codes)

}
