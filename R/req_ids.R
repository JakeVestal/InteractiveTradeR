#' Request IDs
#'
#' @description
#' Fetch the next valid order ID available for the socket specified by
#' \emph{channel}. In InteractiveTradeR, the need for \code{req_ids}() is
#' largely abstracted away and it is unlikely that the user will need this
#' function. However, it is used by other functions in the package and is
#' included here for completeness.
#'
#' @inheritParams req_current_time
#'
#' @details
#' More information can be found in IB's documentation here:
#' \href{https://interactivebrokers.github.io/tws-api/order_submission.html}{Next
#' Valid Identifier}
#'
#' Unlike orders placed using the TWS graphical user interface -- which all use
#' \code{orderId = 0} -- every order placed by an API \emph{on a particular
#' socket} must have a unique integer order identifier. This identifier is the
#' "ID" meant by the function's name "Request IDs".
#'
#' Keeping track of order IDs is necessary because in practice, any number of
#' different users may need to submit orders -- each requiring a unique order ID
#' -- to a given account or account structure. Although each user may keep track
#' of order IDs locally, there is no way to be sure that some other user hasn't
#' already used the ID without querying the IB servers.
#'
#' \code{req_ids}() should, perhaps, really be named "\code{req_id}", without
#' the "s", because it only returns the emph{next valid id} -- a single integer.
#' The plural form of the function's name is due to historical reasons arising
#' from older versions of the IB API system.
#'
#' The function \code{req_ids()} fulfills this role by querying IB and returning
#' the \strong{next valid ID}: the lowest-valued integer that hasn't yet been
#' used as an order ID by any user on a particular client, and therefore may be
#' utilized as the ID for a new order  to be placed on that socket.
#'
#' @return
#' Integer. The next valid order ID available on the specified socket.
#'
#' @example inst/examples/req_ids_ex.R
#' @family orders
#' @export
#'
req_ids <- function(channel = NULL){

  sock <- select_sock_for_api_fun()

  writeBin(
    object = functionary$fixed_api_msgs$req_ids,
    con    = sock,
    endian = "big"
  )

  ids <- sock_seek(
    element_names   = "NEXT_VALID_ID",
    socket          = sock,
    success_element = simple_encode(
      functionary$incoming_msg_codes$NEXT_VALID_ID
    )
  )

  ids

}
