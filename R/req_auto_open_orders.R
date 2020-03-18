#' Request Auto Open Orders
#'
#' @description
#' By default, orders placed within the TWS user interface are "unbound",
#' meaning that they have no order ID. \code{req_auto_open_orders()} toggles
#' this behavior, allowing the user to configure TWS to assign IDs to new orders
#' cretaed from within the TWS or not.
#'
#' \code{req_auto_open_orders()} \strong{cannot} be used on orders that are
#' already placed from the TWS. In other words, if you submit an order within
#' the TWS without having first called \code{req_auto_open_orders()}, then that
#' order is mostly inaccessable by anything other than TWS.
#'
#' Any API script or app that needs to access all orders placed -- whether
#' within the TWS, via the API, or any other source -- should call
#' \code{req_auto_open_orders()} during initiation (or, if a script, near the
#' beginning) so that orders created by users in the TWS have an order ID, and
#' can therefore be accessible to be modified by \link{place_order}() and
#' canceled with \link{cancel_order}().
#'
#' IB's documentation may be found on the
#' \href{https://interactivebrokers.github.io/tws-api/open_orders.html}{Open
#' Orders} page.
#'
#' @param bAutoBind
#' Logical, length 1, default TRUE. If TRUE, all subsequent orders created from
#' within TWS will have an order ID (IB refers to orders having an ID as being
#' "bound" in the documentation). If set to FALSE, future orders will not have
#' an order ID ("unbound").
#'
#' @details
#' \strong{TWS client required}: TWS will only bind newly opened orders so long
#' as the socket connected on \code{clientId = 0} continues to be open. Closing
#' the TWS socket is equivalent to calling \code{req_auto_open_orders(FALSE)}.
#'
#' \strong{\code{orderId =x= permId}}: Although by default, orders created in
#' TWS have no \emph{orderId}, they do have a \emph{permId}. \emph{permId} can
#' be used to identify an order, but cannot be used by the API to cancel or
#' modify it.
#'
#' @return
#' NULL, invisibly
#'
# #' @example inst/examples/req_auto_open_orders_ex.R
#' @family orders
#' @export
#'
req_auto_open_orders <- function(bAutoBind = TRUE){

  if(is.null(sock_drawer$tws)){
    usethis::ui_oops(
      paste0(
        "You need to open a TWS socket connection on by calling:\n",
        crayon::bold("create_new_connections(1, include_tws = TRUE)"),
        "\nbefore using ",
        crayon::bold("req_auto_open_orders"),
        "()."
      )
    )
    stop("No TWS connection")
  }

  writeBin(
    object = ib_encode_raw_msg(
      c(
        functionary$outgoing_msg_codes$REQ_AUTO_OPEN_ORDERS,
        functionary$function_versions_py$reqAutoOpenOrders,
        as.numeric(bAutoBind)
      )
    ),
    con    = sock_drawer$tws,
    endian = "big"
  )

}
