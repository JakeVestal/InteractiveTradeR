#' Request Completed Orders
#'
#' @description
#' Fetch the orders completed (if any) in the current trading day.
#'
#' @param apiOnly
#' Logical, length 1, default FALSE. If FALSE, result will include completed
#' orders submitted via the TWS; if TRUE, then only orders submitted through the
#' API will be included.
#'
#' @inheritParams req_current_time
#'
#' @return
#' * **If SYNC  mode** (`channel` *not specified*)**:**
#' * **If ASYNC mode** (*specified* `channel`)**:**
#'
# #' @example inst/examples/req_completed_orders_ex.R
#' @family orders
#'
req_completed_orders <- function(apiOnly = FALSE, channel = NULL){

  sock <- select_sock_for_api_fun()

  req_completed_orders_msg <- ib_encode_raw_msg(
    c(
      functionary$outgoing_msg_codes$REQ_COMPLETED_ORDERS,
      as.numeric(apiOnly)
    )
  )

  writeBin(object = req_completed_orders_msg, con = sock, endian = "big")

  completed_orders <- sock_seek(
    element_names   = "COMPLETED_ORDERS",
    socket          = sock,
    success_element = simple_encode(
      functionary$incoming_msg_codes$COMPLETED_ORDERS_END
    )
  )

  if(is.null(completed_orders)){
    usethis::ui_info("No completed orders.")
    return(invisible())
  }

  completed_orders

}
