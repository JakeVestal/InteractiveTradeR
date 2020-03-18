#' Cancel Specified Orders
#'
#' Describe
#'
#' @param orders
#' List or char of orders
#'
#' @return
#' * **If SYNC  mode** (`channel` *not specified*)**:**
#' * **If ASYNC mode** (*specified* `channel`)**:**
#'
# #' @example inst/examples/cancel_order_ex.R
#' @family orders
#' @export
#'
cancel_order <- function(orders){

  # Keeps R CMD Check Happy
  permId <- clientId <- orderId <- NULL 
  
  if(is.vector(orders) || ncol(orders) == 1){
    orders <- req_all_open_orders() %>%
      dplyr::filter(permId %in% unlist(orders, use.names = FALSE))
  } else if(
    isTRUE(
      any(colnames(orders) == "clientId") &&
      any(colnames(orders) == "orderId")
    )
  ){
    invisible()
  } else if(isTRUE(any(colnames(orders) == "permId"))){
    orders <- req_all_open_orders() %>%
      dplyr::filter(permId %in% orders[,which(colnames(orders) == "permId")])
  } else {
    usethis::ui_oops(
      paste0(
        crayon::bold("orders"),
        " must be either a vector whose elements are permIds OR a ",
        "dataframe/matrix/tibble containing a column named permId OR two ",
        "columns named clientId and orderId."
      )
    )
    return(invisible())
  }

  orders %>%
    dplyr::group_by(clientId) %>%
    dplyr::select(clientId, orderId) %>%
    tidyr::nest(.key = "orderIds") %>%
    apply(
      MARGIN = 1,
      function(client_row){

        sock_client_ids <- vapply(
          mget(ls(sock_drawer), envir = sock_drawer),
          attr,
          numeric(1),
          which = "client_id"
        )
        sock_not_in_drawer <- !any(sock_client_ids == client_row$clientId)

        sock <- if(sock_not_in_drawer){
          ib_sync_connect(client_id = client_row$clientId)
        } else {
          select_sock_for_api_fun()
        }

        cancel_order_msg <- lapply(
          unlist(client_row$orderIds, use.names = FALSE),
          function(orderId){
            ib_encode_raw_msg(
              c(
                functionary$outgoing_msg_codes$
                  CANCEL_ORDER,
                functionary$function_versions_py$
                  cancelOrder,
                orderId
              )
            )
          }
        ) %>%
          unlist()


        writeBin(object = cancel_order_msg, con = sock, endian = "big")

        cancel_order_response <- sock_seek(
          "ORDER_STATUS", sock
        )$ORDER_STATUSES



        cancel_order_response

      }
    ) %>%
    purrr::reduce(dplyr::bind_rows)

}
