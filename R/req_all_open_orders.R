#' Request All Open Orders
#'
#' Update the \link{treasury}'s \strong{ORDER_STATUSES} and \strong{OPEN_ORDERS}
#' objects and, optionally, receive list of two \link[tibble]{tibble}s
#' containing the updated ORDER_STATUS and OPEN_ORDERS objects.
#' \code{req_all_open_orders}() retreives information about every open order
#' accessible by your username, regardless of the order's client ID (socket) or
#' whether it was placed via the API or within the Trader Workstation (TWS).
#'
#' @inheritParams req_account_updates_multi
#'
#' @section OPEN_ORDERS and ORDER_STATUSES Treasury Objects:
#' This function updates the following \link{treasury} objects:
#'
#' \strong{OPEN_ORDERS} A \link[tibble]{tibble} in which each row represents an
#' order that, at the time it was last updated, had not fully filled. The column
#' names are described in the
#' \href{https://interactivebrokers.github.io/tws-api/classIBApi_1_1Contract.html}{Contract},
#' \href{https://interactivebrokers.github.io/tws-api/classIBApi_1_1Order.html}{Order},
#' and
#' \href{https://interactivebrokers.github.io/tws-api/classIBApi_1_1OrderState.html}{OrderState}
#' sections of IB's documentation.
#'
#' \strong{ORDER_STATUSES} A \link[tibble]{tibble} in which each row represents
#' an order that, at the time it was last updated, had not fully filled. The
#' column names are described in the
#' \href{https://interactivebrokers.github.io/tws-api/interfaceIBApi_1_1EWrapper.html#a17f2a02d6449710b6394d0266a353313}{Order
#' Status} section of IB's documentation.
#'
#' @inherit req_account_summary return
#'
# #' @example inst/examples/req_all_open_orders_ex.R
#' @family orders
#' @export
#'
req_all_open_orders <- function(channel = NULL, return_data = is.null(channel)){

  sock <- select_sock_for_api_fun()

  writeBin(
    object = functionary$fixed_api_msgs$req_all_open_orders,
    con    = sock,
    endian = "big"
  )

  all_open_orders <- sock_seek(
    element_names   = c("OPEN_ORDERS", "ORDER_STATUSES"),
    socket          = sock,
    success_element = simple_encode(
      functionary$incoming_msg_codes$OPEN_ORDER_END
    )
  )

  if(is.null(unlist(all_open_orders, use.names = FALSE))){
    all_open_orders <- list()
    all_open_orders$OPEN_ORDERS <- list()
    all_open_orders$ORDER_STATUSES <- list()
  }

  assign(
    "OPEN_ORDERS",
    structure(
      all_open_orders$OPEN_ORDERS,
      last_updated = Sys.time()
    ),
    envir = get("treasury")
  )

  assign(
    "ORDER_STATUSES",
    structure(
      all_open_orders$ORDER_STATUSES,
      last_updated = Sys.time()
    ),
    envir =  get("treasury")
  )

  if(return_data){
    vapply(all_open_orders, identical, logical(1), y = list()) %>% {
      if(any(.)){
        purrr::walk(
          names(.[which(.)]),
          function(empty_obj){
            usethis::ui_info(
              paste0("No ", gsub("_", " ", tolower(empty_obj)), ".")
            )
          }
        )
      }
    }
    if(is.null(unlist(all_open_orders, use.names = FALSE))){
      invisible(all_open_orders)
    } else {
      all_open_orders
    }
  } else {
    
    treasury <- get("treasury")
    
    invisible(
      all.equal(
        all_open_orders$OPEN_ORDERS,
        treasury$OPEN_ORDERS,
        check.attributes = FALSE
      ) &&
        all.equal(
          all_open_orders$ORDER_STATUSES,
          treasury$ORDER_STATUSES,
          check.attributes = FALSE
        )
    )
  }

}
