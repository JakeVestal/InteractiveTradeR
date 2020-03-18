#' Request and Bind Open Orders Placed from Within TWS Itself
#'
#' @description
#' This function as implemented in InteractiveTradeR differs slightly from the
#' default *reqOpenOrders*() function implemented in the standard API in that
#' `req_open_orders`() takes no arguments as it may only be invoked for
#' \link[=client_id_0]{Client ID 0}, the TWS GUI client ID.
#'
#' For requesting open orders on other sockets, use \link{req_all_open_orders}
#' to fetch all orders, no matter what socket was used to open them, and then
#' use \href{http://adv-r.had.co.nz/Subsetting.html}{subsetting} to filter the
#' output \link[tibble]{tibble} by client ID to select the orders you're
#' interested in.
#'
#' The only reason to use `req_open_orders`() in InteractiveTradeR is due to an
#' additional special property of `req_open_orders`(): **when called,
#' `req_open_orders`() causes any and all open orders that were created within
#' the TWS itself to become *bound***.
#'
#' **Definition of "Bound"**:
#'
#' In IB jargon, an order is "*bound*" if it is assigned an order ID. By
#' default, orders placed within the TWS do not have order IDs (in other words,
#' they're *unbound*). Because the API requires order IDs to modify or cancel an
#' order, a call to `req_open_orders`() is necessary to enable the API to cancel
#' or modify orders placed within TWS.
#'
#' Relevant IB documentation may be found on the
#' \href{https://interactivebrokers.github.io/tws-api/open_orders.html}{Open
#' Orders} page.
#'
#' @section Use Case:
#' The default behavior of the TWS is to *not* assign order IDs to orders opened
#' within the app itself. That being the case, consider the situation in which
#' you or a colleague has opened orders from within the TWI app. Because the
#' open orders have no IDs, you have no way to identify them in the API, and are
#' therefore unable to cancel or modify them using the API.
#'
#' By calling `req_open_orders`(), all open orders that were opened within the
#' TWS app are returned in a \link[tibble]{tibble} as the function's output.
#' Additionally, **these orders are given an order ID so that they can now be
#' modified/canceled within the API**.
#'
#' @section Use `req_auto_open_orders`() to Assign IDs to Future TWS Orders:
#' By calling \link{req_auto_open_orders}, the behavior of the TWS app will be
#' reconfigured to bind all orders created within the app, but will not affect
#' open orders that were opened within the app before `req_auto_open_orders`()
#' was called.
#'
#' @section Best Practice:
#' Make a call to `req_open_orders`() and `req_auto_open_orders`() during the
#' initialization section of any Shiny app (or script) created with
#' InteractiveTradeR. This will ensure that:
#'
#'  1) Open orders that were placed within the TWS app before the
#'  InteractiveTradeR app was started are assigned an order ID by
#'  `req_open_orders`() so that they are accessible within the API
#'
#'  2) Future orders placed within the TWS app while the InteractiveTradeR app
#'  is running are also accessible to the InteractiveTradeR app because the call
#'  to `req_auto_open_orders`() reconfigured the TWS to assign them order IDs by
#'  default.
#'
#' With this practice, any InteractiveTradeR app only ever needs to call
#' `req_open_orders`() once during initialization.
#'
#' @inheritSection req_all_open_orders OPEN_ORDERS and ORDER_STATUSES Treasury Objects
#'
#' @inherit req_account_summary return
#'
#' @seealso req_auto_open_orders
#'
# #' @example inst/examples/req_open_orders_ex.R
#' @family orders
#'
req_open_orders <- function(){

  channel <- 0
  sock    <- select_sock_for_api_fun()

  writeBin(
    object = functionary$fixed_api_msgs$req_open_orders,
    con    = sock,
    endian = "big"
  )

  open_orders <- sock_seek(
    element_names   = c("OPEN_ORDERS", "ORDER_STATUSES"),
    socket          = sock,
    success_element = simple_encode(
      functionary$incoming_msg_codes$OPEN_ORDER_END
    )
  ) %>%
    purrr::compact()

  if(is.null(unlist(open_orders, use.names = FALSE))){
    open_orders <- list()
    open_orders$OPEN_ORDERS <- list()
    open_orders$ORDER_STATUSES <- list()
  }

  if(isTRUE(any(names(open_orders) == "OPEN_ORDERS"))){
    assign(
      "OPEN_ORDERS",
      functionary$ib_update$OPEN_ORDERS(
        open_orders$OPEN_ORDERS
      ),
      envir = get("treasury")
    )
  }

  if(isTRUE(any(names(open_orders) == "ORDER_STATUSES"))){
    assign(
      "ORDER_STATUSES",
      functionary$ib_update$ORDER_STATUSES(
        open_orders$ORDER_STATUSES
      ),
      envir = get("treasury")
    )
  }

  if(is.null(unlist(open_orders, use.names = FALSE))){
    invisible(open_orders)
  } else {
    open_orders
  }

}
