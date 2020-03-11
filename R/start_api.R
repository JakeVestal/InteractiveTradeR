#' Start API (not explicit in InteractiveTradeR)
#'
#' Functions named "Start API" occur in the official IB APIs but in
#' InteractiveTradeR, this functionality is implimented implicitly by other
#' functions such as \link{create_new_connections}(). Therefore, there is no
#' need to ever call start_api() in InteractiveTradeR -- doing so will produce
#' an informative error message.
#'
#' @example inst/examples/start_api_ex.R
#' @family utilities
#' @export
#'
start_api <- function(){
  usethis::ui_info(
    paste0(
      "In ",
      crayon::bold("InteractiveTradeR"),
      ", the API function ",
      crayon::bold("start_api"),
      "() is implemented\nimplicitly by other functions such as ",
      crayon::bold("create_new_connections"),
      ";\ntherefore, there is no need to ever call ",
      crayon::bold("start_api"),
      "() directly."
    )
  )
}
