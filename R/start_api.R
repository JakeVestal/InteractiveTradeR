#' Start API
#'
#' In InteractiveTradeR, the API function `start_api`() is implemented
#' implicitly by other functions such as \link{create_new_connections}();
#' therefore, there is no need to ever call `start_api`() directly.
#'
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
