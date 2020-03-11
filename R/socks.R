#' Socks
#'
#' Print a detailed description of all the socks that are currently open.
#'
#' @family utilities
#' @export
#'
socks <- function(){
  if(length(ls(sock_drawer)) > 0){
    mget(
      ls(sock_drawer),
      envir = sock_drawer
    ) %>%
      purrr::iwalk(print_async)
  }
  invisible()
}
