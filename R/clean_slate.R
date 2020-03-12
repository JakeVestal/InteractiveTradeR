#' Clear out the subscription environments and the sock drawer
#' 
#' This function wipes out data stored in your environments -- use with care!
#' 
#' @param what
#' What would you like to clear out? Character vector containing any or all of
#' "archives", "sock_drawer", "subscriptions", "treasury". If \emph{what} is
#' left blank, the default behavior of \code{clean_slate} is to clear out
#' each of the listed options.
#' 
#' @family utilities
#' @export
#' 
clean_slate <- function(what){
  
  choices <- c(
    "archives", "sock_drawer", "subscriptions", "treasury", "mkt_data"
  )
  
  if(missing(what)){
    what <- choices
  } else {
    what <- match.arg(what, choices = choices, several.ok = TRUE)
  }
  
  if(any(what == "sock_drawer")){
    purrr::walk(mget(ls(sock_drawer), sock_drawer), close)
  }
  
  purrr::walk(
    what,
    function(slate){
      eval(parse(text = slate)) %>%
        rm(list = ls(.), envir = .)
    }
  )
  
  invisible()
  
}
