#' Socks
#'
#' Describe
#'
#'
socks <- function(){
  if(length(ls(sock_drawer)) > 0){
    return(
      mget(
        ls(sock_drawer),
        envir = sock_drawer
      ) %>%
        structure(., class = c("sock_drawer", class(.)))
    )
  }
  invisible()
}
