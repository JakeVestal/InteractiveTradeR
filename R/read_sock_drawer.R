#' Read Sock Drawer
#'
#' Read the every sock that currently exists in the \link{sock_drawer}.
#'
#' @family utilities
#' @export
#'
read_sock_drawer <- function(){
  
  if(identical(ls(sock_drawer), character(0))){
    usethis::ui_oops("Empty sock drawer: No connections are currently open.")
    return(invisible(FALSE))
  }
  
  socks <- mget(ls(sock_drawer), envir = sock_drawer)
  
  ready_to_read <- which(socketSelect(socks, timeout = 0.1))
  
  if(identical(ready_to_read, integer(0))){
    usethis::ui_info("No socks are ready to read.")
    return(invisible(FALSE))
  }
  
  raw_response_env <- new.env(parent = emptyenv())
  index_name       <- 1
  
  while(!identical(ready_to_read, integer(0))){
    
    purrr::walk(
      ready_to_read,
      function(ready_sock){
        n <- ib_read_incoming_message_size_bytes(socks[[ready_sock]])
        if(isTRUE(n > 0)){
          assign(
            as.character(index_name),
            value = readBin(
              socks[[ready_sock]],
              what   = raw(),
              n      = n,
              endian = "little"
            ),
            envir = raw_response_env
          )
          index_name <<- 1 + index_name
        }
      }
    )
    
    ready_to_read <- which(socketSelect(socks, timeout = 0.1))
    
  }
  
  if(isTRUE(all.equal(raw_response_env, emptyenv()))){
    return(invisible(FALSE))
  }
  
  tryCatch(
    mget(
      as.character(sort(as.numeric(ls(raw_response_env)))),
      envir = raw_response_env
    ) %>%
      unique() %>%
      ib_decode() %>%
      ib_collate() %>%
      purrr::compact() %>% {
        if(length(.) == 0 || is.na(.)){
          return(invisible(FALSE))
        }
        ib_update(.)
      },
    error = function(e){e}
  ) %>% {
    if(inherits(., "error")){
      usethis::ui_oops(paste0(crayon::bold("read_sock_drawer: "), .$message))
      invisible(FALSE)
    } else {
      invisible(TRUE)
    }
  }
  
}
