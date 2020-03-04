#' Read Sock Drawer
#'
#' Read the whole sock drawer (NULL) or certain socks.
#'
#' @export
#'
read_sock_drawer <- function(verbose = FALSE){

  if(identical(ls(sock_drawer), character(0))){
    if(verbose){
      usethis::ui_oops("Empty sock drawer: No connections are currently open.")
    }
    return(invisible(FALSE))
  }

  socks <- mget(ls(sock_drawer), envir = sock_drawer)

  ready_to_read <- which(socketSelect(socks, timeout = 0.1))

  if(identical(ready_to_read, integer(0))){
    if(verbose){
      usethis::ui_oops("No socks are ready to read.")
    }
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
    if(verbose){usethis::ui_done("Sock drawer read")}
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
          if(verbose){usethis::ui_done("Sock drawer read")}
          return(invisible(FALSE))
        }
        if(verbose){
          usethis::ui_done(
            paste0(
              "Updated the following treasury objects:\n\"",
              paste(crayon::bold(unique(names(.))), collapse = "\", \""),
              "\""
            )
          )
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
