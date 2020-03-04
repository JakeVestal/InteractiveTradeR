#' Close and Remove an IB Connection
#'
#' The inverse of \link{create_new_connections}(). Will close and remove IB
#' socket connections as directed.
#'
#' @param number_to_disconnect
#' Integer. Total number of sockets to disconnect, including Master and TWS
#' connections if included.
#'
#' @param disconnect_master
#' Boolean, default FALSE. If TRUE, then the \link{master_client_id}{Master
#' Client Socket} will be disconnected if it exists.
#'
#' @param disconnect_tws
#' Boolean, default FALSE. If TRUE, then the #' \link{client_id_0}{TWS Socket}
#' will be disconnected if it exists.
#'
#' @seealso create_new_connections
#'
disconnect <- function(
  number_to_disconnect = "all",
  disconnect_master    = FALSE,
  disconnect_tws       = FALSE
){

  length_1 <- length(ls(sock_drawer))

  if(identical(number_to_disconnect, "all")){

    clean_slate(c("sock_drawer", "subscriptions"))

    if(length_1 == 0){

      usethis::ui_info(
        crayon::bold("Sock drawer currently contains 0 socks.")
      )

    } else {

      cat(crayon::bold("Sock drawer cleaned out!\n"))
      usethis::ui_done(
        paste0(
          "Disconnected ",
          length(length_1),
          " socket",
          if(length_1 == 1){
            "."
          } else {
            "s."
          }
        )
      )

    }

    return(invisible())

  }

  if(number_to_disconnect > length_1){
    usethis::ui_oops(
      paste0(
        "You've tried to disconnect ",
        number_to_disconnect,
        " sockets but only ",
        length_1, " sockets are currently open."
      )
    )
    return(invisible())
  }

  if(disconnect_master){
    if(any(ls(sock_drawer) == "master")){

      close(sock_drawer$master)

      rm(list = "master", envir = sock_drawer)

      usethis::ui_done("Master socket disconnected.")

    } else {

      usethis::ui_oops("No Master socket is currently open.")

    }

    number_to_disconnect <- number_to_disconnect - 1

  }

  if(number_to_disconnect == 0){return(invisible())}

  if(disconnect_tws){
    if(any(ls(sock_drawer) == "tws")){

      close(sock_drawer$tws)

      rm(list = "tws", envir = sock_drawer)

      usethis::ui_done("TWS socket (Client ID = 0) disconnected.")

    } else {

      usethis::ui_oops("No TWS socket is currently open.")

    }

    number_to_disconnect <- number_to_disconnect - 1

  }

  if(number_to_disconnect == 0){return(invisible())}

  socks_to_disconnect <- grep(
    "sock_",
    ls(sock_drawer),
    value = TRUE
  ) %>%
    gsub("sock_", "", .) %>%
    as.numeric() %>%
    sort() %>%
    utils::tail(number_to_disconnect) %>%
    paste0("sock_", .) %>%
    mget(sock_drawer)

  socks_to_disconnect %>% purrr::walk(close)

  rm(list = names(socks_to_disconnect), envir = sock_drawer)

  conn_rows_left <- vapply(
    ls(sock_drawer),
    function(sock){
      attr(sock_drawer[[sock]], "conn_row")
    },
    numeric(1)
  )

  for(subscription in ls(subscriptions)){
    assign(
      subscription,
      value = subscriptions[[subscription]][
        !(subscriptions[[subscription]]$conn_row %in% conn_rows_left),
        ],
      envir = subscriptions
    )
  }

  usethis::ui_done(
    paste0(
      "Disconnected ",
      length_1 - length(ls(sock_drawer)),
      " sockets."
    )
  )

}
