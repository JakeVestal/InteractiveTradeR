subscribe <- function(
  fun_name,
  req_name,
  req_id,
  req       = list(NULL),
  overwrite = TRUE,
  sock
){

  dplyr::bind_cols(
    tibble::tibble(
      req_name  = req_name
    ),
    purrr::map_dfc(
      purrr::compact(req),
      function(x){
        if(length(x) > 1){return(list(x))}
        x
      }
    ),
    tibble::tibble(
      req_id    = req_id,
      conn_row  = attr(sock, "conn_row"),
      client_id = attr(sock, "client_id")
    )
  ) %>% {
    if(any(subscriptions[[fun_name]]$req_name == req_name) && overwrite){
      subscriptions[[fun_name]][
        which(subscriptions[[fun_name]]$req_name == req_name),
        ] <- .
    } else {
      subscriptions[[fun_name]] <- dplyr::bind_rows(
        subscriptions[[fun_name]],
        .
      )
    }
  }
}

unsubscribe <- function(
  subscription_name,
  request,
  msg,
  id_or_name,
  channel = NULL
){
  
  subscriptions <- get("subscriptions")

  sock <- select_sock_for_api_fun()

  writeBin(object = msg, con = sock, endian = "big")

  read_sock_drawer()

  if(isTRUE(nrow(subscriptions[[subscription_name]]) == 1)){
    rm(list = subscription_name, envir = subscriptions)
  } else {
    assign(
      subscription_name,
      value = subscriptions[[subscription_name]][
        subscriptions[[subscription_name]][id_or_name] != request,
        ],
      envir = subscriptions
    )
  }

}
