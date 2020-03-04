#' @keywords internal
sock_seek <-function(
  element_names,
  socket,
  success_element,
  stop_early
){

  if(!missing(stop_early) && inherits(stop_early, "raw")){
    stop_early <- list(stop_early)
  }
  raw_response_env <- new.env(parent = emptyenv())
  index_name       <- 0
  start_time       <- as.numeric(Sys.time())

  while(TRUE){

    if(as.numeric(Sys.time()) - start_time > package_state$sync_time_out){
      mget(
        as.character(sort(as.numeric(ls(raw_response_env)))),
        envir = raw_response_env
      ) %>%
        ib_decode()
      usethis::ui_oops(paste0(rlang::call_name(sys.call(-1)), " TIMEOUT"))
      break()
    }

    if(
      !missing(success_element) && identical(
        get0(as.character(index_name), envir = raw_response_env)[
          1:length(success_element)
          ],
        success_element
      )
    ){break()}

    if(
      !missing(stop_early) && any(
        vapply(
          stop_early,
          function(early_stop_msg){
            identical(
              raw_response_env[[as.character(index_name)]][
                1:length(early_stop_msg)
                ],
              early_stop_msg
            )
          },
          logical(1)
        )
      )
    ){
      usethis::ui_done("Stop Early Element reached")
      break()
    }
    n <- if(socketSelect(list(socket), timeout = 0.01)){
      ib_read_incoming_message_size_bytes(socket)
    }

    if(is.null(n)){next()}

    index_name <- index_name + 1

    assign(
      as.character(index_name),
      value = readBin(
        socket,
        what   = raw(),
        n      = n,
        endian = "little"
      ),
      envir = raw_response_env
    )

  }

  mget(
    as.character(sort(as.numeric(ls(raw_response_env)))),
    envir = raw_response_env
  ) %>%
    ib_decode() %>%
    ib_collate() %>%
    ib_update(rtn_elements = element_names)

}
