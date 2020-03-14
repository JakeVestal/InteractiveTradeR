select_sock_for_api_fun <- function(){
  
  channel <- get0("channel", envir = parent.frame())
  
  tryCatch(
    {
      if(inherits(channel, "sockconn")){return(channel)}
      if(isTRUE(is.na(suppressWarnings(as.numeric(channel))))){
        sock <- switch(
          channel,
          if(is.null(sock_drawer[[channel]])){
            usethis::ui_oops("Please specify a valid socket!")
            usethis::ui_info(
              paste0(
                "Valid choices are:\n\t\"",
                paste(ls(sock_drawer), collapse = "\", \""),
                "\""
              )
            )
            invisible()
          } else {
            sock_drawer[[channel]]
          },
          async = tryCatch(
            mget(
              setdiff(ls(sock_drawer), c("master", "tws")),
              envir = sock_drawer
            ) %>% {
              sample(
                .[which(socketSelect(., write = TRUE, timeout = 0.01))],
                size = 1
              )[[1]]
            },
            error = function(e){
              read_sock_drawer()
              tryCatch(
                mget(
                  setdiff(ls(sock_drawer), c("master", "tws")),
                  envir = sock_drawer
                ) %>% {
                  sample(
                    .[[which(socketSelect(., write = TRUE, timeout = 0.1))]],
                    size = 1
                  )[[1]]
                },
                error = function(ee){
                  usethis::ui_oops("No async sockets available for writing.")
                  stop(ee$message)
                }
              )
            }
          ),
          master = if(is.null(sock_drawer$master)){
            usethis::ui_oops(
              paste0(
                "No socket connection is currently open on the ",
                crayon::bold("master"),
                " client id."
              )
            )
            usethis::ui_info(
              paste0(
                "You may call ",
                crayon::bold(
                  "create_new_connections(1, include_master = TRUE)"
                ),
                " to open a master socket."
              )
            )
            usethis::ui_info(
              paste0(
                "For more info, see the helpfile accessible at: ",
                crayon::bold("?create_new_connections")
              )
            )
            invisible()
          } else {
            sock_drawer$master
          },
          tws    = if(is.null(sock_drawer$tws)){
            usethis::ui_oops(
              paste0(
                "No socket connection is currently open on the ",
                crayon::bold("tws"),
                " client id (0)."
              )
            )
            usethis::ui_info(
              paste0(
                "You need to call ",
                crayon::bold("create_new_connections()"),
                " with ",
                crayon::italic("include_tws"),
                " = ",
                crayon::italic("TRUE"),
                " to open a tws socket."
              )
            )
            usethis::ui_info(
              paste0(
                "For more info, see the helpfile accessible at: ",
                crayon::bold("?create_new_connections")
              )
            )
            invisible()
          } else {
            sock_drawer$tws
          }
        )
        return(sock)
      }
      
      tryCatch(
        sock_drawer[[
          names(
            which(
              vapply(
                mget(ls(sock_drawer), envir = sock_drawer),
                attr,
                numeric(1),
                which = "client_id"
              ) == channel
            )
          )
          ]],
        error = function(e){
          do.call(
            what  = on.exit,
            args  = list(
              expr = substitute(try(close(sock), silent = TRUE)),
              add  = TRUE
            ),
            envir = parent.frame(5)
          )
          ib_sync_connect(client_id = channel)
        }
      )
      
    },
    error = function(e){
      
      call_time     <- Sys.time()
      ib_func       <- sys.call(-5)
      
      assign(
        "ib_connect_failure",
        value = dplyr::bind_rows(
          get0("ib_connect_failure", envir = get("error_log")),
          tibble::tibble(
            "time" = call_time,
            "call" = rlang::call_name(ib_func),
            "args" = list(rlang::call_args(ib_func))
          )
        ),
        envir = get("error_log")
      )
      
      conn_fail_msg <- paste0(
        crayon::bold(rlang::call_name(ib_func)),
        ": Could not connect to IB.\nSee ",
        crayon::bold("error_log$ib_connection_failure"),
        " for details."
      )
      
      usethis::ui_oops(conn_fail_msg)
      
      usethis::ui_info(
        paste0(
          "Verify that the API settings entered for your running instance of ",
          "TWS/IBG\n (which are found at ",
          crayon::inverse("File > Global Configuration > API > Settings"),
          ")\nagree with the below output of ",
          crayon::bold("active_connection_parameters"),
          "():"
        )
      )
      
      active_connection_parameters()
      
      usethis::ui_info(
        paste0(
          crayon::underline("Also verify in TWS/IBG's API setings that"),
          ":\n1. The ",
          crayon::inverse("\"Enable ActiveX and Socket Clients"),
          " box is ",
          crayon::underline(crayon::bold("checked")),
          "; and\n2. The ",
          crayon::inverse("Read-Only API"), 
          " box is ",
          crayon::bold(paste0(crayon::underline("un"), "checked")),
          " (if you wish to allow order placement)."
        )
      )
      
      do.call(
        what  = return,
        args  = list(value = "IB Connection Failure"),
        envir = parent.frame(5)
      )
      
      e
    }
  )
  
}
