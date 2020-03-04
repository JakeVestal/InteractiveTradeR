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
              expr = substitute(close(sock)),
              add  = TRUE
            ),
            envir = parent.frame(5)
          )
          ib_sync_connect(client_id = channel)
        }
      )

    },
    error = function(e){
      usethis::ui_oops("Could not connect to IB")
      usethis::ui_info(
        paste0(
          "Make sure that an instance of Trader Workstation (TWS) or IB ",
          "Gateway (IBG) is open."
        )
      )
      usethis::ui_info(
        paste0(
          "Verify that InteractiveTradeR is attempting to connect to TWS / ",
          "IBG with a valid set\nof parameters by issuing command ",
          crayon::bold("active_connection_parameters"),
          "()."
        )
      )
      stop(e$message)
    }
  )

}
