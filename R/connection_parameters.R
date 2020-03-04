#' Connection Settings
#'
#' Describe
#'
connection_parameters <- function(conn_params = NULL){

  if(is.null(conn_params)){
    return(
      tibble::tibble(
        trades   = c("paper", "paper", "live", "live"),
        platform = c("TWS", "IBG", "TWS", "IBG"),
        host     = unlist(
          c(
            options("interactivetrader.tws.paper.host"),
            options("interactivetrader.ibg.paper.host"),
            options("interactivetrader.tws.live.host"),
            options("interactivetrader.ibg.live.host")
          ),
          use.names = FALSE
        ),
        port = unlist(
          c(
            options("interactivetrader.tws.paper.port"),
            options("interactivetrader.ibg.paper.port"),
            options("interactivetrader.tws.live.port"),
            options("interactivetrader.ibg.live.port")
          ),
          use.names = FALSE
        ),
        master = unlist(
          c(
            options("interactivetrader.tws.paper.master"),
            options("interactivetrader.ibg.paper.master"),
            options("interactivetrader.tws.live.master"),
            options("interactivetrader.ibg.live.master")
          ),
          use.names = FALSE
        )
      )
    )
  }

  if(
    !identical(
      colnames(conn_params),
      c("trades", "platform", "host", "port", "master")
    )
  ){
    usethis::ui_oops(
      paste0(
        "Connection parameters colnames must be: \"trades\", \"platform\", ",
        "\"host\", \"port\", \"master\")"
      )
    )
    return(invisible())
  }

  if(
    !identical(
      conn_params[,c("trades", "platform")],
      tibble::tibble(
        trades = c("paper", "paper", "live", "live"),
        platform = c("TWS", "IBG", "TWS", "IBG")
      )
    )
  ){
    usethis::ui_oops(
      "Malformed trades or platform column in connection parameters."
    )
    return(invisible())
  }

  saved_params <- readLines(
    file.path(rprojroot::find_package_root_file(), ".Rprofile")
  )

  for(i in 1:nrow(conn_params)){
    for(param in c("host", "port", "master")){
      param_line <- grep(
        paste0(
          "^options\\(interactivetrader\\.",
          tolower(conn_params$platform[i]),
          "\\.",
          tolower(conn_params$trades[i]),
          "\\.",
          param
        ),
        saved_params
      )

      saved_params[param_line] <- gsub(
        "(= )(.*)(\\))",
        paste0(
          "\\1",
          if(param == "host"){"\""},
          conn_params[i, param],
          if(param == "host"){"\""},
          "\\3"
        ),
        saved_params[param_line]
      )

      eval(parse(text = saved_params[param_line]))

    }
  }

  writeLines(
    saved_params, file.path(rprojroot::find_package_root_file(), ".Rprofile")
  )

}

use_tws <- function(paper = TRUE, make_default = FALSE){

  usethis::ui_done("Switching connection parameters to TWS...")
  disconnect()
  usethis::ui_info("Clearing connections and subscriptions...")
  clean_slate(c("sock_drawer", "subscriptions"))

  saved_params <- readLines(
    file.path(rprojroot::find_package_root_file(), ".Rprofile")
  )

  grep(
    "^options\\(interactivetrader\\.paper",
    saved_params
  ) %>% {
    saved_params[.] <<- gsub(
      "(= )(.*)(\\))",
      paste0("\\1", paper, "\\3"),
      saved_params[.]
    )
    eval(parse(text = saved_params[.]))
  }

  grep(
    "^options\\(interactivetrader\\.platform",
    saved_params
  ) %>% {
    saved_params[.] <<- gsub(
      "(= )(.*)(\\))",
      paste0("\\1\"TWS\"\\3"),
      saved_params[.]
    )
    eval(parse(text = saved_params[.]))
  }

  if(make_default){
    usethis::ui_done(
      crayon::bold(
        "InteractiveTradeR will default to the following parameters upon load:"
      )
    )
    cat("\n")
    writeLines(
      saved_params, file.path(rprojroot::find_package_root_file(), ".Rprofile")
    )
  }

  active_connection_parameters()

}

use_ibg <- function(paper = TRUE, make_default = FALSE){

  usethis::ui_done("Switching connection parameters to TWS...")
  disconnect()
  usethis::ui_info("Clearing connections and subscriptions...")
  clean_slate(c("sock_drawer", "subscriptions"))

  saved_params <- readLines(
    file.path(rprojroot::find_package_root_file(), ".Rprofile")
  )

  grep(
    "^options\\(interactivetrader\\.paper",
    saved_params
  ) %>% {
    saved_params[.] <<- gsub(
      "(= )(.*)(\\))",
      paste0("\\1", paper, "\\3"),
      saved_params[.]
    )
    eval(parse(text = saved_params[.]))
  }

  grep(
    "^options\\(interactivetrader\\.platform",
    saved_params
  ) %>% {
    saved_params[.] <<- gsub(
      "(= )(.*)(\\))",
      paste0("\\1\"IBG\"\\3"),
      saved_params[.]
    )
    eval(parse(text = saved_params[.]))
  }

  if(make_default){
    usethis::ui_done(
      crayon::bold(
        "InteractiveTradeR will default to the following parameters upon load:"
      )
    )
    cat("\n")
    writeLines(
      saved_params, file.path(rprojroot::find_package_root_file(), ".Rprofile")
    )
  }

  active_connection_parameters()

}

active_connection_parameters <- function(){
  structure(
    tibble::tibble(
      live             = !unlist(
        options("interactivetrader.paper"),
        use.names = FALSE
      ),
      platform         = unlist(
        options("interactivetrader.platform"),
        use.names = FALSE
      ),
      port             = active_itr_option(port),
      host             = active_itr_option(host),
      master_client_id = active_itr_option(master)
    )
    ,
    class = c("active_conn_params", "tbl_df", "tbl", "data.frame")
  )
}
