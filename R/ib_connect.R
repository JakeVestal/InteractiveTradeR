#' Establish a connection with Interactive Brokers
#'
#' Creates a socket connection to a running instance of Trader Workstation or
#'   IB Gateway -- either persistant (async) or one-time (sync).
#'
#' @param client_id desired client id as an INT or CHAR. Must be a number
#'  between 0 and 32767, inclusive.
#' @param async boolean:
#'
#' "\strong{FALSE}": Creates a socket that will immediately return the server's
#'   response to an API call, even if it means putting other processes on hold
#'   while the response is retrieved.
#'
#' "\strong{TRUE}" (default): The likely case if `ib_connect()` is being called
#'   by a user instead of within one of the API calls. Will create a persistent
#'   async socket that, after being writen to, will release controlto other
#'   processes while the socket retrieves data from IB, which can be read later.
#'
#' @param host (default = \code{localhost}) Where is the IB connection hosted?
#' @param port (default = 4002) What port is the IB connection listening on?
#'  Defaults to 4002, which is IB's default port for the paper trading account
#'  port accessible using IB Gateway.
#'
#' @keywords internal
ib_connect <- function(
  master = FALSE,
  tws    = FALSE,
  host   = active_itr_option("host"),
  port   = active_itr_option("port")
){
  
  tryCatch(
    
    {
      
      conn_rows_1 <- rownames(showConnections())
      
      s <- suppressWarnings(
        try(
          socketConnection(
            host     = host,
            port     = port,
            open     = "ab",
            encoding = "ascii"
          ),
          silent = TRUE
        )
      )
      
      if(class(s) == "try-error" || !isOpen(s)){
        stop(
          structure(
            class = c("could_not_connect_error", "error", "condition"),
            list(message = s, call = sys.call())
          )
        )
      }
      
      conn_row  <- as.numeric(
        setdiff(
          rownames(showConnections()),
          conn_rows_1
        )
      )
      
      if(master){
        client_id <- active_itr_option("master")
        sock_name <- "master"
      } else if(tws){
        client_id <- 0
        sock_name <- "tws"
      } else {
        client_id <- min(
          setdiff(
            1:32,
            vapply(
              socks(),
              attr,
              which = "client_id",
              numeric(1)
            )
          )
        )
        
        if(client_id == active_itr_option("master")){
          client_id <- 100
        }
        
        sock_name <- paste0("sock_", client_id)
      }
      
      writeBin(
        object = c(
          InteractiveTradeR:::functionary$fixed_api_msgs$initiate_handshake_msg,
          ib_encode_raw_msg(
            c(
              InteractiveTradeR:::functionary$outgoing_msg_codes$START_API,
              InteractiveTradeR:::functionary$function_versions_py$startApi,
              client_id,
              ""
            )
          )
        ),
        con    = s,
        endian = "big"
      )
      
      socket_raw_data <- read_channel_raw(s)
      
      handshake <- ib_decode_low_level_msg(socket_raw_data[[1]]) %>% {
        list(
          "server_version" = .[[1]],
          "start_time"     = lubridate::as_datetime(.[[2]], tz = NULL)
        )
      }
      
      assign(
        sock_name,
        value = structure(
          s,
          client_id      = client_id,
          conn_row       = conn_row,
          mktDataType    = 1,
          server_version = handshake$server_version,
          start_time     = handshake$start_time
        ),
        envir = sock_drawer
      )
      
    },
    
    could_not_connect_error = function(e) stop(e)
    
  )
  
}
