#' IB Sync Connect
#'
#' Creates a lightweight socket connection for sync usage.
#'
#' @param host Where is the IB connection hosted?
#' @param port What port is the IB connection listening on?
#'
#' @keywords internal
ib_sync_connect <- function(
  host      = active_itr_option("host"),
  port      = active_itr_option("port"),
  client_id
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

      client_id <- if(missing(client_id) || is.null(client_id)){
        if(conn_row - 2 == active_itr_option("master")){
          100
        } else {
          conn_row - 2
        }
      } else {
        client_id
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

      read_channel_raw(s)

      structure(s, conn_row = conn_row, client_id = client_id)

    },

    could_not_connect_error = function(e) stop(e)

  )

}
