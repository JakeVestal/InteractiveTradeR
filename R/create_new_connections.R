number_of_new_socks_param <- function(
  rox_list = list(
    "@param number_of_new_socks Integer, default = 1. Total number of new ",
    "connections to open, including Master and TWS (if requested). ",
    "Interactive Brokers allows a maximum of ",
    InteractiveTradeR::functionary$max_client_applications,
    "connections open at a time, per username."
  )
){ do.call("paste0", rox_list) }

#' Create New Connections to Interactive Brokers
#'
#' Creates a specified number of persistent async socket connections to
#'   Interactive Brokers. Returns them in a list, initialized and ready to send
#'   and receive data.
#'
#' @eval number_of_new_socks_param()
#'
#' @param include_master
#' Boolean, default = FALSE. If TRUE, then a connection using the
#' \link{master_client_id}{Master Client ID} will be opened and assigned the
#' name "**master**".
#'
#' @param include_tws
#' Boolean, default = FALSE. If TRUE, then a connection using
#' \link{client_id_0}{Client ID 0} will be included in the result and assigned
#' the name "**tws**".
#'
#' @param host
#' Character vector of length 1 giving the **hostname** for the session of IB
#' Gateway or TWS to which the connection(s) should be made. Usually is
#' "`localhost`" because most users connect to an instance of TWS/IBG that is
#' running on the same computer as InteractiveTradeR.
#'
#' @param port
#' Character vector length 1 or numeric giving the **port number** for the
#' session of IB Gateway or TWS to which the connection(s) should be made.
#'
#' @details
#' **Client IDs** In InteractiveTradeR, Client IDs are not assigned by the user;
#' instead, they are tracked internally. The user may assign subscriptions and
#' requests to the \link{master_client_id}{Master Client}, the
#' \link{client_id_0}{TWS Client}(Client ID = 0) or a generic socket.
#'
#' @return
#' NULL. This function is called for its side effects: opening sockets and
#' storing pointers that refer to them within InteractiveTradeR.
#'
#' @seealso disconnect
#'
#' @export
#'
create_new_connections <- function(
  number_of_new_socks = 1,
  include_master      = FALSE,
  include_tws         = FALSE,
  host                = active_itr_option("host"),
  port                = active_itr_option("port")
){

  # arg checks --------------------------------------------------------------

  # Edge case: User specifies 1 new sock, but wants to include both a master
  #  and a TWS GUI connection.
  if(number_of_new_socks == 1 && include_master && include_tws){

    err_msg(
      fun_name = "create_new_connections",
      msg      = paste0(
        "You can't include both a master connection ",
        crayon::bold("and"),
        " a Client ID 0 connection\n if you only allow 1 new connection in ",
        crayon::bold("number_of_new_socks"),
        "! :P "
      )
    )

    return(invisible())

  }

  # Check max connections rule
  if(
    InteractiveTradeR::functionary$max_client_applications < sum(
      length(ls(sock_drawer)), number_of_new_socks
    )
  ){

    err_msg(
      fun_name = "create_new_connections",
      msg      = paste0(
        "You've tried to open ",
        crayon::bold(number_of_new_socks),
        " new connections, but only ",
        crayon::bold(
          InteractiveTradeR::functionary$max_client_applications -
            nrow(showConnections())
        ), " new connections\n",
        "are possible without exceeding Interactive Brokers' limit of\n\t",
        crayon::bold(
          crayon::underline(
            InteractiveTradeR::functionary$max_client_applications,
            "connections",
            "at any given time"
          )
        ),
        "."
      )
    )

    return(invisible())

  }

  # Check for existing master connection and create one if all's well
  if(include_master){

    mcid <- fetch_master_w_msg()

    if(is.null(mcid)){return(invisible())}

    if(isTRUE(any(names(sock_drawer) == "master"))){

      err_msg(
        fun_name = "create_new_connections",
        msg      = paste0(
          "Cannot create a master connection because there is already a ",
          "master connection open!"
        )
      )

      return(invisible())

    }

    ib_connect(master = TRUE, host = host, port = port)

    number_of_new_socks <- number_of_new_socks - 1

  }

  # Check for existing Client ID 0 connection and create one if all's well
  if(number_of_new_socks > 0 && include_tws){

    if(isTRUE(any(names(sock_drawer) == "tws"))){

      err_msg(
        fun_name = "create_new_connections",
        msg      = paste0(
          "Cannot create a TWS GUI connection (Client ID = 0) because there",
          " is\nalready a TWS GUI connection open!"
        )
      )

      return(invisible())
    }

    ib_connect(tws = TRUE, host = host, port = port)

    number_of_new_socks <- number_of_new_socks - 1

  }

  # ----------------------------------------------------------------------------

  if(number_of_new_socks > 0){
    for(i in 1:number_of_new_socks){
      ib_connect(host = host, port = port)
    }
  }

  return(invisible())

}
