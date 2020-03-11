#' InteractiveTradeR
#' 
#' This package can be used to connect to Interactive Brokers' trading services
#' through the Trader Workstation (TWS) and IB Gateway (IBG) apps.
#'
#' @section Disclaimer and Waiver:
#' You can use this package for any and all purposes so long as it's connected
#' to your paper trading account or Interactive Brokers' live demo. You are
#' violating the license agreement in the instant you connect InteractiveTradeR
#' to a live Interactive Brokers account without permission. Please contact Jake
#' Vestal, the package author, for information on licensing.
#'
#' Not affiliated with Interactive Brokers, LLC in any way.
#'
#' @section Package options:
#'
#' InteractiveTradeR uses the following R options to configure your connections
#' to Trader Workstation (TWS) and IB Gateway (IBG):
#'
#' \itemize{
#'
#'   \item \code{interactivetrader.paper}: Set to TRUE to use options configured
#'   for \strong{PAPER} account; set to FALSE for \strong{LIVE}. Defaults to
#'   TRUE. \strong{Using InteractiveTradeR to trade real money without a license
#'   violates the license agreement}.
#'
#'   \item \code{interactivetrader.platform}: Set to "TWS" to use options
#'   configured for \strong{Trader Workstation} account; set to "IBG" for
#'   \strong{IB Gateway}. Defaults to TWS.
#'
#'   \item \code{interactivetrader.tws.paper.host}: Identifies its set value as
#'   the host name at which InteractiveTradeR should look to connect to an
#'   instance of Trader Workstation connected to a \strong{PAPER} account.
#'   Because most users run TWS on the same machine as InteractiveTradeR, this
#'   option should usually be kept to the default setting of "localhost".
#'
#'   \item \code{interactivetrader.tws.paper.host}: Identifies its set value as
#'   the host name at which InteractiveTradeR should look to connect to an
#'   instance of Trader Workstation connected to a \strong{PAPER} account.
#'   Because most users run TWS on the same machine as InteractiveTradeR, this
#'   option should usually be kept to the default setting of "localhost".
#'
#'   \item \code{interactivetrader.tws.paper.port}: Identifies the port on which
#'   an instance of Trader Workstation, connected to a \strong{PAPER} account,
#'   has been configured to listen for API connections. Default is
#'   \strong{7497}, matching the default set by Interactive Brokers.
#'
#'   \item \code{interactivetrader.tws.paper.master}: To use a
#'   \link{master_client_id}{Master Client ID} that you have configred within
#'   TWS for a \strong{PAPER} account, set this option to the value you have
#'   selected for your TWS paper account's Master Client ID.
#'
#'   \item \code{interactivetrader.ibg.paper.host}: Identifies its set value as
#'   the host name at which InteractiveTradeR should look to connect to an
#'   instance of IB Gateway connected to a \strong{PAPER} account. Because most
#'   users run IBG on the same machine as InteractiveTradeR, this option should
#'   usually be kept to the default setting of "localhost".
#'
#'   \item \code{interactivetrader.ibg.paper.port}: Identifies the port on which
#'   an instance of IB Gateway, connected to a \strong{PAPER} account, has been
#'   configured to listen for API connections. Default is \strong{4002**,
#'   matching the default set by Interactive Brokers.
#'
#'   \item \code{interactivetrader.ibg.paper.master}: To use a
#'   \link{master_client_id}{Master Client ID} that you have configred within
#'   IBG for a \strong{PAPER} account, set this option to the value you have
#'   selected for your IBG paper account's Master Client ID.
#'
#'   \item \code{interactivetrader.tws.live.host}: Identifies its set value as
#'   the host name at which InteractiveTradeR should look to connect to an
#'   instance of Trader Workstation connected to a \strong{LIVE} account on
#'   which actual trades will be executed with real money. Because most users
#'   run TWS on the same machine as InteractiveTradeR, this option should
#'   usually be kept to the default setting of "localhost". \strong{Using
#'   InteractiveTradeR to trade real money without a license violates the
#'   license agreement}.
#'
#'   \item \code{interactivetrader.tws.live.port}: Identifies the port on which
#'   an instance of Trader Workstation, connected to a \strong{LIVE} account,
#'   has been configured to listen for API connections. Default is
#'   \strong{7496}, matching the default set by Interactive Brokers.
#'   \strong{Using InteractiveTradeR to trade real money without a license
#'   violates the license agreement}.
#'
#'   \item \code{interactivetrader.tws.live.master}: To use a
#'   \link{master_client_id}{Master Client ID} that you have configred within
#'   TWS for a \strong{LIVE} account, set this option to the value you have
#'   selected for your TWS paper account's Master Client ID. \strong{Using
#'   InteractiveTradeR to trade real money without a license violates the
#'   license agreement}.
#'
#'   \item \code{interactivetrader.ibg.live.host}: Identifies its set value as
#'   the host name at which InteractiveTradeR should look to connect to an
#'   instance of IB Gateway connected to a \strong{LIVE} account on which actual
#'   trades will be executed with real money. Because most users run IBG on the
#'   same machine as InteractiveTradeR, this option should usually be kept to
#'   the default setting of "localhost". \strong{Using InteractiveTradeR to
#'   trade real money without a license violates the license agreement}.
#'
#'   \item \code{interactivetrader.ibg.live.port}: Identifies the port on which
#'   an instance of IB Gateway, connected to a \strong{LIVE} account, has been
#'   configured to listen for API connections. Default is \strong{4001},
#'   matching the default set by Interactive Brokers. \strong{Using
#'   InteractiveTradeR to trade real money without a license violates the
#'   license agreement}.
#'
#'   \item \code{interactivetrader.ibg.live.master}: To use a
#'   \link{master_client_id}{Master Client ID} that you have configred within
#'   IBG for a \strong{LIVE} account, set this option to the value you have
#'   selected for your IBG paper account's Master Client ID. \strong{Using
#'   InteractiveTradeR to trade real money without a license violates the
#'   license agreement}.
#'
#' }
#' @docType package
#' @name InteractiveTradeR
"_PACKAGE"

# Define "." for R CMD Check:
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

.onAttach <- function(libname, pkgname) {
  
  packageStartupMessage(
    crayon::bold("Welcome to InteractiveTradeR!"),
    "\nby Jake Vestal"
  )
  
}

archives      <- new.env(parent = emptyenv())
package_state <- new.env(parent = emptyenv())
error_log     <- new.env(parent = emptyenv())
assign("place_order_on_non_master", value = TRUE, envir = package_state)
assign("UNSET_DOUBLE", value = .Machine$double.xmax, envir = package_state)
assign("sync_time_out", value = 5, envir = package_state)
assign("default_market_data_type", value = 1, envir = package_state)
sock_drawer   <- new.env(parent = emptyenv())
subscriptions <- new.env(parent = emptyenv())
mkt_data      <- new.env(parent = emptyenv())
treasury      <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  
  if(interactive() && !check_for_saved_params()){
    
    save_default_params()
    
  }
  
}

.onUnload <- function(libpath){
  quiet(disconnect())
}
