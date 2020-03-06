#' InteractiveTradeR
#'
#' @section Disclaimer and Waiver:
#' You can use this package to make actual trades on real money if you connect
#' it to one of your live accounts.
#'
#' Not affiliated with Interactive Brokers.
#'
#' Only a fool would place blind trust into some random package downloaded off
#' the Internet.
#'
#' @section Package options:
#'
#' InteractiveTradeR uses the following [options()] to configure your
#' connections to Trader Workstation (TWS) and IB Gateway (IBG):
#'
#' \itemize{
#'
#'   \item `interactivetrader.paper`: Set to TRUE to use options configured
#'   for **PAPER** account; set to FALSE for **LIVE**. Defaults to TRUE. **Using
#'   InteractiveTradeR to trade real money without a license violates the
#'   license agreement**.
#'
#'   \item `interactivetrader.platform`: Set to "TWS" to use options configured
#'   for **Trader Workstation** account; set to "IBG" for **IB Gateway**.
#'   Defaults to TWS.
#'
#'   #'   \item `interactivetrader.tws.paper.host`: Identifies its set value as
#'   the host name at which InteractiveTradeR should look to connect to an
#'   instance of Trader Workstation connected to a **PAPER** account. Because
#'   most users run TWS on the same machine as InteractiveTradeR, this option
#'   should usually be kept to the default setting of "localhost".
#'
#'   \item `interactivetrader.tws.paper.host`: Identifies its set value as the
#'   host name at which InteractiveTradeR should look to connect to an instance
#'   of Trader Workstation connected to a **PAPER** account. Because most users
#'   run TWS on the same machine as InteractiveTradeR, this option should
#'   usually be kept to the default setting of "localhost".
#'
#'   \item `interactivetrader.tws.paper.port`: Identifies the port on which an
#'   instance of Trader Workstation, connected to a **PAPER** account, has been
#'   configured to listen for API connections. Default is **7497**, matching the
#'   default set by Interactive Brokers.
#'
#'   \item `interactivetrader.tws.paper.master`: To use a
#'   \link{master_client_id}{Master Client ID} that you have configred within
#'   TWS for a **PAPER** account, set this option to the value you have selected
#'   for your TWS paper account's Master Client ID.
#'
#'   \item `interactivetrader.ibg.paper.host`: Identifies its set value as the
#'   host name at which InteractiveTradeR should look to connect to an instance
#'   of IB Gateway connected to a **PAPER** account. Because most users run IBG
#'   on the same machine as InteractiveTradeR, this option should usually be
#'   kept to the default setting of "localhost".
#'
#'   \item `interactivetrader.ibg.paper.port`: Identifies the port on which an
#'   instance of IB Gateway, connected to a **PAPER** account, has been
#'   configured to listen for API connections. Default is **4002**, matching the
#'   default set by Interactive Brokers.
#'
#'   \item `interactivetrader.ibg.paper.master`: To use a
#'   \link{master_client_id}{Master Client ID} that you have configred within
#'   IBG for a **PAPER** account, set this option to the value you have selected
#'   for your IBG paper account's Master Client ID.
#'
#'   \item `interactivetrader.tws.live.host`: Identifies its set value as the
#'   host name at which InteractiveTradeR should look to connect to an instance
#'   of Trader Workstation connected to a **LIVE** account on which actual
#'   trades will be executed with real money. Because most users run TWS on the
#'   same machine as InteractiveTradeR, this option should usually be kept to
#'   the default setting of "localhost". **Using InteractiveTradeR to trade real
#'   money without a license violates the license agreement**.
#'
#'   \item `interactivetrader.tws.live.port`: Identifies the port on which an
#'   instance of Trader Workstation, connected to a **LIVE** account, has been
#'   configured to listen for API connections. Default is **7496**, matching the
#'   default set by Interactive Brokers. **Using InteractiveTradeR to trade real
#'   money without a license violates the license agreement**.
#'
#'   \item `interactivetrader.tws.live.master`: To use a
#'   \link{master_client_id}{Master Client ID} that you have configred within
#'   TWS for a **LIVE** account, set this option to the value you have selected
#'   for your TWS paper account's Master Client ID. **Using InteractiveTradeR to
#'   trade real money without a license violates the license agreement**.
#'
#'   \item `interactivetrader.ibg.live.host`: Identifies its set value as the
#'   host name at which InteractiveTradeR should look to connect to an instance
#'   of IB Gateway connected to a **LIVE** account on which actual
#'   trades will be executed with real money. Because most users run IBG on the
#'   same machine as InteractiveTradeR, this option should usually be kept to
#'   the default setting of "localhost". **Using InteractiveTradeR to trade real
#'   money without a license violates the license agreement**.
#'
#'   \item `interactivetrader.ibg.live.port`: Identifies the port on which an
#'   instance of IB Gateway, connected to a **LIVE** account, has been
#'   configured to listen for API connections. Default is **4001**, matching the
#'   default set by Interactive Brokers. **Using InteractiveTradeR to trade real
#'   money without a license violates the license agreement**.
#'
#'   \item `interactivetrader.ibg.live.master`: To use a
#'   \link{master_client_id}{Master Client ID} that you have configred within
#'   IBG for a **LIVE** account, set this option to the value you have selected
#'   for your IBG paper account's Master Client ID. **Using InteractiveTradeR to
#'   trade real money without a license violates the license agreement**.
#'
#' }
#' @docType package
#' @name InteractiveTradeR
"_PACKAGE"

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

.onAttach <- function(libname, pkgname) {
  
  packageStartupMessage(
    crayon::bold("Welcome to InteractiveTradeR!"),
    "\nby Jake Vestal"
  )
  
}

archives      <- new.env(parent = emptyenv())
package_state <- new.env(parent = emptyenv())
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
