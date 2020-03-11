#' InteractiveTradeR
#' @docType package
#' @keywords internal
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
