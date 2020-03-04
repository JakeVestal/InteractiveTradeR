#' Run InteractiveTradeR Example App
#'
#' Don't forget to open IB Gateway or Trader Workstation first!
#'
#' @param port The TCP port that the app should listen on. Defaults to choosing
#'   a random port.
#' @param launch.browser If true, the system's default web browser will be
#'   launched automatically after the app is started. Defaults to true in
#'   interactive sessions only.
#' @param host The IPv4 address that the application should listen on. Defaults
#'   to the \code{shiny.host} option, if set, or \code{"127.0.0.1"} if not.
#' @param display.mode The mode in which to display the example. Defaults to
#'   \code{showcase}, but may be set to \code{normal} to see the example without
#'   code or commentary.
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   # List all available examples
#'   run_example()
#'
#'   # Run one of the examples
#'   run_example("01_basics")
#'
#'   # Print the directory containing the code for all examples
#'   system.file("examples", package="InteractiveTradeR")
#' }
#' @export
launch_InteractiveTradeR_example_app <- function(
  port           = NULL,
  launch.browser = getOption('shiny.launch.browser', interactive()),
  host           = getOption('shiny.host', '127.0.0.1'),
  display.mode   = c("auto", "normal", "showcase")
){

  dir <- system.file(
    'InteractiveTradeR_example_app',
    package = 'InteractiveTradeR'
  )

  if (dir == ""){
    stop(crayon::bold("Example App not found!"))
    } else {

    shiny::runApp(
      dir,
      port = port,
      host = host,
      launch.browser = launch.browser,
      display.mode = display.mode
    )

  }

}
