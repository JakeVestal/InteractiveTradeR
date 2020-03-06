#' Retreive Active Connection Parameters
#'
#'
#' @export
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
      port             = active_itr_option("port"),
      host             = active_itr_option("host"),
      master_client_id = active_itr_option("master")
    )
    ,
    class = c("active_conn_params", "tbl_df", "tbl", "data.frame")
  )
}
