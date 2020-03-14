#' Cancel Market Data
#'
#' Cancel an existing market data subscription.
#'
#' @param req_names
#' Character vector containing the names of the subscriptions you wish to
#' cancel. May contain any or all existing market data subsription names. If
#' left blank or set to value "**all**", then `cancel_mkt_data`() will cancel
#' every active market data subscription.
#'
#' @return
#' TRUE upon success (invisible), FALSE or error otherwise.
#'
#' @export
#'
#' @example inst/examples/mkt_data_ex.R
#' @family market data
#' @export
#'
cancel_mkt_data <- function(req_names){
  
  subscriptions <- get("subscriptions")

  if(all(subscriptions$mkt_data$req_name == "")){
    usethis::ui_info(
      "There are no active market data subscriptions to cancel."
    )
    return(invisible())
  }

  requests <- if(missing(req_names)){
    subscriptions$mkt_data$req_id
  } else {
    subscriptions$mkt_data %>%
      dplyr::filter(get("req_name") %in% req_names) %>%
      dplyr::select("req_id")
  }

  cancellator(
    requests   = requests,
    cancel_msg = expression(
      ib_encode_raw_msg(
        c(
          functionary$outgoing_msg_codes$CANCEL_MKT_DATA,
          functionary$function_versions_py$cancelMktData,
          req_id
        )
      )
    )
  )

}
