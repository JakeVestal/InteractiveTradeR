#' Validate IB Output
#' 
#' @keywords internal
#'
ib_validate <- function(output_obj){
  
  call_time     <- Sys.time()
  ib_func       <- rlang::call_name(sys.call(-1))
  
  switch(
    ib_func,
    "req_account_summary"    = is.null(output_obj) || 
      tibble::is_tibble(output_obj),
    "req_account_updates"    = tibble::is_tibble(output_obj) ||
      all(names(output_obj) %in% c("ACCOUNTS", "PORTFOLIO_VALUE")),
    "req_current_time"       = inherits(output_obj, "POSIXct"),
    "req_contract_details"   = is.null(output_obj) ||
      tibble::is_tibble(output_obj),
    "req_family_codes"       = inherits(output_obj, "character"),
    "req_managed_accts"      = inherits(output_obj, "character"),
    "req_matching_symbols"   = is.null(output_obj) ||
      tibble::is_tibble(output_obj),
    "req_sec_def_opt_params" = is.null(output_obj) ||
      (tibble::is_tibble(output_obj) && nrow(output_obj) == 1),
    FALSE
  ) %>% {
    if(.){
      output_obj
    } else {
      assign(
        "data_retrieval_errors",
        value = dplyr::bind_rows(
          get0("data_retrieval_errors", envir = get("error_log")),
          tibble::tibble(
            "time" = call_time,
            "call" = rlang::call_name(ib_func),
            "args" = list(rlang::call_args(ib_func))
          )
        ),
        envir = get("error_log")
      )
      usethis::ui_oops(
        paste0(
          "Data retrieval failure in ",
          crayon::bold(rlang::call_name(ib_func)),
          "().\nSee ",
          crayon::bold("error_log$data_retrieval_errors"),
          " for details."
        )
      )
    }
  }
  
}