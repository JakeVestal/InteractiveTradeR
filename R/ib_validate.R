#' Validate IB Output
#' 
#' @keywords internal
#'
ib_validate <- function(output_obj){
  
  call_time     <- Sys.time()
  ib_func       <- rlang::call_name(sys.call(-1))
  ib_func_frame <- sys.frame(-1)
  
  switch(
    ib_func,
    "req_account_summary"    =  is.null(output_obj) || 
      tibble::is_tibble(output_obj),
    "req_account_updates"    =  is.null(output_obj) ||
      tibble::is_tibble(output_obj),
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
          error_log$data_retrieval_errors,
          tibble::tibble(
            "time" = call_time,
            "call" = ib_func,
            "args" = mget(ls(ib_func_frame), envir = ib_func_frame)
          )
        )
      )
      usethis::ui_oops(
        paste0(
          "Data retrieval failure in ",
          crayon::bold(ib_func),
          "().\nSee ",
          crayon::bold("error_log$data_retrieval_errors"),
          " for details."
        )
      )
    }
  }
  
}