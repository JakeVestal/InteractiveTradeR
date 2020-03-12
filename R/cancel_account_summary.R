#' Cancel Account Summary
#'
#' Stop receiving account summary updates for the accounts that are members of
#' of groups specified in \emph{groupName}.
#'
#' @param groupNames
#' Character vector of length 1 or 2 containing the names of the subscriptions
#' you wish to cancel -- which, in the case of \link{req_account_summary}(), are
#' always GROUP names. If \emph{groupName} is not supplied, then
#' \code{cancel_account_summary}() will cancel all active subscription made by
#' \link{req_account_summary}().
#'
#' @details
#' If \emph{groupNames} is not supplied, then \code{cancel_account_summary}()
#' will cancel \strong{all} Account Summary subscriptions.
#'
#' @inherit cancel_account_updates_multi return
#'
#' @example inst/examples/account_summary_ex.R
#' @family treasury
#' @export
#'
cancel_account_summary <- function(groupNames){
  
  subscriptions <- get("subscriptions")
  
  if(all(subscriptions$account_summary$req_name == "")){
    usethis::ui_info(
      "There are no active account summary subscriptions to cancel."
    )
    return(invisible())
  }

  requests <- if(missing(groupNames)){
    subscriptions$account_summary$req_id
  } else {
    subscriptions$account_summary %>%
      dplyr::filter(get("req_name") %in% groupNames) %>%
      dplyr::select("req_id")
  }

  cancellator(
    requests   = requests,
    cancel_msg = expression(
      ib_encode_raw_msg(
        c(
          functionary$outgoing_msg_codes$
            CANCEL_ACCOUNT_SUMMARY,
          functionary$function_versions_py$
            cancelAccountSummary,
          req_id
        )
      )
    )
  )

}
