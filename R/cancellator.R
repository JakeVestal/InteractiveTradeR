#' The Cancellator
#' Handles most `cancel_` API function calls.
#' @keywords internal
cancellator <- function(
  cancel_msg,
  requests          = NULL,
  fun_name          = rlang::call_name(sys.call(-1)),
  subscription_name = gsub("cancel_", "", fun_name)
){

  subscriptions <- get("subscriptions")
  
  requests <- if(is.null(requests)){
    id_or_name <- "req_name"
    unlist(subscriptions[[subscription_name]][id_or_name], use.names = FALSE)
  } else if(any(is.na(suppressWarnings(as.numeric(requests))))){
    id_or_name <- "req_name"
    match.arg(
      arg        = requests,
      choices    = unlist(
        subscriptions[[subscription_name]][id_or_name],
        use.names = FALSE
      ),
      several.ok = TRUE
    )
  } else {
    id_or_name <- "req_id"
    as.numeric(requests)
  }

  check_vec <- vapply(
    1:length(requests),
    function(i){

      req_id <- subscriptions[[subscription_name]]$req_id[
          subscriptions[[subscription_name]][id_or_name] == requests[i]
          ]
      client_id <- subscriptions[[subscription_name]]$client_id[
        subscriptions[[subscription_name]][id_or_name] == requests[i]
        ]

      if(is.na(req_id)){
        usethis::ui_oops(
          paste0(
            "No ",
            crayon::bold(subscription_name),
            " subscription found for ",
            crayon::bold(id_or_name),
            " = ",
            crayon::italic(requests[i]),
            "."
          )
        )
        return(invisible())
      }

      unsubscribe(
        subscription_name = subscription_name,
        request           = requests[i],
        id_or_name        = id_or_name,
        msg               = eval(cancel_msg),
        channel           = client_id
      )

      !any(subscriptions[[subscription_name]][id_or_name] == requests[i])

    },
    logical(1)
  )

  return(invisible(all(check_vec)))

}
