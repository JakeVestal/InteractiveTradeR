tags_param <- function()(
  paste0(
    "@param tags A character vector containing any or all of the following ",
    " allowed tags (Default is all tags):\n\n",
    paste(
      sort(
        unlist(
          functionary$account_summary_tags,
          use.names = FALSE
        )
      ),
      collapse = ", "
    ),
    "\n\nYou may also make use of the \\code{$LEDGER} tag to receive summary ",
    "data in different currencies. See \\strong{Details}."
  )
)
#' Request Account Summary
#'
#' This function will either initiate an ongoing subscription for account
#' summary data (if a persistant ASYNC socket is passed as input to
#' \emph{channel}), or quickly fetch the data on a socket that is opened for that
#' purpose and then closed (if no value is passed as input to  \emph{channel}).
#' All data retrieved by this function is stored in the \strong{ACCOUNT_SUMARY}
#' object accessible in the \strong{\code{treasury}}. The accounts to be included in the
#' output are specified by the \emph{groupName} parameter. The user may choose to
#' include any/all of a selection of account values by passing appropriate
#' values as \emph{tags}.
#'
#' @section Behavior:
#' \itemize{
#'   \item \strong{When a subscription is created by calling
#'   \code{req_account_summary}() on a socket}, the server immediately responds
#'   by providing values for the requested tags for all of the requested
#'   accounts, and the ACCOUNTS object is immediately updated in the treasury.
#'   Thereafter, IB will send updated account summary data to the socket,
#'   including only the \emph{tags} whose values have changed since the last
#'   request was made. IB will continue to update the socket until either the
#'   socket is closed (i.e., with \link{disconnect}(), or the subscription is
#'   cancelled by \link{cancel_account_summary}()).
#'   \item \strong{The "3-minute" update rule}: IB's documentation states that
#'   after the initial call to \code{req_account_summary}(), those account tags
#'   whose values have changed will be updated every three (3) minutes. However,
#'   during market hours, you may observe much shorter, more frequent update
#'   intervals. You can explore your observed update frequency by experimenting
#'   with the "Update Behavior" example in the "Examples" section below.
#'   \item \strong{Only two (2) active summary subscriptions are allowed at a
#'   time}: In practice, only subscription at a time should be needed if the API
#'   is well designed. If you do have two live subscriptions to
#'   \code{req_account_summary}(), InteractiveTradeR will not allow you to
#'   create another one until one of the two existing subscriptions is
#'   \link{cancel_account_summary}{cancelled}. This rule differs from IB's
#'   standard API, in which one of the existing subscriptions is overwritten
#'   with the new one. The reason for the change is to make it difficult for
#'   user to overwrite a subscription unintentionally.
#'   \item \strong{Adjusting \code{req_account_summary}() Subscriptions}:
#'   Calling \code{req_account_summary}() with a \emph{groupName} that is
#'   identical to that of one of the two possible existing live subscriptions to
#'   \code{req_account_summary}() will overwrite the existing subscription with
#'   the new one. This can be used to change subscribed \emph{tags}.
#' }
#'
#' @param groupName
#' Character vector of length 1 containing the name of an existing group of
#' accounts. Default value is "All". Descriptions of each parameter can be found
#' on IB's
#' \href{https://interactivebrokers.github.io/tws-api/interfaceIBApi_1_1EWrapper.html#acd761f48771f61dd0fb9e9a7d88d4f04}{accountSummary
#' Tags} page.
#'
#' @eval tags_param()
#'
#' @inheritParams req_current_time
#'
#' @param return_data
#' Boolean of length 1. Defaults to TRUE unless argument \emph{channel} is
#' specified. If FALSE, data retrieved by the function will be returned as the
#' funciton's output. If TRUE, then a Boolean succeses flag will be returned as
#' the function's output indicating the success (TRUE) or failure (FALSE) of the
#' function's attempt to transceive data to/from IB. Data in the
#' \strong{\code{treasury}} is always updated regardless of the value passed as
#' \emph{return_data} in the function call.
#'
#' @details
#' \strong{Groups}: Interactive Brokers' instructions for creating account
#' groups can be in the
#' \href{https://www.interactivebrokers.com/en/index.php?f=980#3b}{Creating
#' Account Groups} section of IB's TWS webinar notes. Any group create in TWS
#' can be input as \emph{groupName}.
#'
#' \strong{\code{"$LEDGER"} tags}: If you hold assets in your various accounts
#' that are denominated in different currencies, you can use the use the tag
#' \code{"$LEDGER:<currency>"} to summarize holdings by currency. Here,
#' \code{<currency>} is a three-letter currency abbreviation, e.g.,
#' \code{"$LEDGER:USD"}, \code{"$LEDGER:GBP"}, \code{"$LEDGER:CHF"}, etc.
#' \itemize{
#'   \item Use the tag \code{"$LEDGER"}, with no currency specified, to get
#'   summary data in only the currency you have set as your BASE currency in
#'   Account Management.
#'   \item Use \code{"$LEDGER:ALL"} tag to get summary data for all currencies.
#' }
#' \strong{Max of 50 subaccounts for }\code{groupName = "All"}: If there are
#' more than 50 subaccounts accessible to a particular user, then the tag "All"
#' may not be used for \code{groupName}.
#'
#' @return
#' This function is called for its side effect of updating the
#' \strong{\code{treasury}}, which takes place every time the function executes.
#' Additionally, the function's return value depends upon the value passed in as
#' \emph{return_data} as follows:
#'
#' \itemize{
#'   \item \strong{If \code{return_data == FALSE}}: A Boolean success flag,
#'   returned invisibly, indicating that the function executed correctly and
#'   updated the \strong{\code{treasury}} with any new data retrieved.
#'   \item \strong{If \code{return_data == TRUE}}: Any new data retrieved will
#'   be returned in a \link[tibble]{tibble} in addition to being added to the
#'   \strong{\code{treasury}}. If no new data is available, returns NULL.
#' }
#' 
#' \emph{return_data} defaults to TRUE unless \emph{channel} is specified.
#'
#' @section ACCOUNTS Treasury Object:
#' \code{req_account_summary}() updates the \strong{ACCOUNTS} object in the
#' treasury. ACCOUNTS is a \link{tibble} in which each row represents a
#' parameter pertaining to a particular account. Has the following columns:
#' \itemize{
#'   \item \strong{account} <chr>: Account ID (e.g., "DF1234567")
#'   \item \strong{tag}  <chr>: Name of account tag (e.g., "BuyingPower")
#'   \item \strong{tag_value} <chr>: Value of tag (e.g., "500000")
#'   \item \strong{currency} <chr>: 3-letter currency abbreviation if
#'     \code{tag_value} is a monetary amount, "" otherwise.
#' }
#'
#' @inherit cancel_account_summary examples
#' @family treasury
#' @export
#'
req_account_summary <- function(
  groupName   = "All",
  tags        = "All",
  channel     = NULL,
  return_data = is.null(channel)
){
  
  if(
    isTRUE(nrow(subscriptions$account_summary) >= 2) &&
    any(subscriptions$account_summary == groupName)
  ){
    usethis::ui_oops(
      paste0(
        "IB supports a maximum of 2 active ",
        crayon::bold("account summmary"),
        " at a time.\nPlease cancel an existing account summary subscription ",
        "before creating another one."
      )
    )
    usethis::ui_info(
      paste0(
        "You may view your active account summmary subscriptions with the ",
        "command:\n> ",
        crayon::bold("subscriptions$account_summary"),
        "."
      )
    )
    usethis::ui_info(
      paste0(
        "For more info, see documentation for cancel_account_summary, ",
        "accessed by command:\n> ",
        crayon::bold("?cancel_account_summary")
      )
    )
    return(invisible(FALSE))
  }
  
  sock   <- select_sock_for_api_fun()
  req_id <- fetch_and_bump("account_summary")
  tags   <- match.arg(
    arg        = grep("\\$LEDGER", tags) %>% {
      if(identical(., integer(0))){
        tags
      } else {
        tags[-.]
      }
    },
    choices    = c(
      "All",
      unlist(functionary$account_summary_tags)
    ),
    several.ok = TRUE
  ) %>% {
    if(any(tags == "All")){
      unlist(functionary$account_summary_tags)
    } else {
      .
    }
  } %>%
    c(tags[grep("\\$LEDGER", tags)])
  
  req_account_summary_msg <- ib_encode_raw_msg(
    c(
      functionary$outgoing_msg_codes$REQ_ACCOUNT_SUMMARY,
      functionary$function_versions_py$reqAccountSummary,
      req_id,
      groupName,
      paste(tags, collapse = ", ")
    )
  )
  
  writeBin(
    object = req_account_summary_msg,
    con    = sock,
    endian = "big"
  )
  
  account_summary <- sock_seek(
    element_names   = "ACCOUNTS",
    socket          = sock,
    success_element = simple_encode(
      functionary$incoming_msg_codes$ACCOUNT_SUMMARY_END
    )
  )
  
  assign(
    "ACCOUNTS",
    value = structure(
      functionary$ib_update$ACCOUNTS(
        account_summary
      ),
      last_updated = Sys.time()
    ),
    envir = get("treasury")
  )
  
  if(is.null(channel)){
    writeBin(
      object = ib_encode_raw_msg(
        c(
          functionary$outgoing_msg_codes$
            CANCEL_ACCOUNT_SUMMARY,
          functionary$function_versions_py$
            cancelAccountSummary,
          req_id
        )
      ),
      con    = sock,
      endian = "big"
    )
  } else {
    subscribe(
      fun_name = "account_summary",
      req_name = groupName,
      req      = list(tags = paste(tags, collapse = ", ")),
      sock     = sock,
      req_id   = req_id
    )
  }
  
  if(return_data){
    ib_validate(account_summary)
  } else {
    invisible(
      tibble::is_tibble(account_summary) && (
        is.null(channel) ||
          any(subscriptions$account_summary$req_name == groupName)
      )
    )
  }
  
}
