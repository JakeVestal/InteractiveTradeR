#' Decode IB Raw Vector
#'
#' Accepts a list of raw vector  and returns them as formatted, human
#'   readable R data objects.
#' @keywords internal
ib_decode <- function(ib_parsed_raw_list){
  # during operation, `ib_decode` SHOULD fail with error if any element of
  #   `ib_parsed_raw_list` is NULL, so don't bother checking for NULL.
  
  if(length(ib_parsed_raw_list) == 0){NULL}
  
  lapply(
    ib_parsed_raw_list,
    function(ib_vec_raw){
      ib_decode_low_level_msg(ib_vec_raw)
    }
  ) %>%
    stats::setNames(
      names(InteractiveTradeR::functionary$incoming_msg_codes)[match(
        vapply(., function(x){x[1]}, character(1)),
        InteractiveTradeR::functionary$incoming_msg_codes
      )]
    ) %>% {
      .[!is.na(names(.))]
    } %>%
    Map(
      function(ibdc, ib_polish_index){
        InteractiveTradeR::functionary$ib_polish[[ib_polish_index]](ibdc)
      },
      .,
      match(
        names(.),
        names(InteractiveTradeR::functionary$ib_polish)
      )
    ) %>% {
      y <- .[!vapply(., is.null, logical(1))]
      if(length(y) == 0) return(list("no data"))
      y
    }
  
}
