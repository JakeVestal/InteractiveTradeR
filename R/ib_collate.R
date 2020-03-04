#' Group multi-response functions into lists.
#' @keywords internal
ib_collate <- function(stamped_responses_list){

  if(is.null(stamped_responses_list)){list(NULL)}

  InteractiveTradeR::functionary$collate_map[
    vapply(
      InteractiveTradeR::functionary$collate_map,
      function(cmap) any(cmap %in% names(stamped_responses_list)),
      FUN.VALUE = logical(1),
      USE.NAMES = FALSE
    )] %>% {
      c(
        stamped_responses_list[!(names(stamped_responses_list) %in% unlist(.))],
        purrr::imap(
          .,
          function(constituents, group_name){
            structure(
              stamped_responses_list[
                names(stamped_responses_list) %in% constituents
                ],
              class = c("list", group_name)
            )
          }
        )
      )
    } %>%
    Map(
      function(x, ib_finish_index){
        InteractiveTradeR::functionary$ib_finish[[ib_finish_index]](x)
      },
      x = .,
      ib_finish_index = match(
        names(.),
        names(InteractiveTradeR::functionary$ib_finish)
      )
    ) %>%
    purrr::compact()

}
