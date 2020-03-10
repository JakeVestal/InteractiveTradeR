#' @keywords internal
ib_update <- function(incoming_data, rtn_elements = NULL){

  if(any(!(names(incoming_data) %in% rtn_elements))){
    incoming_data[which(!(names(incoming_data) %in% rtn_elements))] %>%
      Map(
        function(x, ib_update_index){
          functionary$ib_update[[ib_update_index]](x)
        },
        x = .,
        ib_update_index = match(
          names(.),
          names(functionary$ib_update)
        )
      ) %>%
      purrr::compact() %>%
      purrr::imap(
        function(updated_data, data_obj_name){
          assign(
            data_obj_name,
            value = structure(updated_data, last_updated = Sys.time()),
            envir = eval(
              parse(
                text = {
                  for(
                    envir in names(functionary$environ_map)
                  ){
                    if(
                      any(
                        functionary$environ_map[[
                          envir
                          ]] == data_obj_name
                      )
                    ){break()}
                  }
                  envir
                }
              )
            )
          )
        }
      )
  }

  if(is.null(rtn_elements)){
    return(invisible())
  }

  if(length(rtn_elements) == 1){
    return(incoming_data[[rtn_elements]])
  }

  incoming_data[rtn_elements]

}
