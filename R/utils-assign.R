mkt_data_assign <- function(var_name, var_value, dta_name){
  assign(
    var_name,
    value = var_value,
    envir = get("mkt_data")[[dta_name]]
  )
  NULL
}
