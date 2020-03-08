contract_param <- function(fun_name, parameters){
  paste0(
    "@param contract ",
    "Named character vector of contract parameter(s). The name of each ",
    "element identifies the parameter -- for example, \\code{secType}, ",
    "\\code{symbol}, \\code{exchange}... -- and each element itself gives the ",
    "set value (e.g., \"STK\", \"IBM\", \"SMART\"). \\cr \\cr ",
    "\\strong{The `contract` parameters that may be used with} ",
    "\\code{",
    fun_name,
    "}() \\strong{are}:\\cr",
    paste(
      paste0("\\code{", parameters, "}"),
      collapse = ", "
    ),
    "\\cr \\cr See \\link{contract} for detailed information on all ",
    "\\code{contract} parameters."
  )
}
