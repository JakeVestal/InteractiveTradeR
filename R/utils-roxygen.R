contract_param <- function(fun_name, parameters){
  paste0(
    "@param contract ",
    "Named character vector of contract parameter(s). The name of each ",
    "element identifies the parameter -- e.g., `secType` or `symbol` -- ",
    "and each element itself gives the set value, e.g., `STK` or `IBM`. ",
    "\\cr \\cr **The `contract` parameters that may be passed in to `",
    fun_name,
    "`() are**:\\cr",
    paste(
      paste0("`", parameters, "`"),
      collapse = ", "
    ),
    "\\cr \\cr See \\link{contract} for detailed information on all ",
    "`contract` parameters."
  )
}
