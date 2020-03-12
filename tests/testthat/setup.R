test_html_output <- function(ref_path, ref_name){
  testthat::expect_false(
    object = readLines(ref_path) %>% 
      paste0(collapse = "") %>%
      grepl("Could not connect to IB", .),
    info   = ref_name
  )
  testthat::expect_false(
    object = readLines(ref_path) %>% 
      paste0(collapse = "") %>%
      grepl("Data retrieval failure in ", .),
    info   = ref_name
  )
  testthat::expect_false(
    object = readLines(ref_path) %>% 
      paste0(collapse = "") %>%
      grepl("object '(.*)' not found", .),
    info   = ref_name
  )
  testthat::expect_false(
    object = readLines(ref_path) %>% 
      paste0(collapse = "") %>%
      grepl("TIMEOUT", .),
    info   = ref_name
  )
  testthat::expect_false(
    object = readLines(ref_path) %>% 
      paste0(collapse = "") %>%
      grepl("Error in ", .),
    info   = ref_name
  )
}
