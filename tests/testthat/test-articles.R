context("Articles Ran Successfully")
# In other words, test basic operation of functions sync mode by making sure
#   functions' error messages do NOT appear anywhere in /docs/references.

list.files(
  file.path("..", "..", "..", "InteractiveTradeR", "docs", "articles"),
  full.names = TRUE,
  pattern    = "(.*)\\.html$"
) %>%
  stats::setNames(., gsub("\\.html$", "", basename(.))) %>%
  purrr::iwalk(
    function(ref_path, ref_name){
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
    }
  )
