context("Articles Ran Successfully")
# In other words, test basic operation of functions sync mode by making sure
#   functions' error messages do NOT appear anywhere in /docs/references.

list.files(
  file.path("..", "..", "..", "InteractiveTradeR", "docs", "articles"),
  full.names = TRUE,
  pattern    = "(.*)\\.html$"
) %>%
  stats::setNames(., gsub("\\.html$", "", basename(.))) %>%
  purrr::iwalk(test_html_output)
