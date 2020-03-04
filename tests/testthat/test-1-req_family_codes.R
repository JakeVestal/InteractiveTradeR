context("Request Family Codes")

if(connected){
  req_family_codes() %>% {
    test_that(
      "req_family_codes() has the right class.",
      expect_true(inherits(., "character"))
    )
    test_that(
      "req_family_codes() has names.",
      expect_named(.)
    )
  }
} else {
  test_that(
    "req_family_codes() errors if no connection.",
    expect_error(quiet(req_family_codes()))
  )
}
