context("Request Managed Accounts")

if(connected){
  test_that(
    "req_managed_accts() yields a result of the right class.",
    expect_true(inherits(req_managed_accts(), "character"))
  )
} else {
  test_that(
    "req_managed_accts() errors if not connected.",
    expect_error(quiet(req_managed_accts()))
  )
}
