context("Request Current Time")

if(connected){
  test_that(
    "req_current_time() yields a POSIXt object.",
    expect_true(lubridate::is.POSIXt(req_current_time()))
  )
} else {
  test_that(
    "req_current_time() errors if no connection.",
    expect_error(quiet(req_current_time()))
  )
}
