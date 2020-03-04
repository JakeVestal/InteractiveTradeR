context("Connection")

skip_if_not(connected)

test_that(
  desc = "The START_TIME in the connection handshakes are POSIXt objects.",
  expect_true(
    all(
      vapply(
        socks(),
        function(sock){lubridate::is.POSIXt(attr(sock, "start_time"))},
        FUN.VALUE = logical(1)
      )
    )
  )
)

test_that(
  paste0(
    "Connection Handshake returns a SERVER_VERSION that is between ",
    "MIN_CLIENT_VER and MAX_CLIENT_VER"
  ),
  expect_true(
    all(
      vapply(
        socks(),
        function(sock){
          attr(sock, "server_version") %>% {
            (
              . >= InteractiveTradeR::functionary$server_versions$MIN_CLIENT_VER
            ) && (
              . <= InteractiveTradeR::functionary$server_versions$MAX_CLIENT_VER
            )
          }
        },
        FUN.VALUE = logical(1)
      )
    )
  )
)
