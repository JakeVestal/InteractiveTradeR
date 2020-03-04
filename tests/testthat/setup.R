
# options(interactivetrader.platform         = "IBG")
# options(interactivetrader.paper            = TRUE)
# options(interactivetrader.tws.paper.host   = "localhost")
# options(interactivetrader.tws.paper.port   = 4002)
# options(interactivetrader.ibg.paper.master = 3011)

options(interactivetrader.platform         = "TWS")
options(interactivetrader.paper            = TRUE)
options(interactivetrader.tws.paper.host   = "localhost")
options(interactivetrader.tws.paper.port   = 7497)
options(interactivetrader.tws.paper.master = 57869)

# Create connections to use in test.
connect_success <- try(
  create_new_connections(5, include_master = TRUE, include_tws = TRUE),
  silent = TRUE
)

connected <- !inherits(connect_success, "try-error") && (
  length(ls(sock_drawer)) == 5
)

# Bump up sync timeout for pacing during tests
sync_timeout(60)

# Is the socket connected to a paper account?
using_ib_paper_account <- if(connected){
  identical(
    vapply(
      quiet(req_family_codes()),
      function(fam_code) substr(fam_code, start = 1, stop = 2),
      FUN.VALUE = character(1)
    ) %>%
      unique(),
    "DF"
  )
} else {
  FALSE
}

# Is the socket connected to a demo account?
using_ib_demo_account <- if(connected){
  identical(
    quiet(req_family_codes()),
    c("*" = "")
  )
} else {
  FALSE
}

if(connected & !using_ib_paper_account & !using_ib_demo_account){
  cli::cat_line(
    paste0(
      "\n#####################################################\n",
      crayon::inverse(
        crayon::bold(
          cli::symbol$warning,
          "   DANGER! DANGER! DANGER! DANGER! DANGER! DANGER!\n"
        )
      ),
      "#####################################################\n\n",
      crayon::bold(
        paste0(
          "Somehow, you've managed to connect this test script to\n",
          "a non-paper Interactive Brokers account.\n",
          "To proceed,"
        ),
        crayon::underline(
          crayon::red(
            "you must connect to a paper account.\n\n"
          )
        ),
        crayon::inverse(
          "This precaution is in place in order to prevent the\n",
          "tests from making trades with real money."
        )
      ),
      "\n\n#####################################################\n"
    )
  )

  connected <- FALSE
  testthat::set_reporter(StopReporter)
  stop()
}

during_market_hours <- !any(
  weekdays(Sys.Date()) %in% c("Saturday", "Sunday")
) &&
  lubridate::as_datetime(Sys.time(), tz = "EDT") %>% {
    . > lubridate::as_datetime(
      paste0(Sys.Date(), " 09:30:00 EDT"), tz = NULL
    ) && . < lubridate::as_datetime(
      paste0(Sys.Date(), " 16:00:00 EDT"), tz = NULL
    )
  }
