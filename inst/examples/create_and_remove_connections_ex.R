context("Creating & Removing Connections")

# Graceful Fails ---------------------------------------------------------------
# No connection needed.

test_that(
  "create_new_connections() fails gracefully in edge case 1.",
  expect_null(
    quiet(
      create_new_connections(
        number_of_new_socks = 1,
        include_master      = TRUE,
        include_tws         = TRUE
      )
    )
  )
)

test_that(
  "create_new_connections() fails gracefully if max conns exceeded.",
  expect_null(quiet(create_new_connections(500)))
)

test_that(
  "create_new_connections() fails gracefully if no connection.",
  expect_error(
    quiet(
      create_new_connections(
        number_of_new_socks = 5,
        host                = "some crazy hostname that doesn't exist",
        port                = 1537184
      )
    )
  )
)


# Creating Connections --------------------------------------------------------
# Connection needed from setup.R:
# test_sock_list <- create_new_connections(
#   number_of_new_socks = 5,
#   include_master      = TRUE,
#   include_tws_gui_0   = TRUE
# )

skip_if_not(connected_to_paper_account)

test_that(
  # Note: The only way to get a Client ID clash is if Master or TWS GUI
  #         sockets have already been opened.
  "create_new_connections() fails gracefully if Client ID already exists.",
  {
    expect_output(create_new_connections(1, include_master = TRUE))
    expect_output(create_new_connections(1, include_tws    = TRUE))
  }
)

test_that(
  paste0(
    "create_new_connections() creates and names socks correctly, and ",
    "disconnect works properly."
  ),
  {

    create_new_connections(5)

    expect_identical(
      names(socks()),
      c(
        "master", "sock_1", "sock_2", "sock_3", "sock_4", "sock_5", "sock_6",
        "sock_7", "sock_8", "tws"
      )
    )

    quiet(disconnect(5))

    expect_length(socks(), 5)

    expect_identical(
      names(socks()),
      c("master", "sock_1", "sock_2", "sock_3", "tws")
    )

    # Check that showConnections() and `sock_drawer` agree on conn_rows
    expect_identical(
      vapply(
        socks(),
        attr,
        numeric(1),
        which = "conn_row",
        USE.NAMES = FALSE
      ) %>%
        sort(),
      sort(
        as.numeric(
          rownames(
            showConnections()[
              showConnections()[,"mode"] == "ab" & (
                showConnections()[,"class"] == "sockconn"
              ),]
          )
        )
      )
    )

  }
)
