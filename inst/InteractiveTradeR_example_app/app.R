#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(InteractiveTradeR)
library(shiny)
library(magrittr)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)

# Define UI
ui <- dashboardPagePlus(
  skin = "black",
  shinyjs::useShinyjs(),

  header       = dashboardHeaderPlus(),
  sidebar      = shinydashboard::dashboardSidebar(

    sidebarMenu(
      id = "sidebarMenu",
      sidebarUserPanel(
        "IB API for R",
        subtitle = paste0(
          "Version ",
          paste(InteractiveTradeR:::functionary$api_version, collapse = ".")
        )
      ),
      div(
        id = "ibConnectPane",
        align = "center",
        numericInput(
          inputId = "port",
          label   = "Port",
          value   = 4002,
          width   = "100px"
        ),
        textInput(
          inputId = "host",
          label   = "Host",
          value   = "localhost"
        ),
        actionButton(
          inputId = "connect",
          label   = "Connect",
          icon    = icon("plug")
        )
      ),
      menuItem(
        text = "Add API Element",
        icon = icon("th"),
        tabName = "add_api_element_tab",
        menuSubItem("Request Current Time", tabName = "req_current_time")
      )
    )
  ),

  body = shinydashboard::dashboardBody(

    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "InteractiveTradeR.css"
      ),
      tags$script(src = "InteractiveTradeR.js")
    ),

    shinyjqui::jqui_draggable(
      widgetUserBox(
        title = "Sock Drawer",
        subtitle = "Create and and manage connections to Interactive Brokers",
        type = 2,
        width = 12,
        src = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
        color = "black",
        closable = TRUE,
        div(
          style="display: inline-block;vertical-align:middle; width: 150px;",
          {
            sock_type_options  <- c(
              "Master" = "master",
              "Normal" = "normal",
              "TWS"    = "tws_client_0"
            )
            pickerInput(
              inputId = "sock_type",
              label   = "Sock Type",
              choices = sock_type_options,
              options = list(
                style = "btn-info"
              )
            )
          }
        ),
        div(
          style = "display: inline-block;vertical-align:middle;",
          actionButton(inputId = "connect_sock", label = "Connect")
        ),
        div(
          style = "display: inline-block;vertical-align:middle; width: 250px;",
          textInput(
            inputId = "sock_name",
            label = "Name",
            value = "sock_1",
            placeholder = "Please enter a socket name"
          )
        ),
        footer = DTOutput(outputId = "sock_drawer_dt")
      )
    )
  ),
  rightsidebar = rightSidebar()


)

# Define server logic
server <- function(input, output, session) {

  displayed_sock_types <- reactiveVal(sock_type_options)

  sock_drawer <- reactiveValues()

  observeEvent(
    input$connect,
    ignoreInit = TRUE,
    {
      shinyjs::toggle(id = "ibConnectPane")
      shinyjs::toggle(id = "asdf")
    }
  )

  observeEvent(
    input$connect_sock,
    ignoreInit = TRUE,
    {
      if(isTRUE(input$sock_name %in% names(sock_drawer))){

        sendSweetAlert(
          session = session,
          title = "Socket Name already in use",
          type = "error"
        )

      } else if(
        isTRUE(
          length(
            reactiveValuesToList(sock_drawer)) + 1 >
          InteractiveTradeR:::functionary$max_client_applications
        )
      ){

        sendSweetAlert(
          session = session,
          title = "Max Sockets reached",
          type = "error"
        )

      } else {

        sock_candidate <- suppressWarnings(
          try(
            switch(
              input$sock_type,
              normal = setdiff(
                1:InteractiveTradeR:::functionary$max_client_id,
                c(
                  "0",
                  as.numeric(get_temp_info("IB_MASTER_CLIENT_ID")),
                  vapply(
                    reactiveValuesToList(sock_drawer),
                    function(x){
                      as.numeric(x[[1]]$client_id)
                    },
                    FUN.VALUE = numeric(1)
                  )
                )
              ) %>%
                sample(1) %>%
                as.character(),
              master   = {

                sock_type_options[-match("Master", names(sock_type_options))] %>%
                  displayed_sock_types()

                updatePickerInput(
                  session = session,
                  inputId = "sock_type",
                  selected = "normal",
                  choices = displayed_sock_types()
                )

                as.numeric(get_temp_info(IB_MASTER_CLIENT_ID))
              },
              tws_client_0 = "0"
            ) %>%
              ib_connect(
                port = input$port,
                host = input$host
              ) %>%
              list()
          )
        )

        if(class(sock_candidate) == "try-error"){

          sendSweetAlert(
            session,
            title = "No Connection",
            text  = "Couldn't connect to IB on port host",
            type  = "Error",
            btn_labels = "Ok",
            closeOnClickOutside = TRUE
          )

        } else {

          sock_drawer[[input$sock_name]] <- sock_candidate

          updateTextInput(
            session,
            inputId = "sock_name",
            value   = "sock_" %>%
              paste0(length(reactiveValuesToList(sock_drawer)) + 1)
          )

        }

      }

    }
  )

  output$sock_drawer_dt <- renderDT({

    req(sock_drawer)

    tibble::tibble(
      "Connect Time" = vapply(
        reactiveValuesToList(sock_drawer),
        function(x){
          as.character(x[[1]]$handshake$start_time)
        },
        FUN.VALUE = character(1)
      ),
      "Status" = "",
      "Sock Name" = names(sock_drawer),
      "Last API Call" = ""
    ) %>%
      datatable(
        options = list(
          dom = 't',
          columnDefs = list(
            list(
              className = 'dt-center', targets = 0:ncol(.)
            )
          )
        ),
        extensions = "Responsive",
        style = "bootstrap",
        class = "table-hover"
      )

  })



}

# Run the application
shinyApp(
  ui = ui,
  server = server,
  onStart = function() {
    cat("Closing all connections; startup\n")
    rm(list=ls())
    closeAllConnections()

    onStop(function() {
      cat("Closing all connections, exiting\n")
      closeAllConnections()
    })
  }
)
