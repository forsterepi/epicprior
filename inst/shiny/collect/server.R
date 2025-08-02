#' UI of Domain Expertise Collector `Shiny` app
#'
#' @noRd
server <- function(input, output, session) {
  # Stop app when closing the browser window
  session$onSessionEnded(function() {
    stopApp()
  })

  # Update inputs of lower and upper thresholds when lower >= upper
  shiny::observeEvent(input$ltinfinf, {
    if (!is.na(input$ltinfinf) & !is.na(input$utinfinf)) {
      if (input$ltinfinf >= input$utinfinf) {
        shiny::updateNumericInput(session, "utinfinf", value = input$ltinfinf + 1)
      }
    }
  })

  shiny::observeEvent(input$utinfinf, {
    if (!is.na(input$ltinfinf) & !is.na(input$utinfinf)) {
      if (input$ltinfinf >= input$utinfinf) {
        shiny::updateNumericInput(session, "ltinfinf", value = input$utinfinf - 1)
      }
    }
  })

  shiny::observeEvent(input$lt0inf, {
    if (!is.na(input$lt0inf) & !is.na(input$ut0inf)) {
      if (input$lt0inf >= input$ut0inf) {
        shiny::updateNumericInput(session, "ut0inf", value = input$lt0inf + 1)
      }
    }
  })

  shiny::observeEvent(input$ut0inf, {
    if (!is.na(input$lt0inf) & !is.na(input$ut0inf)) {
      if (input$lt0inf >= input$ut0inf) {
        shiny::updateNumericInput(session, "lt0inf", value = input$ut0inf - 0.1)
      }
    }
  })

  # Render dynamic UI (meanbeta and type01ui; rest are conditionalPanels)
  output$meanbetaui <- renderUI({
    if (input$t01[1] == 0 && input$t01[2] == 1) {
      shiny::numericInput("meanbeta",
        label = "Mean",
        value = 0.5,
        min = 0.25,
        max = 0.75,
        step = 0.05
      )
    }
  })

  output$type01ui <- renderUI({
    if (identical(input$t01[1], 0) || identical(input$t01[2], 1)) {
      value <- shiny::isolate(input$type01)

      shiny::selectInput("type01",
        label = "Select Shape",
        choices = list(
          "Beta" = 1,
          "Half-Normal" = 2
        ),
        selected = as.numeric(value)
      )
    }
  })

  # Plot
  output$distplot <- shiny::renderPlot({
    if (identical(input$range, "1")) {
      lt <- input$t01[1]
      ut <- input$t01[2]
    }
    if (identical(input$range, "2")) {
      lt <- input$lt0inf
      ut <- input$ut0inf
    }
    if (identical(input$range, "3")) {
      lt <- input$ltinfinf
      ut <- input$utinfinf
    }

    epicprior::plot_prior(
      range = input$range,
      lq = input$lq,
      uq = input$uq,
      lt = lt,
      ut = ut,
      start = list(
        shape = input$startshape,
        rate = input$startrate,
        meanlog = input$startmeanlog,
        sdlog = input$startsdlog
      ),
      type01 = input$type01 %||% "1",
      type0inf = input$type0inf %||% "1",
      meanbeta = input$meanbeta %||% 0.5
    )
  })
}
