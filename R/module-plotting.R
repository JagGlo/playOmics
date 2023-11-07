plottingUI <- function(id, box_id, data) {
  ns <- NS(id)

  tagList(
    shiny::tags$div(
      id = paste0("plot_", box_id),
      shinydashboardPlus::box(
        id = ns("box"),
        width = 4,
        title = paste0("Plot #", box_id),
        fluidRow(
          h3(),
          column(width = 8, selectInput(ns("metric"), "Select metric:", choices = c("", names(data)))),
          h2(),
          column(width = 3, actionButton(ns("draw"), "Draw plot"))
        ),
        plotOutput(ns("plot")),
        DT::dataTableOutput(ns("stats"))
      )
    )
  )
}

plottingServer <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$draw, {
      output$plot <- renderPlot({
        req(input$metric)
        metric <- as.name(input$metric)
        ggplot(df, aes(x = !!metric)) +
          # ggplot(df, aes(x = train_mcc)) +
          geom_histogram(aes(y = after_stat(density)),
            alpha = 0.5,
            position = "identity"
          ) +
          geom_density(alpha = .2) +
          theme_bw()
      })

      output$stats <- DT::renderDataTable({
        req(input$metric)
        df %>%
          select(input$metric) %>%
          rename(value = 1) %>%
          summarise(
            min = min(value, na.rm = T),
            median = median(value, na.rm = T),
            mean = mean(value, na.rm = T),
            sd = sd(value, na.rm = T),
            max = max(value, na.rm = T)
          ) %>%
          mutate_all(round, 3) %>%
          DT::datatable(
            rownames = FALSE,
            options = list(searching = FALSE, paging = FALSE),
            escape = FALSE,
            selection = "none"
          )
      })
    })
  })
}
