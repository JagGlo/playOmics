library(shiny)

results_GUI <- function(results, target) {
  ui <- fluidPage(
    column(uiOutput("choose_metrics"), width = 3),
    DT::dataTableOutput("data"),
    tabsetPanel(
      id = "tabset",
      tabPanel(
        "Experiment overview",
        actionButton("add_plot", "Add new plot"),
        verticalLayout(
          div(id = "addPlaceholder"),
          shinyjs::useShinyjs(debug = TRUE),
        )
      ),
      tabPanel(
        "Analytes overview",
        DT::dataTableOutput("analytes_stats")
      ),
      tabPanel(
        "Single model overview",
        DT::dataTableOutput("present_data"),
        plotly::plotlyOutput("3dplot"),
        DT::dataTableOutput("variable_stats"),
        permutationUI("permutations"),
        plotOutput("variable_stats_plots")
      ),
      tabPanel(
        "Prediction",
        predictionUI("new_data")
      )
      # tabPanel("Model ensembling"
      #
      # )
    )
  )


  server <- function(input, output, session) {
    values <- reactiveValues(dir = NULL, df_data = NULL, model_link = NULL, prediction_data = NULL, prediction_result = NULL)

    shinyInput <- function(FUN, len, id, ...) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, i), ...))
      }
      inputs
    }

    df <- reactiveValues(
      data = data.frame(
        results %>% mutate_if(is.numeric, round, 3),
        Data = shinyInput(actionButton, nrow(results), "button_", label = "Show data", onclick = 'Shiny.onInputChange(\"present_data_btn\",  this.id)'),
        Predict = shinyInput(actionButton, nrow(results), "button_", label = "Make prediction", onclick = 'Shiny.onInputChange(\"predict_btn\",  this.id)'),
        stringsAsFactors = FALSE,
        row.names = 1:nrow(results)
      ) %>%
        relocate(c(Data, Predict), .after = model_name) %>%
        relocate(c(model_id, model_dir), .after = last_col())
    )

    output$choose_metrics <- renderUI({
      req(nrow(df$data) > 0)
      shinyWidgets::pickerInput(
        inputId = "select_metrics",
        label = "Choose column to present:",
        multiple = TRUE,
        selected = names(df$data),
        choices = names(df$data),
        options = list(`actions-box` = TRUE)
      )
    })

    # plotting module
    list_modules <- list()
    current_id <- 1 # current module id

    # dynamically adding new fields
    observeEvent(input$add_plot, {
      id <- paste0("plot_", current_id)

      insertUI(
        selector = "#addPlaceholder",
        where = "beforeEnd",
        ui = plottingUI(session$ns(id),
          box_id = current_id,
          data = results %>% select(-c(model_name, model_id, model_dir))
        )
      )

      plottingServer(id, results %>% select(-c(model_name, model_id, model_dir)))

      observeEvent(input[[paste0(id, "-box")]][["visible"]], {
        if (input[[paste0(id, "-box")]][["visible"]] == FALSE) {
          removeUI(selector = paste0("#", id))
          list_modules[[id]] <<- NULL
          remove_shiny_inputs(paste0(id, "-box"), input)
        }
      })

      current_id <<- current_id + 1
    })

    output$data <- DT::renderDT({
      req(!is.null(input$select_metrics))

      df$data %>%
        select(input$select_metrics) %>%
        DT::datatable(
          extensions = c("Buttons", "Scroller"),
          # filter = "top",
          options = list(
            # searching = FALSE,
            columnDefs = list(list(className = "dt-left", targets = "_all")),
            dom = "Bfrtip",
            deferRender = TRUE,
            scroller = TRUE,
            scrollX = TRUE,
            scrollY = 400,
            buttons = c("copy", "csv", "excel", "pdf", "print")
          ),
          class = "cell-border strip hover",
          escape = FALSE
        )
    })

    output$analytes_stats <- DT::renderDT({
      vars <- lapply(1:nrow(df$data), function(i) {
        vars <-
          df$data[i, "model_name"] %>%
          strsplit(split = "\\s\\+\\s") %>%
          unlist() %>%
          as_tibble() %>%
          rename(variable = value)

        bind_cols(
          model_name = df$data[i, "model_name"],
          vars
        )
      }) %>%
        bind_rows()


      vars %>%
        left_join(df$data, by = "model_name") %>%
        group_by(variable) %>%
        summarise(appears_in_n_models = n()) %>%
        left_join(
          vars %>%
            left_join(df$data, by = "model_name") %>%
            group_by(variable) %>%
            summarise_if(is.numeric, mean, na.rm = T),
          by = "variable"
        ) %>%
        mutate_if(is.numeric, round, 3) %>%
        DT::datatable(
          extensions = c("Buttons", "Scroller"),
          # filter = "top",
          options = list(
            columnDefs = list(list(className = "dt-left", targets = "_all")),
            dom = "Bfrtip",
            deferRender = TRUE,
            scroller = TRUE,
            scrollX = TRUE,
            scrollY = 400,
            buttons = c("copy", "csv", "excel", "pdf", "print")
          ),
          class = "cell-border strip hover",
          escape = FALSE
        )
    })


    observeEvent(input$present_data_btn, {
      updateTabsetPanel(
        session = getDefaultReactiveDomain(),
        "tabset",
        selected = "Single model overview"
      )

      selectedRow <- as.numeric(strsplit(input$present_data_btn, "_")[[1]][2])
      values$dir <- results$model_dir[selectedRow]
      link_to_data <- paste(values$dir, "train_data.Rds", sep = "/")
      values$df_data <<- readRDS(link_to_data)
      permutationServer("permutations", values$dir, target)
    })

    output$present_data <- DT::renderDT({
      req(nrow(values$df_data) > 0)
      values$df_data %>%
        DT::datatable(
          extensions = c("Buttons", "Scroller"),
          options = list(
            searching = FALSE,
            columnDefs = list(list(className = "dt-left", targets = "_all")),
            dom = "Bfrtip",
            deferRender = TRUE,
            scroller = TRUE,
            scrollX = TRUE,
            scrollY = 400,
            buttons = c("copy", "csv", "excel", "pdf", "print")
          ),
          class = "cell-border strip hover"
        )
    })

    output$variable_stats <- DT::renderDT({
      req(nrow(values$df_data) > 0)
      count_stats_per_model(values$df_data, target) %>%
        DT::datatable(
          options = list(searching = FALSE, paging = FALSE, server = FALSE, escape = FALSE, selection = "none")
        )
    })

    output$variable_stats_plots <- renderPlot({
      lapply(1:(length(values$df_data)-1), function(i){

       data <- values$df_data[,c(i, length(values$df_data))]

       unique_counts <-
         data %>%
         summarise_all(n_distinct)

       fct_names <- names(unique_counts[which(unique_counts == 2)])

       data <-
         data %>%
         mutate_at(fct_names, as.factor)

       if(is.numeric(data[1])){
         raincloud_plot(data)
       } else if(is.factor(data[[1]])){
         ggplot(aes(x = get(names(data)[2]), y = get(names(data)[1]), fill = get(names(data)[2])), data = data)
       }

      })

    })

    output$`3dplot` <- plotly::renderPlotly({
      req(input$present_data_btn)
      if (length(values$df_data) == 3) {
        p <-
          values$df_data %>%
          ggplot(aes_(x = as.name(names(values$df_data)[1]), y = as.name(names(values$df_data)[2]), color = as.name(target$target_variable))) +
          geom_jitter(width = 0.1, height = 0.1) +
          theme_bw()
        plotly::ggplotly(p)
      } else {
        plotly::plot_ly(values$df_data,
          x = as.formula(paste0("~\u0060", names(values$df_data)[1], "\u0060")),
          y = as.formula(paste0("~\u0060", names(values$df_data)[2], "\u0060")),
          z = as.formula(paste0("~\u0060", names(values$df_data)[3], "\u0060")),
          color = as.formula(paste0("~", target$target_variable)),
          colors = c("#0C4B8E", "#BF382A"),
          type = "scatter3d",
          mode = "markers"
        )
      }
    })

    observeEvent(input$predict_btn, {
      updateTabsetPanel(
        session = getDefaultReactiveDomain(),
        "tabset",
        selected = "Prediction"
      )

      selectedRow <- as.numeric(strsplit(input$predict_btn, "_")[[1]][2])
      link_to_model <- results$model_dir[selectedRow]
      values$model_link <<- link_to_model

      predictionServer("new_data", results, values)
    })
  }

  shinyApp(ui = ui, server = server)
}
