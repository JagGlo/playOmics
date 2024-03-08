results_GUI <- function(results, target) {
  ui <- fluidPage(
    column(uiOutput("choose_metrics"), width = 3),
    DT::dataTableOutput("data"),
    shinybusy::add_busy_bar(centered = T, color = "#0047ab"),
    tabsetPanel(
      id = "tabset",
      tabPanel(
        "Experiment overview",
        actionButton("add_plot", "Add new plot"),
        verticalLayout(
          div(id = "addPlaceholder"),
          shinyjs::useShinyjs(debug = TRUE)
        )
      ),
      tabPanel(
        "Analytes overview",
        DT::dataTableOutput("analytes_stats")
      ),
      tabPanel(
        "Single model overview",
        htmlOutput("this_is_single_model_overview"),
        DT::dataTableOutput("present_data"),
        uiOutput("conditionalUI"),
        plotly::plotlyOutput("plotOutput"),
        DT::dataTableOutput("variable_stats"),
        permutationUI("permutations")
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

      current_id <<- current_id + 1
    })

    output$data <- DT::renderDT({
      req(!is.null(input$select_metrics))

      df$data %>%
        select(input$select_metrics) %>%
        DT::datatable(
          extensions = c("Buttons", "Scroller"),
          caption = "Experiment results",
          selection = "single",
          # filter = "top",
          options = list(
            # searching = FALSE,
            columnDefs = list(list(className = "dt-left", targets = "_all")),
            dom = "Bfrtip",
            deferRender = TRUE,
            scroller = TRUE,
            scrollX = TRUE,
            scrollY = 300,
            scrollCollapse = TRUE,
            buttons = c("copy", "csv", "excel", "pdf", "print")
          ),
          class = "cell-border strip hover",
          escape = FALSE
        ) %>%
        DT::formatStyle(names(df$data), lineHeight = "5%")
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
            summarise_if(is.numeric, mean, na.rm = T) %>%
            rename_with(~ paste0("avg_", .x), -variable),
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

    output$this_is_single_model_overview <- renderUI({
      h3("Training data:")
    })

    observeEvent(input$present_data_btn, {
      updateTabsetPanel(
        session = getDefaultReactiveDomain(),
        "tabset",
        selected = "Single model overview"
      )
      selectedRow <- as.numeric(strsplit(input$present_data_btn, "_")[[1]][2])
      values$dir <<- results$model_dir[selectedRow]
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
            scrollY = 300,
            buttons = c("copy", "csv", "excel", "pdf", "print")
          ),
          class = "cell-border strip hover"
        )
    })

    output$variable_stats <- DT::renderDT({
      req(nrow(values$df_data) > 0)
      count_stats_per_model(values$df_data, target) %>%
        DT::datatable(
          caption = "Variable statistics across classes. Median and IQR are calculated for numeric variables, and counts for factors.",
          options = list(searching = FALSE, paging = FALSE, server = FALSE, escape = FALSE, selection = "none")
        )
    })

    output$variable_stats_plots <- renderPlot({
      lapply(1:(length(values$df_data) - 1), function(i) {
        data <- values$df_data[, c(i, length(values$df_data))]

        unique_counts <-
          data %>%
          summarise_all(n_distinct)

        fct_names <- names(unique_counts[which(unique_counts == 2)])

        data <-
          data %>%
          mutate_at(fct_names, as.factor)

        if (is.numeric(data[1])) {
          raincloud_plot(data)
        } else if (is.factor(data[[1]])) {
          ggplot(aes(x = get(names(data)[2]), y = get(names(data)[1]), fill = get(names(data)[2])), data = data)
        }
      })
    })

    output$conditionalUI <- renderUI({
      req(input$present_data_btn) # Ensure button has been clicked

      if (length(values$df_data) >= 5) {
        tagList(
          selectInput("plotColumns", "Choose Columns to Plot:",
            choices = setdiff(names(values$df_data), target$target_variable),
            selected = names(values$df_data)[c(1, 2)], # Default selection
            multiple = TRUE
          )
        )
      }
    })


    output$plotOutput <- plotly::renderPlotly({
      req(input$present_data_btn) # Ensure button has been clicked
      req(values$df_data) # Ensure the data is loaded

      if (!is.null(input$plotColumns)) {
        selected_cols <- input$plotColumns
      } else {
        selected_cols <- setdiff(names(values$df_data), target$target_variable)
      }

      req(length(selected_cols) >= 2) # Ensure at least two columns are selected

      if (length(selected_cols) == 2) {
        p <-
          values$df_data %>%
          ggplot(aes_(x = as.name(selected_cols[1]), y = as.name(selected_cols[2]), color = as.name(target$target_variable))) +
          geom_jitter(width = 0.1, height = 0.1) +
          theme_bw()
        plotly::ggplotly(p)
      } else if (length(selected_cols) == 3) {

        hover_text <- paste(
          selected_cols[1], ": ", values$df_data[[selected_cols[1]]],
          "<br>", selected_cols[2], ": ", values$df_data[[selected_cols[2]]],
          "<br>", selected_cols[3], ": ", values$df_data[[selected_cols[3]]],
          "<br>", target$target_variable, ": ", values$df_data[[target$target_variable]]
        )


        plotly::plot_ly(values$df_data,
          x = ~ jitter(values$df_data[[selected_cols[1]]]),
          y = ~ jitter(values$df_data[[selected_cols[2]]]),
          z = ~ jitter(values$df_data[[selected_cols[3]]]),
          text = hover_text, # Custom hover text
          hoverinfo = "text", # Display only the custom text
          color = as.formula(paste0("~", target$target_variable)),
          colors = c("#0C4B8E", "#BF382A"),
          type = "scatter3d",
          mode = "markers"
        ) %>%
          plotly::layout(scene = list(
            xaxis = list(title = selected_cols[1]),
            yaxis = list(title = selected_cols[2]),
            zaxis = list(title = selected_cols[3])
          ))
      } else {
        # print error
        print("Too many columns selected for 2D or 3D plot. Please select 2 or 3 columns.")
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

      predictionServer("new_data", results, values, target)
    })
  }

  shinyApp(ui = ui, server = server)
}
