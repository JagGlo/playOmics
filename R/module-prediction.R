predictionUI <- function(id) {
  ns <- NS(id)

  tagList(
    shiny::tags$div(
      fluidPage(
        shiny::radioButtons(ns("select_input"),
          label = "Select data for prediction",
          choices = c("Insert values", "Select data from your env", "Load data from file"),
          selected = "Insert values"
        ),
        uiOutput(ns("predict_dashboard")),
        htmlOutput(ns("this_is_prediction")),
        DT::dataTableOutput(ns("show_prediction")),
        htmlOutput(ns("this_is_explanation")),
        plotOutput(ns("explain"))
      )
    )
  )
}

predictionServer <- function(id, df, values) {
  moduleServer(id, function(input, output, session) {
    output$predict_dashboard <- renderUI({
      tagList(
        fluidPage(
          if (input$select_input == "Insert values") {
            names <- na.omit(results$model_name[results$model_dir == values$model_link])
            vars <-
              names %>%
              strsplit(split = "\\s\\+\\s") %>%
              unlist()
            lapply(1:length(vars), function(i) {
              numericInput(session$ns(paste0("input", i)), vars[i], "")
            })
          } else if (input$select_input == "Select data from your env") {
            dataframes <- ls(envir = .GlobalEnv)[sapply(mget(ls(envir = .GlobalEnv), envir = .GlobalEnv), is.data.frame) | sapply(mget(ls(envir = .GlobalEnv), envir = .GlobalEnv), is.list)]
            selectInput(session$ns("env_dataframe"), "Select a dataframe or a list:", dataframes)
          }
        ),
        actionButton(session$ns("predict"), "Predict")
      )
    })

    observeEvent(input$predict, {
      if (input$select_input == "Insert values") {
        # Extract list of data frames from reactiveValues
        list_of_inputs <- reactiveValuesToList(input)

        # Filter list by keys
        filtered_list <- list_of_inputs[names(list_of_inputs) %in% grep("input\\d", names(list_of_inputs), value = TRUE)]

        # If you need to bind them together into one data frame (assuming they have the same structure):
        new_data <- bind_rows(filtered_list) %>% select(order(colnames(.)))

        names <- na.omit(results$model_name[results$model_dir == values$model_link])
        vars <-
          names %>%
          strsplit(split = "\\s\\+\\s") %>%
          unlist()
        colnames(new_data) <- vars
      } else if (input$select_input == "Select data from your env") {
        data_from_env <- get(input$env_dataframe, envir = .GlobalEnv)
        new_data <-
          if (!is.data.frame(data_from_env)) {
            prepare_data_for_modelling(data_from_env)
          } else {
            data_from_env
          }
      }
      values$prediction_data <- new_data
    })

    output$show_prediction <- DT::renderDT({
      req(!is.null(values$prediction_data))
      predicted <- load_and_predict(values$model_link, values$prediction_data)
      values$prediction_result <- predicted
      values$prediction_result %>%
        mutate_if(is.numeric, round, 3) %>%
        DT::datatable(
          rownames = FALSE,
          options = list(searching = FALSE, paging = FALSE, server = FALSE, escape = FALSE, selection = "none")
        )
    })

    output$this_is_prediction <- renderUI({
      req(!is.null(values$prediction_data))
      h3("Prediction result(s):")
    })

    output$this_is_explanation <- renderUI({
      req(!is.null(values$prediction_result))
      h3("Explanation with SHAP values:")
    })

    output$explain <- renderPlot({
      req(!is.null(values$prediction_result))
      explained <- load_and_explain(values$model_link, values$prediction_data)

      explained %>%
        group_by(variable) %>%
        mutate(mean_val = mean(contribution)) %>%
        ungroup() %>%
        mutate(variable = fct_reorder(variable, abs(mean_val))) %>%
        ggplot(aes(contribution, variable, fill = mean_val > 0)) +
        geom_col(
          data = ~ distinct(., variable, mean_val),
          aes(mean_val, variable),
          alpha = 0.5
        ) +
        geom_boxplot(width = 0.5) +
        theme_bw() +
        theme(legend.position = "none", axis.text = element_text(size = 12)) +
        labs(y = NULL)
    })
  })
}
