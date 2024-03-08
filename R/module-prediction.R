predictionUI <- function(id) {
  ns <- NS(id)

  tagList(
    shiny::tags$div(
      fluidPage(
        shiny::radioButtons(ns("select_input"),
          label = "Select data for prediction",
          choices = c("Insert values", "Select data from your env"),
          selected = "Insert values"
        ),
        uiOutput(ns("predict_dashboard")),
        htmlOutput(ns("this_is_prediction")),
        uiOutput(ns("error_message")),
        DT::dataTableOutput(ns("show_prediction")),
        htmlOutput(ns("this_is_explanation")),
        uiOutput(ns("choose_ID_to_explain")),
        plotOutput(ns("explain"))
      )
    )
  )
}

predictionServer <- function(id, df, values, target) {
  moduleServer(id, function(input, output, session) {

    # A reactive value to store error messages
    error_message <- reactiveVal(NULL)

    dynamicUI <- reactive({
      # tagList(
      #   fluidPage(
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
      #   ),
      #   actionButton(session$ns("predict"), "Predict")
      # )
    })

    output$predict_dashboard <- renderUI({
      tagList(
        fluidPage(
          dynamicUI(),  # Use the reactive expression for dynamic UI components
          actionButton(session$ns("predict"), "Predict")
        )
      )
    })

    observeEvent(input$predict, {

      names <- na.omit(results$model_name[results$model_dir == values$model_link])
      vars <-
        names %>%
        strsplit(split = "\\s\\+\\s") %>%
        unlist()

      if (input$select_input == "Insert values") {
        # Extract list of data frames from reactiveValues
        list_of_inputs <- reactiveValuesToList(input)

        # Filter list by keys
        filtered_list <- list_of_inputs[names(list_of_inputs) %in% grep("input\\d", names(list_of_inputs), value = TRUE)]

        # If you need to bind them together into one data frame (assuming they have the same structure):
        new_data <- bind_rows(filtered_list) %>% select(order(colnames(.)))

        colnames(new_data) <- vars
      } else if (input$select_input == "Select data from your env") {
        data_from_env <- get(input$env_dataframe, envir = .GlobalEnv)
        new_data <-
          if (!is.data.frame(data_from_env)) {
            tryCatch(
              {
                data_from_env %>%
                reduce(full_join, by = target$id_variable) %>%
                select(target$id_variable, vars)
              },
              error = function(e) {
                values$prediction_data <- NULL
                # Update the reactive value with the error message
                # check if "vars" exist in the data_from_env
                if(!all(vars %in% names(data_from_env))){
                  error_message("The selected dataframe or list does not contain the required variables.")
                } else {
                  error_message(e)
                }
                NULL
              }
            )

          } else {
            data_from_env
          }
      }

      if(all(setdiff(names(new_data), names(new_data) != "") == vars)){
        values$prediction_data <- new_data[!is.na(names(new_data))]
      } else {
        values$prediction_data <- NULL
      }

      lapply(1:length(vars), function(i) {
            updateNumericInput(session, session$ns(paste0("input", i)), value = NULL)
        })

    })

    output$show_prediction <- DT::renderDT({

      req(!is.null(values$prediction_data))

      predicted <- load_and_predict(values$model_link, values$prediction_data)


      if(!is.null(predicted) & nrow(predicted) > 1) {
        values$prediction_result <- bind_cols(values$prediction_data[,1],predicted)
      } else {
        values$prediction_result <- predicted
      }
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

    # Render the error message if there is one
    output$error_message <- renderUI({
      if (!is.null(error_message())) {
        shiny::tags$div(style = "color: red;", error_message())
      }
    })

    output$this_is_explanation <- renderUI({
      req(!is.null(values$prediction_result))
      h3("Explanation with SHAP values:")
    })

    #if nrow(values$prediction_data) > 1, select one row for explanation with selector
    output$choose_ID_to_explain <- renderUI({
      req(!is.null(values$prediction_data))
      req(all(names(values$prediction_data) == setdiff(names(values$df_data), target$target_variable)))
      if (nrow(values$prediction_result) > 1) {
        selectInput(
          session$ns("explanation_id"),
          "Select ID to explain:",
          choices = na.omit(values$prediction_data)[[target$id_variable]],
          selected = na.omit(values$prediction_data)[[target$id_variable]][1]
        )
      }
    })

    output$explain <- renderPlot({
      req(!is.null(values$prediction_data))
      req(all(names(values$prediction_data) == setdiff(names(values$df_data), target$target_variable)))
      req(is.null(error_message()))

      if (nrow(values$prediction_result) > 1) {
        req(input$explanation_id)
        data_to_predict <-
          values$prediction_data[values$prediction_data[[target$id_variable]] == input$explanation_id, ] %>%
          select(-target$id_variable)
      } else {
        data_to_predict <- values$prediction_data
      }

      tryCatch({
        explained <- load_and_explain(values$model_link, data_to_predict)
      },
      error = function(e) {
        values$prediction_data <- NULL
        error_message(e)
      }
      )

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
        labs(y = NULL) +
        theme(legend.position = "top", text=element_text(size=20,  family="Helvetica"),
              plot.title = element_text(size=20),
              axis.text = element_text(size=20, colour = "black"),
              strip.text.x = element_text(size = 20))

    })
  })
}
