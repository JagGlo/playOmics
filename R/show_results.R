results_GUI <- function(results){

library(shiny)
library(DT)
library(plotly)

shinyApp(
  ui <- fluidPage(
    DT::dataTableOutput("data"),
    DT::dataTableOutput("present_data"),
    plotlyOutput("3dplot"),
    uiOutput("predict_dashboard"),
    plotOutput("explain")
  ),

  server <- function(input, output) {

    values <- reactiveValues(df_data = NULL, model_link = NULL, prediction_data = NULL, prediction_result = NULL)

    shinyInput <- function(FUN, len, id, ...) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, i), ...))
      }
      inputs
    }

    df <- reactiveValues(data = data.frame(
      results[,c(1:14)],
      Data = shinyInput(actionButton, nrow(results), 'button_', label = "Show data", onclick = 'Shiny.onInputChange(\"present_data_btn\",  this.id)' ),
      Predict = shinyInput(actionButton, nrow(results), 'button_', label = "Make prediction", onclick = 'Shiny.onInputChange(\"predict_btn\",  this.id)' ),
      stringsAsFactors = FALSE,
      row.names = 1:nrow(results)
    ))


    output$data <- DT::renderDataTable(
      df$data, server = FALSE, escape = FALSE, selection = 'none'
    )

    observeEvent(input$present_data_btn, {
      selectedRow <- as.numeric(strsplit(input$present_data_btn, "_")[[1]][2])
      link_to_data <-  results$data[selectedRow]
      values$df_data <<- readRDS(link_to_data)
    })

    observeEvent(input$predict_btn, {
      selectedRow <- as.numeric(strsplit(input$predict_btn, "_")[[1]][2])
      link_to_data <-  results$model[selectedRow]
      values$model_link <<- link_to_data
    })

    output$present_data <- DT::renderDataTable(
      values$df_data, server = FALSE, escape = FALSE, selection = 'none'
    )

    output$`3dplot`<-renderPlotly({
      req(input$present_data_btn)
      if(length(values$df_data) == 3){
          p <-
            values$df_data %>%
            ggplot(aes_(x = as.name(names(values$df_data)[1]), y =  as.name(names(values$df_data)[2]), color = as.name(target$target_variable))) +
            geom_point() +
            theme_bw()
          ggplotly(p)

      } else {
          plotly::plot_ly(values$df_data,
                          x = as.formula(paste0("~", names(values$df_data)[1])),
                          y = as.formula(paste0("~", names(values$df_data)[2])),
                          z = as.formula(paste0("~", names(values$df_data)[3])),
                          color = as.formula(paste0("~", target$target_variable)),
                          colors = c("#0C4B8E", "#BF382A"),
                          type = "scatter3d",
                          mode = "markers"
          )
      }

    })

    output$predict_dashboard <- renderUI({
      req(input$predict_btn)
      names <- results$model_name[results$model == values$model_link]
      vars <-
        names %>%
        strsplit(split = "\\s\\+\\s") %>%
        unlist()
      tagList(
        shinydashboardPlus::box(
          "Predict",
          collapsible = TRUE,
        numericInput("input1", vars[1],""),
        numericInput("input2", vars[2],""),
        numericInput("input3", vars[3], ""),
        actionButton("predict", "Predict"),
        htmlOutput("show_prediction")
      )
      )
    })

    observeEvent(input$predict, {
      prediction_data <- data.frame(input$input1,input$input2,input$input3)
      names <- results$model_name[results$model == values$model_link]
      vars <-
        names %>%
        strsplit(split = "\\s\\+\\s") %>%
        unlist()
      colnames(prediction_data) <- vars
      values$prediction_data <- prediction_data
       mymodel <- mlflow::mlflow_load_model(values$model_link)
       predicted <- mlflow::mlflow_predict(mymodel, prediction_data)
       values$prediction_result <- predicted
    })

      output$show_prediction <-renderText({
        values$prediction_result
      })

      output$explain <- renderPlot({
        req(!is.null(values$prediction_result))
        values$explainer_link <- stringr::str_replace(values$model_link, "model", "explainer")
        myexplainer <- mlflow::mlflow_load_model(values$explainer_link)
        explained <- mlflow::mlflow_predict(myexplainer, values$prediction_data)

        explained %>%
          group_by(variable) %>%
          mutate(mean_val = mean(contribution)) %>%
          ungroup() %>%
          mutate(variable = fct_reorder(variable, abs(mean_val))) %>%
          ggplot(aes(contribution, variable, fill = mean_val > 0)) +
          geom_col(data = ~distinct(., variable, mean_val),
                   aes(mean_val, variable),
                   alpha = 0.5) +
          geom_boxplot(width = 0.5) +
          theme_bw() +
          theme(legend.position = "none") +
          labs(y = NULL)
      })
  }
)

shinyApp(ui = ui, server = server)
}
