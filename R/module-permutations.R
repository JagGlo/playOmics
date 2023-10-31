permutationUI <- function(id, data) {
  ns <- NS(id)

  tagList(
    shiny::tags$div(
      fluidPage(
        htmlOutput(ns("this_is_permutations")),
        numericInput(
          ns("no_of_permutations"),
          "Number of permutations:",
          value = 100,
          min = 1,
          max = 1000,
          step = 1
        ),
        actionButton(ns("permute"), "Permute"),
        uiOutput(ns("see_perm_results")),
        plotOutput(ns("see_results")),
        DT::dataTableOutput(ns("results_tab")),
        DT::dataTableOutput(ns("detailed_results_tab"))
      )
    )
  )
}


permutationServer <- function(id, link_to_folder, target) {
  moduleServer(id, function(input, output, session) {
    output$this_is_permutations <- renderUI({
      h3("Validate with permutations:")
    })

    perm_results <- reactive({
      req(input$permute)

      no_of_perm <- as.numeric(input$no_of_permutations)
      train_data <- readRDS(paste(link_to_folder, "train_data.Rds", sep = "/"))
      test_data <- readRDS(paste(link_to_folder, "test_data.Rds", sep = "/"))
      data <- bind_rows(train_data, test_data)
      # read params
      params_path <- paste(link_to_folder, "params.json", sep = "/")
      params <- jsonlite::fromJSON(params_path, simplifyVector = FALSE)

      permutation_results <-
        lapply(1:no_of_perm, function(i) {
          # Shuffle the target variable in train and test data
          data_perm <- data %>% mutate(!!target$target_variable := sample(!!sym(target$target_variable)))
          data_splitted <- rsample::initial_split(data_perm, prop = 0.8, strata = target$target_variable)

          # Create a model with shuffled data
          model_result <-
            create_model(rsample::training(data_splitted),
              rsample::testing(data_splitted),
              target,
              log_experiment = FALSE,
              explain = FALSE,
              validation_method = params$validation_method,
              n_prop = params$prop,
              n_repeats = params$times
            )

          model_result
        }) %>%
        bind_rows(.id = "permutation_no")
    })

    original_model_result <- reactive(jsonlite::fromJSON(paste(link_to_folder, "metrics.json", sep = "/"), simplifyVector = T) %>% data.frame())

    pvalues <- reactive({
      req(!is.null(perm_results()))

      # Calculate p-values to evaluate the significance of the mode
      pval <- rowSums(apply(perm_results(), 1, function(x) {
        select(original_model_result(), !starts_with("model")) > x[-c(1:2)]
      }, simplify = T)) / nrow(perm_results())

      # Prepare p-values for merging with the model results
      names(pval) <- names(select(original_model_result(), !starts_with("model")))
      # Convert p-values to data frame
      pval <- as.data.frame(t(pval)) %>% rename_with(~ paste0("perm_pval_", .x))

      pval
    })

    output$see_perm_results <- renderUI({
      req(!is.null(perm_results()))
      h3("Permutation results:")
      # insert selectInput
      shiny::selectInput(session$ns("select_input"),
        label = "Select metric to present",
        choices = names(perm_results()),
        selected = "train_roc_auc"
      )
    })

    output$see_results <- renderPlot({
      req(!is.null(perm_results()))
      req(input$select_input)
      metric <- as.name(input$select_input)
      perm_results() %>%
        ggplot(aes(x = !!metric)) +
        geom_density() +
        geom_vline(xintercept = original_model_result()[[metric]], linetype = "dashed") +
        theme_bw()
    })

    output$results_tab <- DT::renderDataTable({
      req(!is.null(pvalues()))
      pvalues() %>%
        mutate_all(round, 3) %>%
        select(-contains("_n_")) %>%
        DT::datatable(
          rownames = FALSE,
          options = list(searching = FALSE, paging = FALSE),
          escape = FALSE,
          selection = "none"
        )
    })

    output$detailed_results_tab <- DT::renderDataTable({
      req(!is.null(perm_results()))
      perm_results() %>%
        mutate_all(round, 3) %>%
        DT::datatable(
          rownames = FALSE,
          options = list(searching = FALSE, paging = FALSE),
          escape = FALSE,
          selection = "none"
        )
    })
  })
}
