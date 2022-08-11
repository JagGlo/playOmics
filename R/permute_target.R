permute_target <- function(models_to_eval, raw_data, n_iterations){
  results <-
    lapply(1:nrow(models_to_eval), function(n){

      # if data can be taken from mlflow, remove this part
      row <-
        models_to_eval[n,]

      vars <-
        row[[1, "model"]] %>%
        str_split(pattern = "\\s\\+\\s") %>%
        unlist()

      fields <-
        vars %>%
        match(names(raw_data))

      data <-
        data_united[, c(fields, target$target_variable)] %>%
        filter(complete.cases(.))

      parallel::parLapplyLB(cl, 0:n_iters, function(i){
        set.seed(i) # random number generator for parallel computing
        if(i != 0){
          data$status <- permuted_status
        }

        data_recipe <-
          recipe(status ~ ., data = data) %>%
          update_role(target$target_variable, new_role = "outcome")

        log_spec <- # your model specification
          logistic_reg() %>%  # model type
          set_engine(engine = "glm") %>%  # model engine
          set_mode("classification")   # model mode

        # Subsampling
        resample <- mc_cv(data, prop = 2/3, times = 30, strata = target$target_variable)

        # Define workflow
        log_wflow <- # new workflow object
          workflow() %>% # use workflow function
          add_recipe(data_recipe) %>%   # use the recipe
          add_model(log_spec)

        log_res <-
          log_wflow %>%
          fit_resamples(
            resamples = resample,
            metrics = metric_set(yardstick::mcc,
                                 recall, precision,
                                 accuracy,
                                 roc_auc, sens, spec, yardstick::ppv, yardstick::npv
            ),
            control = control_resamples(allow_par = T)
          )

        results <-
          log_res %>%
          collect_metrics(summarize = T) %>%
          select(.metric, mean) %>%
          spread(.metric, mean)

        if(i == 0){
          permuted_status <- "no permutation"
        } else {
          permuted_status = paste(permuted_status, collapse=",")
        }

        results <-
          bind_cols(
            model = paste(vars, collapse = "+"),
            iteration_no = i,
            permuted_status = permuted_status,
            results
          )

        write_csv(results, "results_permutation_wp2.csv", append = T)

        return(results)
      }) %>% bind_rows()

    })
}
