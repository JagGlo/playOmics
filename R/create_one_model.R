
create_one_model <- function(data, target,  split_proportion = 0.8, n_repeats = 50){
  browser()
  data_united <- data %>% reduce(full_join, by = c(target$target_variable, target$id_variable)) %>% na.omit()

  set.seed(123)
  ames_split <- initial_split(data_united, prop = 0.80, strata = target$target_variable)
  ames_train <- training(ames_split)
  ames_test  <-  testing(ames_split)

  ames_rec <-
    data_united %>%
    recipe(status ~ .) %>%
    step_center(all_numeric()) %>%
    step_scale(all_numeric())

  rf_mod <-
    rand_forest(trees = 1000) %>%
    set_engine("ranger") %>%
    set_mode("classification")

  rf_wflow <-
    workflow() %>%
    add_model(rf_mod) %>%
    add_recipe(ames_rec)

  rf_fit <- fit(rf_wflow, ames_train)

  rf_wflow <-
    workflow() %>%
    add_formula(
      status ~ .) %>%
    add_model(rf_mod)

  set.seed(55)
  ames_folds <- mc_cv(ames_train, prop = split_proportion, times = n_repeats, strata = target$target_variable)

  keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

  set.seed(130)
  rf_res <- rf_wflow %>% fit_resamples(resamples = ames_folds, control = keep_pred)

  rf_res$.extracts[[1]][[1]]

  rf_testing_pred <- collect_metrics(rf_res)

  rf_testing_pred %>%                   # test set predictions
    accuracy(truth = class, .pred_class)


}
