% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_multiple_models.R
\name{create_multiple_models}
\alias{create_multiple_models}
\title{Create multiple models for given datasets}
\usage{
create_multiple_models(
  experiment_name,
  train_data,
  test_data,
  target,
  n_min = 2,
  n_max = 3,
  trim_models = TRUE,
  trim_metric = "train_mcc",
  trim_threshold = 0.3,
  validation_method = "cv",
  n_prop = 2/3,
  n_repeats = 5,
  log_experiment = TRUE,
  explain = TRUE,
  add_weights = FALSE,
  n_cores = parallel::detectCores()/4,
  directory = getwd()
)
}
\arguments{
\item{experiment_name}{A character string denoting the name of the experiment.}

\item{train_data}{A list containing the training data.}

\item{test_data}{A list containing the testing data.}

\item{target}{A list with at least two elements: 'target_variable' (name of the dependent variable) and 'id_variable' (name of the identifier variable); see more under \link[playOmics]{define_target}.}

\item{n_max}{An integer specifying the maximum number of predictor variables to consider in combinations. Default is 3.}

\item{trim_models}{A logical indicating whether to trim models based on a given metric and threshold. Default is TRUE.}

\item{trim_metric}{A character string specifying the metric to use for trimming models. Default is 'train_mcc'.}

\item{trim_threshold}{A numeric value specifying the threshold below which models should be trimmed. Default is 0.3.}

\item{validation_method}{A character string specifying the validation method to be used; either "subsampling" or "cv"; see more under \link[playOmics]{create_model}.}

\item{n_prop}{A numeric value representing the proportion of data used for each resample in the subsampling process (default: 2/3); see more under \link[playOmics]{create_model}.}

\item{n_repeats}{A numeric value specifying the number of times to repeat the validation (default: 10); see more under \link[playOmics]{create_model}.}

\item{log_experiment}{A logical value indicating whether to log the experiment details and results. Default is TRUE; see more under \link[playOmics]{create_model}.}

\item{explain}{A logical value indicating whether to create model's explainer using DALEX (default: TRUE);see more under \link[playOmics]{create_model}.}

\item{n_cores}{An integer specifying the number of CPU cores to use in parallel processing. Default is one fourth of the available cores.}

\item{directory}{A character string specifying the path to the working directory. Default is the current working directory.}
}
\value{
A list of results from the modeling process.
}
\description{
This function iterates over combinations of predictor variables in the train data to build
multiple models.
}
\details{
When you dive into using the \emph{create_multiple_models()} function, you're not just making one model — you're orchestrating a symphony of models,
each one trying its best to shed light on your data. Among these models, however, not all are equal. Some might not capture
the patterns in your data well, and this is where the trim_models, trim_metric, and trim_threshold parameters can be handy.

\itemize{
  \item \strong{trim_models}: This parameter acts as a switch or flag.
If set to TRUE, the function will employ the mechanism to remove or "trim" models based on the specified metric and threshold (detailed below). If set to FALSE, all models will be kept, regardless of their performance.
 \item \strong{trim_metric}: This specifies the metric used to evaluate model performance.
In the provided function, "train_mcc" (which likely stands for Matthews Correlation Coefficient on the training data) is the default metric. However, users can potentially specify other metrics if they want models to be evaluated and potentially trimmed based on different performance criteria.
\item \strong{trim_threshold}: This is the critical value, based on which models will be evaluated for potential trimming.
If the performance metric of a model (as specified by trim_metric) is below this threshold, and trim_models is set to TRUE, that model will be removed or "trimmed."
The default value provided is 0.3. So, for instance, with default settings, any model with a "train_mcc" less than 0.3 would be removed.
}
After constructing "n"-variables models for each variable combination, the function checks if the metric (e.g., "train_mcc") for the component of this model in the "n-1"-variable run is
below the specified threshold. If it is and if trim_models is TRUE, those underperforming models are removed. This ensures that subsequent
combinations of variables don't waste time considering models that are deemed unsatisfactory based on prior results.
}
\examples{
# Assuming appropriate data is available
\dontrun{
# results <-
create_multiple_models(
  experiment_name = "my_experiment_name",
  train_data = train_data_prepared,
  test_data = test_data_prepared,
  target = my_target,
  n_max = 3,
  trim_models = TRUE,
  trim_metric = "train_mcc",
  trim_threshold = 0.3,
  # single model settings
  validation_method = "cv",
  n_prop = NULL,
  n_repeats = 5,
  log_experiment = TRUE,
  explain = TRUE,
  # configuration
  n_cores = 5,
  directory = getwd()
)
}
}
