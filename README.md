---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%"
)
```

# playOmics

<!-- badges: start -->

** playOmics: Your Go-to R Package for Handling Multi-Omics Data **

Welcome to playOmics! It's an R package we've built specifically for multi-omics data, which can be super complex to navigate. Here's what it does:

Cool Stuff It Does:

- Finds the Best Markers: Our package looks through all your data and finds the most promising markers - the ones that can help predict things like disease outcomes.

- Builds and Checks Models: It uses these markers to create models and checks how these markers work together. 

- Gets Models Ready for Real Use: Besides just analyzing, playOmics also helps to prepare your models for real-world use.

- Makes Things Easy to Understand: We know how important it is to understand what's going on. That's why playOmics offers lots of stats, visual plots, and explanations.

- Great for Small Sample Sizes: Working with a small sample size or rare diseases? No problem. Our package is designed to handle these situations really well.

Give playOmics a spin for your multi-omics data. It's user-friendly, flexible, and ready to make your life a whole lot easier. We're excited to have you onboard and welcome any feedback you may have!

<!-- badges: end -->

## Installation

### Install package

You can install the development version of playOmics from GitHub with:

``` r
remotes::install_github("JagGlo/playOmics")
```

### Run docker

playOmics can also be run from a Dockerfile available under https://github.com/JagGlo/playOmics. The dockerfile includes installation of playOmics and mlflow, which is needed to log the models. 

To run a Docker container from a Dockerfile located on a GitHub repository, you'll first need to clone the repository, build the Docker image, and then run the Docker container. Follow these steps:

Sure, if you want to only download the Dockerfile from the GitHub repository and not the entire repository, you can follow these instructions:

#### Step 1: Download the Dockerfile

You can directly download the Dockerfile using the `curl` command. You will need to use the raw GitHub URL for the Dockerfile: 

The command to download the Dockerfile would look like this:

```bash
curl -O <raw_github_dockerfile_url>
```

#### Step 2: Build the Docker image

Build the Docker image from the Dockerfile. You can replace `<image_name>` with a name of your choice.

```bash
docker build -t <image_name> .
```

This command should be run in the directory where you downloaded the Dockerfile.

#### Step 3: Run the Docker container

Finally, run the Docker container from the image you just built:

```bash
docker run -d -p 8080:8080 <image_name>
```

In this command, `-d` runs the container in detached mode (in the background), and `-p 8080:8080` maps port 8080 in the container to port 8080 on your machine. Adjust the port mapping as needed depending on the application running inside the Docker container.

Please replace `<image_name>` with the name you want to give to your Docker image.

# BRCA data example

```{r setup}
library(playOmics)
```

```{r}
# Additional libraries
library(tidyverse)
library(readxl)
```

## Data preparation

Most often each dataset is stored in the separate file (e.g. csv or xlsx) as it comes from different teams/laboratories.

We can read them separately, giving as descriptive names as possible. At this point you should also take care of unifying identifier column name, changing incorrect columns types (e.g. char to numeric), removing unwanted variables and any other required cleaning.

Each dataset should be structured in the following format: variables in the columns, observations in the rows and, obligatory, first column with observation ID (named equally for each dataset).

```{r}
clinical_data <-
  read_delim("~/code/test_data/TCGA-BRCA/Human__TCGA_BRCA__MS__Clinical__Clinical__01_28_2016__BI__Clinical__Firehose.tsi", na = c("NA", "NA,NA")) %>% 
  data.table::transpose(keep.names="ID", make.names="attrib_name") %>% 
  select(-"overallsurvival") %>% 
  mutate_at(.vars = c("years_to_birth", "overall_survival"), as.numeric)

clinical_data %>% 
  count(histological_type)
```

We will focus only on two histological types: ductal and lobular cancer and will try to distinct them, as described in <https://www.cell.com/cell/fulltext/S0092-8674(15)01195-2>:

```{r}
clinical_data <-
clinical_data %>% 
  filter(histological_type %in% c("infiltratingductalcarcinoma", "infiltratinglobularcarcinoma"))
```

Other datasets:

```{r}
proteome <-
  read_delim("~/code/test_data/TCGA-BRCA/Human__TCGA_BRCA__BI__Proteome__QExact__01_28_2016__BI__Gene__CDAP_iTRAQ_UnsharedLogRatio_r2.cct", na = c("NA", "NA,NA")) %>% 
  data.table::transpose(keep.names="ID", make.names="attrib_name") %>% 
  mutate_at(vars(-ID), as.numeric)

methylation <-
  read_delim("~/code/test_data/TCGA-BRCA/Human__TCGA_BRCA__JHU_USC__Methylation__Meth450__01_28_2016__BI__Gene__Firehose_Methylation_Prepocessor.cct", na = c("NA", "NA,NA")) %>% 
   data.table::transpose(keep.names="ID", make.names="attrib_name") %>% 
  mutate_at(vars(-ID), as.numeric)

miRNA <-
 read_delim("~/code/test_data/TCGA-BRCA/Human__TCGA_BRCA__BDGSC__miRNASeq__HS_miR__01_28_2016__BI__Gene__Firehose_RPKM_log2.cct", na = c("NA", "NA,NA")) %>% 
   data.table::transpose(keep.names="ID", make.names="attrib_name") %>% 
  mutate_at(vars(-ID), as.numeric)

mutation <-
  read_delim("~/code/test_data/TCGA-BRCA/Human__TCGA_BRCA__WUSM__Mutation__GAIIx__01_28_2016__BI__Gene__Firehose_MutSig2CV.cbt", na = c("NA", "NA,NA")) %>% 
   data.table::transpose(keep.names="ID", make.names="attrib_name") %>% 
  mutate_at(vars(-ID), as.numeric)

RNAseq <-
  read_delim("~/code/test_data/TCGA-BRCA/Human__TCGA_BRCA__UNC__RNAseq__HiSeq_RNA__01_28_2016__BI__Gene__Firehose_RSEM_log2.cct", na = c("NA", "NA,NA")) %>% 
   data.table::transpose(keep.names="ID", make.names="attrib_name") %>% 
  mutate_at(vars(-ID), as.numeric)

SCNV_log_ratio <-
  read_delim("~/code/test_data/TCGA-BRCA/Human__TCGA_BRCA__BI__SCNA__SNP_6.0__01_28_2016__BI__Gene__Firehose_GISTIC2.cct", na = c("NA", "NA,NA")) %>% 
   data.table::transpose(keep.names="ID", make.names="attrib_name") %>% 
  mutate_at(vars(-ID), as.numeric)
```

For the BRCA data (available at TCGA portal), we have 7 different datasets: clinical, RNASeq, proteome, mutation, SCNV_log_ratio, miRNA, methylation.

## Connecting dataset

In our package we used an early integration approach (concatenating all dataframe into a one structure).

However, at this point, we create a list of dataframes - this allows us to manipulate each dataframe individually and, at the same time, to take advantage of its common structure.

The function **connect_datasets()** allows to create a list of named dataframes. Each element of a list will receive a name of a dataframe. We can call an additional parameter *remove_original_data* indicates, whether the original dataframes should be removed. This is often needed as omics data can become quite heavy due to its dimension (tens to hunderds of thousands features)

```{r}
BRCA_data <- connect_datasets(clinical_data, proteome, methylation, miRNA, mutation, RNAseq, SCNV_log_ratio)

BRCA_data$clinical_data$number_of_lymph_nodes <- as.numeric(BRCA_data$clinical_data$number_of_lymph_nodes)
BRCA_data$clinical_data$Tumor_purity <- as.numeric(BRCA_data$clinical_data$Tumor_purity)

BRCA_data %>% summary()
```

When calling this function, we receive a list with 7 elements with common "ID" variable at the beggining at each dataset.

## Data coverage

While conducting omics experiment, different data might be available for different modalites due to various reason (e.g. detection limit, missing samples between laboratories, incorrect material for different type of analysis etc). Therefore it is a primary need to check data coverage between different sets.

This can be easily obtained with **plot_coverage()** function:

```{r}
plot_coverage(BRCA_data)
```

As for the BRCA data we see many combinations of data availability. The largest group (442 subjects) have complete data for 6 datasets (except proteome data).

## Check your data

To discover the data structure, one can use **data_summary()** function. It presents number of samples together with number of variables and describes the content (number of numeric/character/factor columns):

```{r}
data_summary(BRCA_data)
```

It's helpful to discover at glance whether the data have required structure. This might be especially important when reading data from text files (e.g. for proteomics experiment).

Nextly, we can explore data two functions:

-   **check_data()** will return base statistics about each numerical variable separately. It's a simple way to check for suspicious variables (e.g. low number of unique positions)

```{r}
check_data(BRCA_data$clinical_data)
```

-   **plot_density_numeric()** will draw a density plot to visualize trends in the numeric data; each

```{r}
plot_density_numeric(BRCA_data$miRNA)
```

## Check for additional effects

PCA plots

```{r}
BRCA_data$RNAseq %>%
  left_join(BRCA_data$clinical_data %>% select(ID, ethnicity)) %>% 
    plot_PCA(ethnicity, "ethnicity", "RNA data (per sample)")
```

## Quality check

This experiment should be conducted separately for each dataframe, as each omic has its own golden standards for data preprocessing.

Two functions have been implemented:

-   filter_below_threshold - user can define a numeric threshold, for which data are considered as valid in defined percentage of samples (e.g. more than 3 reads in more than 50% of samples)

```{r}
BRCA_data[["RNAseq"]] <- filter_below_threshold(data = BRCA_data[["RNAseq"]],
                                                numeric_threshold =  3, 
                                                pcent_of_samples = 0.5)
BRCA_data[["RNAseq"]]
```

After applying this filter, data reduced from 19,5k variables to 13,991 k.

-   filter missing data:

```{r}
# let's replace all zeros with NA to pretend missing data
BRCA_data[["miRNA"]][BRCA_data[["miRNA"]] == 0] <- NA
# apply filter
BRCA_data[["miRNA"]] <- filter_missing(data = BRCA_data[["miRNA"]],
                                       pcent_of_samples = 0.5)
BRCA_data[["miRNA"]]
```

Initially we had 504 columns. After filtering for non-missing values in at least of 50% columns we end up with 316 columns.

## Classification

### Define analysis target

Right now the playOmics package allows only for supervised binary classification experiment. Therefore it is crucial to define analysis target, which will be propagated to classification algorithm.

If the data is structured as described in the previous sections, then we most likely will have one dataset that contains phenotype data e.g. whether patient survived or died. With **define_target()** function we are obligated to pass a name of this dataframe (e.g. "clinical data"), a name of a column, which contains desired status and an indication of "positive" class (the one we want to predict with our analysis). One additional argument, *id_variable* indicates name of column containing samples identifiers. As discussed previously, it should be common for all of the datasets to allow data merging.

```{r}
target <-
  define_target(phenotype_df_name = "clinical_data",
              id_variable_name = "ID",
              target_variable_name = "histological_type",
              positive_class_name = "infiltratinglobularcarcinoma"
              )

target
```

### Mimic real data

**split_data_into_train_test()** function returns list of dataframe consisting of two elements: `train_data` and `test_data`. Let's use this function to split the data randomly with proportion 90%/10%. 

By passing a *target* argument to this function, we will obtain stratified split based on a target column (*target_variable_name* field in a target object):

```{r}
splitted_data <- split_data_into_train_test(BRCA_data, prop = 9/10, target = target)
```

We will leave the 10% test data as a simulation for new experiment after the modelling process is done, as a simulation of real data:

```{r}
validation_set <- splitted_data$test_data
```

We will treat the `splitted_data$train_data` as a background for next experiments, therefore let's name them "modelling_set" for now:

```{r}
modelling_set <- splitted_data$train_data
```

### Train/test data split

To prepare data for modelling, we will use already introduced function **split_data_into_train_test()**:

```{r}
BRCA_data_splitted <- 
  split_data_into_train_test(modelling_set, prop = 8/10, target = target)
```

### Prepare dataset for modelling

In next step, we want to propagate the defined target column from phenotype/clinical dataset into other datasets. This can be done using **prepare_data_for_modelling()** function. It will also prepare names to match the naming convention, transform all character and factor variables into "dummy" columns (e.g. column `sex` with two values: `male`, `female` will be transformed to two columns: `sex_male` and `sex_female` filled with 1/0 values) and will translate logical columns into numbers. Observations with missing values in the target column will be removed from data.

It will allow us to filter and model data, as described in next sections.

```{r}
data_prepared <-
    prepare_data_for_modelling(data = BRCA_data_splitted$train_data, target = target)
```

### Feature selection

OPIS DO ZMIANY!!!!

In the omics experiment it is often the case that number of features is many times larger than number of observations. Data like this are prone to overfit, as described in [Vabalas et al.](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0224365#pone-0224365-g008). Therefore, before we attempt to predict the target, we need to reduce data size. We choose filter method (apart of wrapper or embedded methods) for the sake of performance.

Vabalas and his team proved that the most overfit- and data leakage-resilient method for feature selection using filter method is nested cross-validation with inner and outer loops. This however might be computationally exhaustive as we often work with tens/hundreds of thousands features in total. Therefore we propose another method: k-fold cross-validatied ranking dedicated for feature selection.

In each fold, training data (k-1 folds) are ranked to check their convergence with target in the univariate analysis fashion. This process is repeated k times and nextly, mean rank value from all runs is calculated. Different evaluation metrics are available, as stated in [<https://mlr3filters.mlr-org.com/>]. Variables might be selected in three ways: 

- by selecting defined "top n" features, 
- by selecting % of variables from each dataset
- or by defining a cut-off threshold for metric.

What's important, at this level we are still operating on each dataset separately, which give us opportunity to preserve equal number of slots for all sets.


```{r}
data_filtered <-
    nested_filtering(
      data = data_prepared,
      target = target,
      filter_name = "auc",
      cutoff_method = "top_n",
      cutoff_treshold = 5,
      nfold = 5,
      n_threads = 5)
```


### Modelling

```{r}
new_models <- create_multiple_models("fct_after_changes_wthout_mlflow", data_filtered, BRCA_data_splitted$test_data, target, n_max = 3, n_cores = 20)
model_data <- function(data_filtered, test_data, experiment_name, n_max, resample, n_cores, n_prop, n_repeats)
```

In the current working directory, a folder is created. Experiment name must be unique.

In this step, data will finally connect into one dataframe.

```{r}
my_experiment_name <- "BRCA_experiment_new_split"

small_models <-
create_multiple_models(
  data= BRCA_data,
  experiment_name = my_experiment_name,
  n_max = 3,
  n_cores = 20)
```

### Check results

```{r}
results <- get_metrics_for_all_data(experiment_name = my_experiment_name, n_cores = 5)
```

```{r}
results %>% 
  filter(!is.na(train_mcc), n_infiltratinglobularcarcinoma > 100) %>%
  # transmute(model_name, train_mcc = as.numeric(train_mcc), test_mcc = as.numeric(test_mcc)) %>% 
  # mutate(diff = train_mcc - test_mcc) %>% 
  # arrange(desc(diff))
  results_GUI()
```

## Validation

```{r}
validation_target <-
  define_target(phenotype_df_name = "clinical_data",
              id_variable_name = "ID",
              target_variable_name = NULL,
              positive_class_name = NULL
              )

validation_data_prepared <-
    prepare_data_for_modelling(data = validation_set, target = validation_target)
```
