# Library ----------------------------------------------------------------------

library(tidyverse)
library(future.apply)
library(purrr)
library(here)

source(here("R/ssp_bayesian_anova.R"))
source(here("R/tpr_optim.R"))

# Functions --------------------------------------------------------------------

# Create a function to safely run Bayesian Anova
safe_ssp_anova_bf <- purrr::safely(ssp_anova_bf)

# Parallel session setup -------------------------------------------------------

# Set number of cores that we want to allocate
n_cores <- future::availableCores()
print(paste("Available cores:", n_cores))

# Make `future` plan for multisession
future::plan(multisession, workers = n_cores)

# Calculation configurations ---------------------------------------------------

# Dataframe is created with "./data-raw/options/anova_options_bayesian.R
bayes_anova_options <- readr::read_csv(here("inst/extdata/ssp_anova_options_bayesian.csv")) %>% 
  mutate(row_id = row_number())

# Set file directory -----------------------------------------------------------

ifelse(
  !dir.exists(here("data/bayes-anova-res")),
  dir.create(here("data/bayes-anova-res")),
  "Directory already exists."
)

# Run calculations -------------------------------------------------------------

# As a safety net:
# Instead of running a batch of 75 possible configurations,
n_batches <- 75

# First, we split all the iteration as list
bayes_anova_options_split <- 
  split(bayes_anova_options, bayes_anova_options$row_id)

# Since each iteration has 75 possible configuration, then we need:
n_saves <- ceiling(length(bayes_anova_options_split) / n_batches)
init <- 1

# Run iterations
for (i in 1:n_saves) {
  # Print the current iteration
  print(paste("Batch", i, "is running currently."))

  # Slice the data
  start_index <- (i - 1) * n_batches + 1
  end_index <- min(i * n_batches, length(bayes_anova_options_split))
  bayes_anova_options_sliced <- bayes_anova_options_split[start_index:end_index]

  # Calculate Bayesian ANOVA
  ssp_bayes_anova_res <- future.apply::future_lapply(bayes_anova_options_sliced,
                                                     function(x) {
                                                       # Print params of current iteration
                                                       print(paste("Running:", "tpr:", x$tpr, "effect:", x$effect, "thresh:", x$thresh, "prior:", x$prior_scale))
                                                       # Calculate sample size
                                                       results <- safe_ssp_anova_bf(
                                                         tpr = x$tpr,
                                                         effect = x$effect,
                                                         thresh = x$thresh,
                                                         prior_scale = x$prior_scale,
                                                         iter = 1000,
                                                         # fixed to 1000
                                                         max_n = 500,
                                                         # fixed to 500
                                                         mu = c(x$m11, x$m12, x$m21, x$m22),
                                                         sigma = 1
                                                       )
                                                       # Combine results with params to save
                                                       list(
                                                         parameters = list(
                                                           tpr = x$tpr,
                                                           effect = x$effect,
                                                           thresh = x$thresh,
                                                           prior_scale = x$prior_scale,
                                                           mu = c(x$m11, x$m12, x$m21, x$m22),
                                                           sigma = 1
                                                         ),
                                                         output = results
                                                       )
                                                     }, future.seed = TRUE)

  # Save the results
  saveRDS(ssp_bayes_anova_res, here(paste0("data/bayes-anova-res/set-", i, ".rds")))

  # Remove object
  rm(ssp_bayes_anova_res)

  # Empty memory
  gc()
}
