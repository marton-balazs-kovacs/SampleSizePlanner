# Library ----------------------------------------------------------------------

library(tidyverse)
library(future.apply)
library(purrr)
library(here)

source(here("R/ssp_eq_anova.R"))
source(here("R/tpr_optim.R"))
# Functions --------------------------------------------------------------------

# Create a function to safely run Bayesian Anova
safe_ssp_anova_eq <- purrr::safely(ssp_anova_eq)

# Parallel session setup -------------------------------------------------------

# Set number of cores that we want to allocate
n_cores <- future::availableCores() - 1
print(paste("Available cores:", n_cores))

# Make `future` plan for multisession
future::plan(multisession, workers = n_cores)

# Calculation configurations ---------------------------------------------------

# Dataframe is created with "./data-raw/options/anova_options_eq.R
eq_anova_options <- readr::read_csv(here("inst/extdata/ssp_anova_options_eq.csv")) %>% 
  mutate(row_id = row_number())

# Set file directory -----------------------------------------------------------

ifelse(
  !dir.exists(here("data/eq-anova-res")),
  dir.create(here("data/eq-anova-res")),
  "Directory already exists."
)

# Run calculations -------------------------------------------------------------

# As a safety net:
# Instead of running a batch of 50 possible configurations
n_batches <- 50

# First, we split all the iteration as list
eq_anova_options_split <- 
  split(eq_anova_options, eq_anova_options$row_id)

# Since each iteration has 75 possible configuration, then we need:
n_saves <- ceiling(length(eq_anova_options_split) / n_batches)
init <- 1

# Run iterations
for (i in 1:n_saves) {
  # Print the current iteration
  print(paste("Batch", i, "is running currently."))

  # Slice the data
  start_index <- (i - 1) * n_batches + 1
  end_index <- min(i * n_batches, length(eq_anova_options_split))
  eq_anova_options_sliced <- eq_anova_options_split[start_index:end_index]

  # Calculate Bayesian ANOVA
  ssp_eq_anova_res <- future.apply::future_lapply(eq_anova_options_sliced,
                                                     function(x) {
                                                       # Print params of current iteration
                                                       print(paste("Running:", "tpr:", x$tpr, "effect:", x$effect, "thresh:", x$thresh, "prior:", x$prior_scale, "eq_band:", x$eq_band))
                                                       # Calculate sample size
                                                       results <- safe_ssp_anova_eq(
                                                         tpr = x$tpr,
                                                         effect = x$effect,
                                                         thresh = x$thresh,
                                                         prior_scale = x$prior_scale,
                                                         eq_band = x$eq_band,
                                                         mu = c(x$m11, x$m12, x$m21, x$m22),
                                                         sigma = 1,
                                                         iter = 1000,
                                                         post_iter = 1000,
                                                         max_n = 500,
                                                         prior_location = 0
                                                       )
                                                       # Combine results with params to save
                                                       list(
                                                         parameters = list(
                                                           tpr = x$tpr,
                                                           effect = x$effect,
                                                           thresh = x$thresh,
                                                           prior_scale = x$prior_scale,
                                                           eq_band = x$eq_band,
                                                           mu = c(x$m11, x$m12, x$m21, x$m22),
                                                           sigma = 1,
                                                           iter = 1000,
                                                           post_iter = 1000,
                                                           max_n = 500,
                                                           prior_location = 0,
                                                         ),
                                                         output = results
                                                       )
                                                     })

  # Save the results
  saveRDS(ssp_eq_anova_res, here(paste0("data/eq-anova-res/set-", i, ".rds")))

  # Remove object
  rm(ssp_eq_anova_res)

  # Empty memory
  gc()
}
