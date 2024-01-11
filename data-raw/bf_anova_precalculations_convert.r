# Library ----------------------------------------------------------------------

library(tidyverse)
library(future.apply)
library(purrr)
source("R/ssp_bayesian_anova.R")

# Functions --------------------------------------------------------------------

# Create a function to safely run Bayesian Anova
safe_ssp_anova_bf <- purrr::safely(ssp_anova_bf)

# Create a function to help batch iteration (LEGACY FOR LATER)
# batch_iterations <- function(df, n_batch = 100) {

#   # Check if the number of rows can be fully divided
#   if (nrow(df) %% n_batch == 0) {
#     batch_iterate <- gl(nrow(df) / 100, 100)
#   } else {
#     # If the number of rows cannot be fully divided,
#     # generate level for the first fully batch,
#     latest_complete_batch <- floor(nrow(df) / 100)
#     initial_batch <- gl(latest_complete_batch, 100)
  
#     # afterward generate level for the rest batch.
#     extra_batch <- factor(
#       rep(latest_complete_batch + 1,
#       (nrow(df) / 100 - latest_complete_batch) * 100))

#     # Combine them into one vector
#     batch_iterate <- append(initial_batch, extra_batch)
#   }

#   return(batch_iterate)

# }

# Parallel session setup -------------------------------------------------------

# Set number of cores that we want to allocate
n_cores <- future::availableCores() - 1

# Make `future` plan for multisession
future::plan(multisession, workers = n_cores)

# Calculation configurations ---------------------------------------------------

# Create a data frame storing all possible Bayesian ANOVA configuration
bayes_anova_options <-
  tidyr::expand_grid(
    m11 = 0,
    m12 = seq(0, 1, by = 0.25),
    m21 = seq(0, 1, by = 0.25),
    m22 = seq(0, 1, by = 0.25),
    tpr = seq(0.5, 0.95, by = 0.05),
    effect = c("Main Effect 1", "Main Effect 2", "Interaction Effect"),
    thresh = c(3, 6, 10),
    prior_scale = c(1 / sqrt(2), 1, sqrt(2))
  ) %>%
  dplyr::filter(m12 <= m21) %>%
  dplyr::slice(-1) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(mu = mapply(c, m11, m12, m21, m22, SIMPLIFY = FALSE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(iter = row_number())

# Set file directory -----------------------------------------------------------

ifelse(
  !dir.exists("./data/bayes-anova-res"),
  dir.create("./data/bayes-anova-res"),
  "Directory already exists."
)

# Run calculations -------------------------------------------------------------

# As a safety net:
# Instead of running a batch of 75 possible configurations,

# First, we split all the iteration as list
bayes_anova_options_split <- 
  split(bayes_anova_options, bayes_anova_options$iter)

# Since each iteration has 75 possible configuration, then we need:
n_saves <- ceiling(length(bayes_anova_options_split) / 75)
init <- 1

# Run iterations
for (i in 1:2) {
  # Print the current iteration
  print(paste("The", i, "iteration is running Currently."))

  # Slice the data
  slice_n <- i * 75
  bayes_anova_options_sliced <- bayes_anova_options_split[init:slice_n]

  # Calculate Bayesian ANOVA
  ssp_bayes_anova_res <- future.apply::future_lapply(
    bayes_anova_options_sliced,
    function(x) safe_ssp_anova_bf(
        tpr = x$tpr,
        effect = x$effect,
        thresh = x$thresh,
        prior_scale = x$prior_scale,
        iter = 1000, # fixed to 1000
        max_n = 400, # fixed to 500
        mu = c(x$m11, x$m12, x$m21, x$m22),
        sigma = 1,
      )
    )

  # Save the results
  saveRDS(ssp_bayes_anova_res, paste0("./data/bayes-anova-res/set-", i, ".rds"))

  # Remove object
  rm(ssp_bayes_anova_res)

  # Empty memory
  gc()

  # To start the next batch, we move the pointers right after the last 
  # iteration in this batch:
  init <- slice_n + 1
}