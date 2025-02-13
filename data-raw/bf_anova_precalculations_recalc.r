# Library ----------------------------------------------------------------------
library(tidyverse)
library(future.apply)
library(purrr)
library(here)

source(here("R/ssp_bayesian_anova.R"))
source(here("R/tpr_optim.R"))

load(file = here::here("data/anova_options_bayesian_recalc.rda"))
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
# Dataframe is created with "./data-raw/options/anova_options_repeat.R
bayes_anova_options <- anova_options_bayesian_recalc %>%
  mutate(row_id = row_number(),
         mu_values = map(mu, unlist)) %>%
  unnest_wider(mu_values, names_sep = "")  %>%
  rename(m11 = mu_values1, m12 = mu_values2, m21 = mu_values3, m22 = mu_values4,
         tpr = tpr_in) %>%
  select(-mu, -error_message, -result_not_null, -n1, -tpr_out)

# Set file directory -----------------------------------------------------------
ifelse(
  !dir.exists(here("data/bayes-anova-res-recalc")),
  dir.create(here("data/bayes-anova-res-recalc")),
  "Directory already exists."
)

# Run calculations -------------------------------------------------------------
# First, we split all the iteration as list
bayes_anova_options_split <- 
  split(bayes_anova_options, bayes_anova_options$row_id)

# Calculate Bayesian ANOVA
ssp_bayes_anova_res <-
  future.apply::future_lapply(bayes_anova_options_split,
                              function(x) {
                                # Print params of current iteration
                                print(
                                  paste(
                                    "Running:",
                                    "tpr:",
                                    x$tpr,
                                    "effect:",
                                    x$effect,
                                    "thresh:",
                                    x$thresh,
                                    "prior:",
                                    x$prior_scale
                                  )
                                )
                                # Calculate sample size
                                results <-
                                  safe_ssp_anova_bf(
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
                                    filename = x$filename,
                                    batch_id = x$batch_id,
                                    calculation_id = x$calculation_id,
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
saveRDS(ssp_bayes_anova_res,
        here("data/bayes-anova-res-recalc/set-recalc.rds"))

# Remove object
rm(ssp_bayes_anova_res)

# Empty memory
gc()

