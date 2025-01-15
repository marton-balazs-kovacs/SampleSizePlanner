# Library ----------------------------------------------------------------------
library(tidyverse)
library(future.apply)
library(purrr)
library(here)
library(assertthat)

source(here("R/ssp_eq_anova.R"))
source(here("R/tpr_optim.R"))
source(here("R/ssp_tost.R"))
source(here("R/assertions.R"))

load(file = here::here("data/anova_options_eq_recalc.rda"))
# Functions --------------------------------------------------------------------
# Create a function to safely run Bayesian Anova
safe_ssp_anova_eq <- purrr::safely(ssp_anova_eq)

# Parallel session setup -------------------------------------------------------
# Set number of cores that we want to allocate
n_cores <- future::availableCores()
print(paste("Available cores:", n_cores))

# Make `future` plan for multisession
future::plan(multisession, workers = n_cores)

# Calculation configurations ---------------------------------------------------

# Dataframe is created with "./data-raw/options/anova_options_repeat.R
eq_anova_options <- anova_options_eq_recalc %>%
  mutate(row_id = row_number(),
         mu_values = map(mu, unlist)) %>%
  unnest_wider(mu_values, names_sep = "")  %>%
  rename(m11 = mu_values1, m12 = mu_values2, m21 = mu_values3, m22 = mu_values4,
         tpr = tpr_in) %>%
  select(-mu, -error_message, -result_not_null, -n1, -tpr_out) %>%
  # In the eq_anova_precalculations_results we used map instead of map_dbl in some cases which
  # resulted in lists with one value only instead of an atomic vector
  mutate(across(where(~ is.list(.x) && all(lengths(.x) == 1)), ~ map_dbl(.x, 1)))

# Set file directory -----------------------------------------------------------
ifelse(
  !dir.exists(here("data/eq-anova-res-recalc")),
  dir.create(here("data/eq-anova-res-recalc")),
  "Directory already exists."
)

# Run calculations -------------------------------------------------------------
# First, we split all the iteration as list
eq_anova_options_split <- 
  split(eq_anova_options, eq_anova_options$row_id)

# Calculate Bayesian ANOVA
ssp_eq_anova_res <-
  future.apply::future_lapply(eq_anova_options_split,
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
                                    x$prior_scale,
                                    "eq_band:",
                                    x$eq_band
                                  )
                                )
                                # Calculate sample size
                                results <-
                                  safe_ssp_anova_eq(
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
                                    filename = x$filename,
                                    batch_id = x$batch_id,
                                    calculation_id = x$calculation_id,
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
                                  ),
                                  output = results
                                )
                              }, future.seed = TRUE)

# Save the results
saveRDS(ssp_eq_anova_res, here("data/eq-anova-res-recalc/set-recalc.rds"))

# Remove object
rm(ssp_eq_anova_res)

# Empty memory
gc()
