# Library ----------------------------------------------------------------------
library(tidyverse)
library(future.apply)
library(future.batchtools)
library(purrr)
library(here)

source(here("R/ssp_rope_anova.R"))
source(here("R/tpr_optim.R"))

load(file = here::here("data/anova_options_rope_recalc.rda"))

# Functions --------------------------------------------------------------------
# Create a function to safely run Bayesian Anova
safe_ssp_anova_rope <- purrr::safely(ssp_anova_rope)

# Calculation configurations ---------------------------------------------------
# Dataframe is created with "./data-raw/options/anova_options_repeat.R
rope_anova_options <- anova_options_rope_recalc %>%
  mutate(row_id = row_number(),
         mu_values = map(mu, unlist)) %>%
  unnest_wider(mu_values, names_sep = "")  %>%
  rename(m11 = mu_values1, m12 = mu_values2, m21 = mu_values3, m22 = mu_values4,
         tpr = tpr_in) %>%
  select(-mu, -error_message, -result_not_null, -n1, -tpr_out) %>%
  # In the rope_anova_precalculations_results we used map instead of map_dbl in some cases which
  # resulted in lists with one value only instead of an atomic vector
  mutate(across(where(~ is.list(.x) && all(lengths(.x) == 1)), ~ map_dbl(.x, 1)))

# Set file directory -----------------------------------------------------------
ifelse(
  !dir.exists(here("data/rope-anova-res-recalc")),
  dir.create(here("data/rope-anova-res-recalc")),
  "Directory already exists."
)

# Run calculations -------------------------------------------------------------
# First, we split all the iteration as list
rope_anova_options_split <- 
  split(rope_anova_options, rope_anova_options$row_id)

# Calculate Bayesian ANOVA
ssp_rope_anova_res <-
  future.apply::future_lapply(rope_anova_options_split,
                              function(x) {
                                # Print params of current iteration
                                print(
                                  paste(
                                    "Running:",
                                    "tpr:",
                                    x$tpr,
                                    "effect:",
                                    x$effect,
                                    "prior:",
                                    x$prior_scale,
                                    "eq_band:",
                                    x$eq_band,
                                    "ci:",
                                    x$ci
                                  )
                                )
                                # Calculate sample size
                                results <-
                                  safe_ssp_anova_rope(
                                    tpr = x$tpr,
                                    effect = x$effect,
                                    prior_scale = x$prior_scale,
                                    mu = c(x$m11, x$m12, x$m21, x$m22),
                                    eq_band = x$eq_band,
                                    ci = x$ci,
                                    iter = 1000,
                                    post_iter = 1000,
                                    max_n = 500,
                                    sigma = 1,
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
                                    prior_scale = x$prior_scale,
                                    mu = c(x$m11, x$m12, x$m21, x$m22),
                                    ci = x$ci,
                                    eq_band = x$eq_band,
                                    iter = 1000,
                                    post_iter = 1000,
                                    max_n = 500,
                                    sigma = 1,
                                    prior_location = 0
                                  ),
                                  output = results
                                )
                              }, future.seed = TRUE)

# Save the results
saveRDS(ssp_rope_anova_res, here("data/rope-anova-res-recalc/set-recalc.rds"))

# Remove object
rm(ssp_rope_anova_res)

# Empty memory
gc()

