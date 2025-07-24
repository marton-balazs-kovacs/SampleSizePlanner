# Load necessary packages
library(tidyverse)
library(here)

# Read the outputs of all iterations
bayes_anova_data_original <-
  readRDS(here("data/recalculation/ssp_bayes_anova_res.rds"))

# Clean the data
bayes_anova_data <-
  tibble(
    calculation_id = seq_along(bayes_anova_data_original),
    parameters = map(bayes_anova_data_original, "parameters"),
    output = map(bayes_anova_data_original, "output")
  ) |>
  mutate(calculation_id = str_pad(row_number(), width = 3, pad = "0")) %>%
  mutate(
    tpr_in = map_dbl(parameters, "tpr"),
    effect = map_chr(parameters, "effect"),
    thresh = map_int(parameters, "thresh"),
    prior_scale = map_dbl(parameters, "prior_scale"),
    mu = map(parameters, "mu"),
    sigma = map_dbl(parameters, "sigma"),
    result = map(output, "result"),
    error = map(output, "error"),
    error_message = map_chr(error, ~ pluck(.x, "message", .default = NA_character_)),
    result_not_null = if_else(map_lgl(result, ~ !is.null(.x)),
                              1L,
                              0L),
    n1 = map_dbl(result, ~ pluck(.x, "n1", .default = NA_real_)),
    tpr_out = map_dbl(result, ~ pluck(.x, "tpr_out", .default = NA_real_)),
    effect_out = map_chr(result, ~ pluck(.x, "effect", .default = NA_character_))
  ) |>
  select(-parameters, -output, -result, -error) |> 
  arrange(calculation_id)

# Calculating the number of calculated values
bayes_anova_data |> 
  count(result_not_null)

bayes_anova_data |> 
  count(error_message)

usethis::use_data(bayes_anova_data, overwrite = TRUE)
