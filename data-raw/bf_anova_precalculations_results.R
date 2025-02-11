# Load necessary packages
library(tidyverse)

# Read the outputs of all iterations
bayes_anova_data_original <-
  tibble(filename = list.files(
    path = "./inst/extdata/bayes-anova-res/",
    pattern = "\\.rds$",
    full.names = TRUE
  )) %>%
  mutate(batch_id = str_extract(filename, "(?<=set-)[\\d]+") %>% str_pad(width = 3, pad = "0")) %>%
  mutate(data = map(filename, readRDS)) %>%
  unnest(data) %>%
  group_by(batch_id) %>%
  mutate(calculation_id = str_pad(row_number(), width = 3, pad = "0")) %>%
  ungroup() %>%
  mutate(parameters = map(data, "parameters"),
         output = map(data, "output")) |>
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
    effect_out = map_chr(result, ~ pluck(.x, "effect", .default = NA_character_)),
    # Because the iteration ids in the filenames do not have trailing zeros to keep the order to join the results with the input params we need to add them here
    batch_id = str_extract(filename, "\\d+"),
    batch_id = str_pad(batch_id, width = 3, pad = "0")
  ) |>
  select(-data, -parameters, -output, -result, -error) |> 
  arrange(batch_id, calculation_id)

bayes_anova_data_original |> 
  count(error_message)

bayes_anova_data_original |> 
  count(result_not_null)

# Load recalc data
bayes_anova_recalc_data <-
  tibble(filename_recalc = list.files(
    path = "./inst/extdata/bayes-anova-res-recalc/",
    pattern = "\\.rds$",
    full.names = TRUE
  )) %>%
  mutate(data = map(filename_recalc, readRDS)) %>%
  unnest(data) %>%
  mutate(parameters = map(data, "parameters"),
         output = map(data, "output")) |>
  mutate(
    filename = map_chr(parameters, "filename"),
    batch_id = map_chr(parameters, "batch_id"),
    calculation_id = map_chr(parameters, "calculation_id"),
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
  select(-data, -parameters, -output, -result, -error) |> 
  arrange(batch_id, calculation_id)

bayes_anova_recalc_data |> 
  count(error_message)

bayes_anova_recalc_data |> 
  count(result_not_null)

# Merging original calculation results with the recalculation results
bayes_anova_data <- bayes_anova_data_original %>%
  rows_update(bayes_anova_recalc_data %>%
              select(batch_id, calculation_id, tpr_in, effect, thresh, 
                     prior_scale, mu, sigma,
                     error_message, result_not_null, n1, tpr_out),
            by = c("batch_id", "calculation_id", "tpr_in", "effect", "thresh", 
                   "prior_scale", "mu", "sigma"),
            unmatched = "error")

# Calculating the number of calculated values
bayes_anova_data |> 
  count(result_not_null)

bayes_anova_data |> 
  count(error_message)

usethis::use_data(bayes_anova_data, overwrite = TRUE)
