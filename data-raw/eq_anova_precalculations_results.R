# Load necessary packages
library(tidyverse)

# Read the outputs of all iterations
eq_anova_data <-
  tibble(filename = list.files(
    path = "./data/eq-anova-res/",
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
    eq_band = map_dbl(parameters, "eq_band"),
    mu = map(parameters, "mu"),
    sigma = map_dbl(parameters, "sigma"),
    iter = map(parameters, "iter"),
    post_iter = map(parameters, "post_iter"),
    max_n = map(parameters, "max_n"),
    prior_location = map(parameters, "prior_location"),
    result = map(output, "result"),
    error = map(output, "error"),
    error_message = map_chr(error, ~ pluck(.x, "message", .default = NA_character_)),
    result_not_null = if_else(map_lgl(result, ~ !is.null(.x)),
                              1L,
                              0L),
    n1 = map_dbl(result, ~ pluck(.x, "n1", .default = NA_real_)),
    tpr_out = map_dbl(result, ~ pluck(.x, "tpr_out", .default = NA_real_)),
    # effect_out = map_chr(result, ~ pluck(.x, "effect", .default = NA_character_)),
    # Because the iteration ids in the filenames do not have trailing zeros to keep the order to join the results with the input params we need to add them here
    batch_id = str_extract(filename, "\\d+"),
    batch_id = str_pad(batch_id, width = 3, pad = "0")
  ) |>
  select(-data, -parameters, -output, -result, -error) |> 
  arrange(batch_id, calculation_id)

# Calculating the number of calculated values
eq_anova_data |> 
  count(result_not_null)

usethis::use_data(eq_anova_data, overwrite = TRUE)
