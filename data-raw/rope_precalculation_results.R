# Load necessary packages
library(tidyverse)

# Read the outputs of all iterations
rope_data <- 
  tibble(filename = list.files(path = "./rope-res/", pattern = ".rds")) %>%
  mutate(file = here::here("rope-res", filename),
         data = map(file, readRDS)) %>% 
  unnest(data) %>% 
  mutate(result = map(data, "result"),
         error = map(data, "error"),
         params = map(data, "params"),
         error_message = map_chr(error, ~ pluck(.x, "message", .default = NA_character_)),
         result_not_null = if_else(map_lgl(result, ~ !is.null(.x)),
                                   1L,
                                   0L),
         n1 = map_dbl(result, ~ pluck(.x, "n1", .default = NA_real_)),
         tpr_out = map_dbl(result, ~ pluck(.x, "tpr_out", .default = NA_real_)),
         tpr = map_dbl(params, ~ pluck(.x, "tpr", .default = NA_real_)),
         eq_band = map_dbl(params, ~ pluck(.x, "eq_band", .default = NA_real_)),
         delta = map_dbl(params, ~ pluck(.x, "delta", .default = NA_real_)),
         prior_scale = map_dbl(params, ~ pluck(.x, "prior_scale", .default = NA_real_))
         ) %>% 
  select(-file, -filename) %>% 
  rowid_to_column(var = "iterate")

# Calculate the number of missing results
rope_data %>% 
  count(result_not_null)

# Calculate the number of different error messages
rope_data %>% 
  count(error_message)

# Prepare data for saving
rope_precalculation_results <-
  rope_data %>% 
  select(iterate, tpr, delta, eq_band, prior_scale, n1, tpr_out, error_message) %>% 
  arrange(iterate)

# Write package data for the app
usethis::use_data(rope_precalculation_results, overwrite = TRUE)
