# Load necessary packages
library(tidyverse)

# Create datatable storing possible ROPE options
rope_options <- 
  expand.grid(
    tpr = seq(0.5, 0.95, by = 0.05),
    delta = seq(0, 2, by = 0.05),
    eq_band = seq(0.1, 0.5, by = 0.01),
    prior_scale = c(1 / sqrt(2), 1, sqrt(2))
  )

# rope_options <- rope_options[order(rope_options[, 1], rope_options[, 2]), ]

rope_options$iterate <- 1:nrow(rope_options)

# Read the outputs of all iterations
rope_data <- 
  tibble(filename = list.files(path = "./rope-res/", pattern = ".rds")) %>%
  mutate(file = here::here("rope-res", filename),
         data = map(file, readRDS)) %>% 
  unnest(data) %>% 
  mutate(result = map(data, "result"),
         error = map(data, "error"),
         error_message = map_chr(error, ~ pluck(.x, "message", .default = NA_character_)),
         result_not_null = if_else(map_lgl(result, ~ !is.null(.x)),
                                   1L,
                                   0L),
         n1 = map_dbl(result, ~ pluck(.x, "n1", .default = NA_real_)),
         npower = map_dbl(result, ~ pluck(.x, "npower", .default = NA_real_)),
         iterate = as.integer(names(data))) %>% 
  select(-file, -filename) %>% 
  left_join(., rope_options, by = "iterate")

# Calculate the number of missing results
rope_data %>% 
  count(result_not_null)

# Calculate the number of different error messages
rope_data %>% 
  count(error_message)

# Create the ouput table
# rope_data <-
#   rope_data %>% 
#   select(iterate, power, delta, band, n1, npower) %>% 
#   arrange(iterate)

rope_result <-
  rope_data %>% 
  filter(!is.na(n1)) %>% 
  select(iterate, power, delta, band, n1, npower, error_message)

# rope_new <- 
#   rope_options %>% 
#   slice(62001:77326)

# rope_rerun <- 
#   rope_data %>% 
#   filter(is.na(n1)) %>% 
#   select(-n1, -npower) %>% 
#   bind_rows(rope_new)

# rope_precalculation_results <- 
#   rope_data %>% 
#   select(iterate, n1, npower, error_message, power, delta, band) %>% 
#   rename(tpr = power,
#          eq_band = band)

rope_rerun_data <- 
  tibble(filename = list.files(path = "./rope-res-rerun/", pattern = ".rds")) %>%
  mutate(file = here::here("rope-res-rerun", filename),
         data = map(file, readRDS)) %>% 
  unnest(data) %>% 
  mutate(result = map(data, "result"),
         error = map(data, "error"),
         error_message = map_chr(error, ~ pluck(.x, "message", .default = NA_character_)),
         result_not_null = if_else(map_lgl(result, ~ !is.null(.x)),
                                   1L,
                                   0L),
         n1 = map_dbl(result, ~ pluck(.x, "n1", .default = NA_real_)),
         npower = map_dbl(result, ~ pluck(.x, "npower", .default = NA_real_)),
         iterate = as.integer(names(data))) %>% 
  select(-file, -filename) %>% 
  left_join(., rope_options, by = "iterate")

# Calculate the number of missing results
rope_rerun_data %>% 
  count(result_not_null)

# Join datasets
rope_precalculation_results <- 
  rope_rerun_data %>% 
  select(iterate, power, delta, band, n1, npower, error_message) %>% 
  bind_rows(., rope_result) %>% 
  arrange(iterate) %>% 
  select(iterate, n1, npower, error_message, power, delta, band) %>% 
  rename(tpr = power,
         eq_band = band)

# Save dataset for collaborators
# write_csv(rope_precalculation_results, "rope_precalculation_results.csv")

# Write package data for the app
usethis::use_data(rope_precalculation_results, overwrite = TRUE)
