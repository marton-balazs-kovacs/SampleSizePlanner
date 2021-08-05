# Load necessary packages
library(tidyverse)

# Create datatable storing possible options
eq_bf_options <- 
  expand.grid(
    tpr = seq(0.5, 0.95, by = 0.05),
    eq_band = seq(0.1, 0.5, by = 0.05),
    delta = seq(0, 2, by = 0.05),
    thresh = c(3, 6, 10),
    prior_scale = c(1 / sqrt(2), 1, sqrt(2))
  )

eq_bf_options$iterate <- 1:nrow(eq_bf_options)

# Read the outputs of all iterations
eq_bf_data <- 
  tibble(filename = list.files(path = "./eq-bf-res/", pattern = ".rds")) %>%
  mutate(file = here::here("eq-bf-res", filename),
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
  rename(tpr_out = npower) %>% 
  select(-file, -filename) %>% 
  left_join(., eq_bf_options, by = "iterate")

# Check the number of cases where the function calculated the n1
eq_bf_data %>% 
  count(result_not_null)

# Check the same for each prior scale
eq_bf_data %>% 
  count(prior_scale, result_not_null)

# Check the different error messages
eq_bf_data %>% 
  count(error_message)

# Check if when the result is not null there is n1 always
eq_bf_data %>% 
  filter(result_not_null == 1) %>% 
  count(is.na(n1))

# Calculate maximum n1
max(eq_bf_data$n1, na.rm = TRUE)

# Prepare data for saving
eq_bf_precalculation_results <- 
  bfda_data %>% 
  select(iterate, tpr, delta, thresh, n1, tpr_out, error_message) 

usethis::use_data(eq_bf_precalculation_results, overwrite = TRUE)