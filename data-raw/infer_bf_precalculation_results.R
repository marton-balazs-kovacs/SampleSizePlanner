# Load necessary packages
library(tidyverse)

# Read the outputs of all iterations
infer_bf_data <- 
  tibble(filename = list.files(path = "./infer-bf-res/", pattern = ".rds")) %>%
  mutate(file = here::here("infer-bf-res", filename),
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
         delta = map_dbl(params, ~ pluck(.x, "delta", .default = NA_real_)),
         prior_scale = map_dbl(params, ~ pluck(.x, "prior_scale", .default = NA_real_)),
         ni_margin = map_dbl(params, ~ pluck(.x, "ni_margin", .default = NA_real_)),
         thresh = map_dbl(params, ~ pluck(.x, "thresh", .default = NA_real_)),
         iterate = map_dbl(params, ~ pluck(.x, "iterate", .default = NA_real_))
  ) %>% 
  select(-file, -filename)

# Check the number of cases where the function calculated the n1
infer_bf_data %>% 
  count(result_not_null)

# Check the same for each prior scale
infer_bf_data %>% 
  count(prior_scale, result_not_null)

# Check the different error messages
infer_bf_data %>% 
  count(error_message)

# Check if when the result is not null there is n1 always
infer_bf_data %>% 
  filter(result_not_null == 1) %>% 
  count(!is.na(n1))

# Calculate maximum n1
max(infer_bf_data$n1, na.rm = TRUE)

# Prepare data for saving
infer_bf_precalculation_results <- 
  infer_bf_data %>% 
  select(iterate, tpr, delta, thresh, prior_scale, ni_margin, n1, tpr_out, error_message) %>% 
  arrange(iterate)

usethis::use_data(infer_bf_precalculation_results, overwrite = TRUE)
