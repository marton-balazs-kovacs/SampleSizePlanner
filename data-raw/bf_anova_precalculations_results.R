# Load necessary packages
library(tidyverse)

# Read the outputs of all iterations
bayes_anova_data <- 
  tibble(filename = list.files(path = "./data/bayes-anova-res/", pattern = ".rds")) %>%
  mutate(file = here::here("data", "bayes-anova-res", filename),
         data = map(file, readRDS)) %>% 
  unnest(data) %>% 
  mutate(result = map(data, "result"),
         error = map(data, "error"),
         # params = map(data, "params"),
         # error_message = map_chr(error, ~ pluck(.x, "message", .default = NA_character_)),
         # result_not_null = if_else(map_lgl(result, ~ !is.null(.x)),
         #                           1L,
         #                           0L),
         # n1 = map_dbl(result, ~ pluck(.x, "n1", .default = NA_real_)),
         # # tpr in result and params are the same
         # tpr_out = map_dbl(result, ~ pluck(.x, "tpr", .default = NA_real_)),
         # h0 = map_dbl(result, ~ pluck(.x, "h0", .default = NA_real_)),
         # ha = map_dbl(result, ~ pluck(.x, "ha", .default = NA_real_)),
         # delta = map_dbl(params, ~ pluck(.x, "delta", .default = NA_real_)),
         # thresh = map_dbl(params, ~ pluck(.x, "thresh", .default = NA_real_)),
         # prior_scale = map_dbl(params, ~ pluck(.x, "prior_scale", .default = NA_real_))
         )  
  # select(-file, -filename) %>% 
  # rowid_to_column(var = "iterate")

# Calculate the number of missing results
bayes_anova_data %>% 
  count(result_not_null)

# Calculate the number of different error messages
bayes_anova_data %>% 
  count(error_message)

# Prepare data for saving
bayes_anova_precalculation_results <- 
  bayes_anova_data %>% 
  select(iterate, tpr_out, delta, thresh, prior_scale, n1, h0, ha, error_message) %>% 
  arrange(iterate)

usethis::use_data(bayes_anova_precalculation_results, overwrite = TRUE)
