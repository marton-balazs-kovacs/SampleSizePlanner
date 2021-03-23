# Load necessary packages
library(tidyverse)

# Create datatable storing possible BFDA options
bfda_options <- 
  expand.grid(
    tpr = seq(0.5, 0.95, by = 0.05),
    delta = seq(0, 2, by = 0.05),
    thresh = c(3, 6, 10)
  )

bfda_options$iterate <- 1:nrow(bfda_options)

# Read the outputs of all iterations
bfda_data <- 
  tibble(filename = list.files(path = "./bfda-res/", pattern = ".rds")) %>%
  mutate(file = here::here("bfda-res", filename),
         data = map(file, readRDS)) %>% 
  unnest(data) %>% 
  mutate(result = map(data, "result"),
         error = map(data, "error"),
         error_message = map_chr(error, ~ pluck(.x, "message", .default = NA_character_)),
         result_not_null = if_else(map_lgl(result, ~ !is.null(.x)),
                                   1L,
                                   0L),
         n1 = map_dbl(result, ~ pluck(.x, "n1", .default = NA_real_)),
         tpr = map_dbl(result, ~ pluck(.x, "tpr", .default = NA_real_)),
         h0 = map_dbl(result, ~ pluck(.x, "h0", .default = NA_real_)),
         ha = map_dbl(result, ~ pluck(.x, "ha", .default = NA_real_)),
         iterate = as.integer(names(data))) %>% 
  rename(tpr_out = tpr) %>% 
  select(-file, -filename) %>% 
  left_join(., bfda_options, by = "iterate")

bfda_precalculation_results <- 
  bfda_data %>% 
  select(iterate, tpr, delta, thresh, n1, tpr_out, h0, ha, error_message) 

usethis::use_data(bfda_precalculation_results, overwrite = TRUE)
