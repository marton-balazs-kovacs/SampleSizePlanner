# Load necessary packages
library(tidyverse)

# Read the descriptives of all sets
all_set <- readRDS("rope-res/all-set.rds")
all_set <-
  all_set %>% 
  mutate(n_iterate = as.character(n_iterate)) %>% 
  rename(iterate = n_iterate)

# Read the outputs of all iterations
rope_data <- 
  tibble(filename = list.files(path = "./rope-res/", pattern = ".rds")) %>%
  filter(filename != "all-set.rds") %>% 
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
         iterate = names(data)) %>% 
  left_join(., all_set, by = "iterate")

# Calculate the number of missing results
rope_data %>% 
  count(result_not_null)

# Calculate the number of different error messages
rope_data %>% 
  count(error_message)

# Create the ouput table
rope_data <-
  rope_data %>% 
  select(iterate, power, delta, band, n1, npower) %>% 
  arrange(iterate)

rope_result <-
  rope_data %>% 
  filter(!is.na(n1))

# Write output table
write_csv(rope_data, "rope_data_all.csv")
write_csv(rope_result, "rope_data_result.csv")
