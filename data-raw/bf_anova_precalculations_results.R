# Load necessary packages
library(tidyverse)

# Input params are not saved with result outputs thus we will join them here
bayes_anova_options <-
  tidyr::expand_grid(
    m11 = 0,
    m12 = seq(0, 1, by = 0.25),
    m21 = seq(0, 1, by = 0.25),
    m22 = seq(0, 1, by = 0.25),
    tpr = seq(0.5, 0.95, by = 0.05),
    effect = c("Main Effect 1", "Main Effect 2", "Interaction Effect"),
    thresh = c(3, 6, 10),
    prior_scale = c(1 / sqrt(2), 1, sqrt(2))
  ) %>%
  dplyr::filter(m12 <= m21) %>%
  dplyr::slice(-1) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(mu = mapply(c, m11, m12, m21, m22, SIMPLIFY = FALSE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(iter = row_number()) |> 
  dplyr::mutate(batch_id = str_pad(ceiling(row_number() / 75), width = 3, pad = "0"),
               intra_batch_id = str_pad(((iter - 1) %% 75) + 1, width = 3, pad = "0"))

# Read the outputs of all iterations
bayes_anova_output <- tibble(filename = list.files(path = "./data/bayes-anova-res/", pattern = "\\.rds$", full.names = TRUE)) %>%
  mutate(batch_id = str_extract(filename, "(?<=set-)[\\d]+") %>% str_pad(width = 3, pad = "0")) %>%
  mutate(data = map(filename, readRDS)) %>%
  unnest(data) %>%
  group_by(batch_id) %>%
  mutate(intra_batch_id = str_pad(row_number(), width = 3, pad = "0")) %>%
  ungroup() %>%
  mutate(result = map(data, "result"),
         error = map(data, "error"),
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
  select(-data)

bayes_anova_data <- left_join( bayes_anova_output, bayes_anova_options, by = c("batch_id", "intra_batch_id")) |> 
  arrange(batch_id, intra_batch_id)

# Randomly checking if some results match by recalculating them here
source(here::here("R/ssp_bayesian_anova.R"))
source(here::here("R/tpr_optim.R"))

# Create a function to safely run Bayesian Anova
safe_ssp_anova_bf <- purrr::safely(ssp_anova_bf)

# For batch_id 001 and intra_batch_id 018
safe_ssp_anova_bf(
  tpr = 0.50,
  effect = "Interaction Effect",
  thresh = 3,
  prior_scale = 0.7071068,
  iter = 1000,
  max_n = 500,
  mu = c(0, 0, 0, 0),
  sigma = 1
)

# For batch_id 010 and intra_batch_id 071
safe_ssp_anova_bf(
  tpr = 0.85,
  effect = "Main Effect 2",
  thresh = 10,
  prior_scale = 1.4142136,
  iter = 1000,
  max_n = 500,
  mu = c(0, 0, 0, 0.5),
  sigma = 1
)

# For batch_id 015 and intra_batch_id 029
safe_ssp_anova_bf(
  tpr = 0.95,
  effect = "Interaction Effect",
  thresh = 10,
  prior_scale = 1.4142136,
  iter = 1000,
  max_n = 500,
  mu = c(0, 0, 0, 0.75),
  sigma = 1
)

# Prepare data for saving
# bayes_anova_precalculation_results <- 
#   bayes_anova_data %>% 
#   select(iterate, tpr_out, delta, thresh, prior_scale, n1, h0, ha, error_message) %>% 
#   arrange(iterate)

usethis::use_data(bayes_anova_data, overwrite = TRUE)
