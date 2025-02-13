#### Script to identify failed pre calculations for re-calculation
#### save the design parameter combinations as csv
library(tidyverse)

## Bayesian anova method
load("data/bayes_anova_data.rda")
dplyr::count(bayes_anova_data, error_message)
anova_options_bayesian_recalc <-
  bayes_anova_data |> 
  dplyr::filter(error_message == "missing value where TRUE/FALSE needed")

## EQ method
load("data/eq_anova_data.rda")
dplyr::count(eq_anova_data, error_message)
anova_options_eq_recalc <- eq_anova_data |> 
  dplyr::filter(error_message == "contrasts can be applied only to factors with 2 or more levels")

## ROPE method
load("data/rope_anova_data.rda")
dplyr::count(rope_anova_data, error_message)
anova_options_rope_recalc <- rope_anova_data |> 
  dplyr::filter(error_message == "contrasts can be applied only to factors with 2 or more levels")

## Write to csv
# store as RDS package data
usethis::use_data(anova_options_bayesian_recalc, overwrite = TRUE)
usethis::use_data(anova_options_eq_recalc, overwrite = TRUE)
usethis::use_data(anova_options_rope_recalc, overwrite = TRUE)
