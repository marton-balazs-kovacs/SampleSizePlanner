#### Script to identify failed pre calculations for re-calculation
#### save the design parameter combinations as csv
library(tidyverse)

## Bayesian anova method
data("bayes_anova_data", package = "SampleSizePlanner")
dplyr::distinct(bayes_anova_data, error_message)
anova_options_bayesian_recalc <-
  bayes_anova_data |> 
  dplyr::filter(error_message == "missing value where TRUE/FALSE needed")

## EQ method
data("eq_anova_data", package = "SampleSizePlanner")
dplyr::distinct(eq_anova_data, error_message)
anova_options_eq_recalc <- eq_anova_data |> 
  dplyr::filter(error_message == "contrasts can be applied only to factors with 2 or more levels")

## ROPE method
data("rope_anova_data", package = "SampleSizePlanner")
dplyr::distinct(rope_anova_data, error_message)
anova_options_rope_recalc <- rope_anova_data |> 
  dplyr::filter(error_message == "contrasts can be applied only to factors with 2 or more levels")

## Write to csv
# store as RDS package data
usethis::use_data(anova_options_bayesian_recalc, overwrite = TRUE)
usethis::use_data(anova_options_eq_recalc, overwrite = TRUE)
usethis::use_data(anova_options_rope_recalc, overwrite = TRUE)
