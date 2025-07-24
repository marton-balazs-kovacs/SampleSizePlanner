# Recalculation of failed pre calculations
options(repos=c(CRAN="https://ftp.belnet.be/mirror/CRAN/"))

set.seed(310779)
pkgs <- c("tidyverse", "future.apply", "purrr", "bayestestR", "progressr", "BayesFactor")
invisible(
  lapply(pkgs, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) { install.packages(pkg)}
    library(pkg, character.only = TRUE)
  })
)

#source(here("R/ssp_bayesian_anova.R"))
#source(here("R/tpr_optim.R"))
source("/home4/p310779/ssp.functions.R")
#source("/Users/lortz/Desktop/Research Master/Projects/Sample Size planner/Recalc/ssp.functions.R")
#anova_options_bayesian_recalc <- read.csv("/home4/p310779/ssp_anova_options_bayesian_repeat")
anova_options_bayesian_recalc <- read.csv("/home4/p310779/ssp_anova_options_bayesian.csv")

#anova_options_bayesian_recalc <- read.csv("/Users/lortz/Desktop/Research Master/Projects/Sample Size planner/Recalc/ssp_anova_options_bayesian_repeat")
# Functions --------------------------------------------------------------------
# Create a function to safely run Bayesian Anova
safe_ssp_anova_bf <- purrr::safely(ssp_anova_bf)

# Parallel session setup -------------------------------------------------------

# Set number of cores that we want to allocate
n_cores <- future::availableCores()-10
print(paste("Available cores:", n_cores))

# Make `future` plan for multisession
future::plan(multisession, workers = n_cores)
handlers(handler_txtprogressbar())


# Calculation configurations ---------------------------------------------------
# Dataframe is created with "./data-raw/options/anova_options_repeat.R
bayes_anova_options <- anova_options_bayesian_recalc %>%
  mutate(row_id = row_number())
         
# Run calculations -------------------------------------------------------------
anova_options_rope_recalc <- read.csv("/home4/p310779/ssp_anova_options_rope.csv")


# Functions --------------------------------------------------------------------
# Create a function to safely run Bayesian Anova
safe_ssp_anova_rope <- purrr::safely(ssp_anova_rope)

# Calculation configurations ---------------------------------------------------
# Dataframe is created with "./data-raw/options/anova_options_repeat.R
rope_anova_options <- anova_options_rope_recalc %>%
  mutate(row_id = row_number()) 

# Run calculations -------------------------------------------------------------
# First, we split all the iteration as list
rope_anova_options_split <- 
  split(rope_anova_options, rope_anova_options$row_id)

# Calculate Bayesian ANOVA
with_progress({
  p <- progressr::progressor(steps = length(eq_anova_options_split))
ssp_rope_anova_res <-
  future.apply::future_lapply(rope_anova_options_split,
                              function(x) {
                                # Print params of current iteration
                                print(
                                  paste(
                                    "Running:",
                                    "tpr:",
                                    x$tpr,
                                    "effect:",
                                    x$effect,
                                    "prior:",
                                    x$prior_scale,
                                    "eq_band:",
                                    x$eq_band,
                                    "ci:",
                                    x$ci
                                  )
                                )
                                # Calculate sample size
                                results <-
                                  safe_ssp_anova_rope(
                                    tpr = x$tpr,
                                    effect = x$effect,
                                    prior_scale = x$prior_scale,
                                    mu = c(x$m11, x$m12, x$m21, x$m22),
                                    eq_band = x$eq_band,
                                    ci = x$ci,
                                    iter = 5000,
                                    post_iter = 5000,
                                    max_n = 500,
                                    sigma = 1,
                                    prior_location = 0
                                  )
                                p()
                                # Combine results with params to save
                                list(
                                  parameters = list(
                                    filename = x$filename,
                                    batch_id = x$batch_id,
                                    calculation_id = x$calculation_id,
                                    tpr = x$tpr,
                                    effect = x$effect,
                                    prior_scale = x$prior_scale,
                                    mu = c(x$m11, x$m12, x$m21, x$m22),
                                    ci = x$ci,
                                    eq_band = x$eq_band,
                                    iter = 5000,
                                    post_iter = 5000,
                                    max_n = 500,
                                    sigma = 1,
                                    prior_location = 0
                                  ),
                                  output = results
                                )
                              }, future.seed = TRUE)
})
# Save the results
saveRDS(ssp_rope_anova_res, file = "ssp_rope_anova_res.rds")

# Remove object
rm(ssp_rope_anova_res)

# Empty memory
gc()
