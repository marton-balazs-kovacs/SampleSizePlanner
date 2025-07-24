# Recalculation of failed pre calculations
options(repos=c(CRAN="https://ftp.belnet.be/mirror/CRAN/"))

set.seed(310779)
pkgs <- c("tidyverse", "future.apply", "purrr", "bayestestR", "progressr","BayesFactor")
invisible(
  lapply(pkgs, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) { install.packages(pkg)}
    library(pkg, character.only = TRUE)
  })
)

source("/home4/p310779/ssp.functions.R")

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

anova_options_eq_recalc <- read.csv("/home4/p310779/ssp_anova_options_eq.csv")

# Functions --------------------------------------------------------------------
# Create a function to safely run Bayesian Anova
safe_ssp_anova_eq <- purrr::safely(ssp_anova_eq)

# Calculation configurations ---------------------------------------------------
# Dataframe is created with "./data-raw/options/anova_options_repeat.R
eq_anova_options <- anova_options_eq_recalc %>%
  mutate(row_id = row_number())

# Run calculations -------------------------------------------------------------
# First, we split all the iteration as list
eq_anova_options_split <- 
  split(eq_anova_options, eq_anova_options$row_id)

# Calculate Bayesian ANOVA
with_progress({
  p <- progressr::progressor(steps = length(eq_anova_options_split))
ssp_eq_anova_res <-
  future.apply::future_lapply(eq_anova_options_split,
                              function(x) {
                                # Print params of current iteration
                                print(
                                  paste(
                                    "Running:",
                                    "tpr:",
                                    x$tpr,
                                    "effect:",
                                    x$effect,
                                    "thresh:",
                                    x$thresh,
                                    "prior:",
                                    x$prior_scale,
                                    "eq_band:",
                                    x$eq_band
                                  )
                                )
                                # Calculate sample size
                                results <-
                                  safe_ssp_anova_eq(
                                    tpr = x$tpr,
                                    effect = x$effect,
                                    thresh = x$thresh,
                                    prior_scale = x$prior_scale,
                                    eq_band = x$eq_band,
                                    mu = c(x$m11, x$m12, x$m21, x$m22),
                                    sigma = 1,
                                    iter = 5000,
                                    post_iter = 5000,
                                    max_n = 500,
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
                                    thresh = x$thresh,
                                    prior_scale = x$prior_scale,
                                    eq_band = x$eq_band,
                                    mu = c(x$m11, x$m12, x$m21, x$m22),
                                    sigma = 1,
                                    iter = 5000,
                                    post_iter = 5000,
                                    max_n = 500,
                                    prior_location = 0
                                  ),
                                  output = results
                                )
                              }, future.seed = TRUE)
})
# Save the results
saveRDS(ssp_eq_anova_res, file = "ssp_eq_anova_res.rds")

# Remove object
rm(ssp_eq_anova_res)

# Empty memory
gc()
