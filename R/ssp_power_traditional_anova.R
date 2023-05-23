#' Determine sample size with the Classical power analysis method
#' 
#' This method is used to estimate the minimum sample size that
#' a design needs to reach a statistical power, given a  desired
#' significance level and expected effect size.
#' 
#' @param tpr Numeric. The desired long-run probability of obtaining a significant result with a one-sided t-test, given Delta.
#' @param delta Numeric. The expected population effect size.
#' @param max_n Integer. The maximum number of participants per group (both groups are assumed to have equal sample size).
#' @param alpha Numeric. The level of significance.
#' 
#' @return The function returns a list of one named numeric vector.
#' The vector called `n1` contains the determined sample size per group
#' for the given design.
#' @export
#' @examples 
#' \dontrun{
#' SampleSizePlanner::ssp_power_traditional(tpr = 0.8, delta = 0.5, max_n = 5000, alpha = 0.05)
#' }
ssp_power_traditional_anova <- function(
    effect = c("Main Effect 1", "Main Effect 2", "Interaction"), 
    iter = 100, max_n, tpr, alpha = 0.05, seed = NULL) {
  
  tpr_optim(
    fun    = twowayANOVApwr,
    effect = effect,
    iter   = iter,
    range  = c(4, max_n), 
    mu     = mu,
    sigma  = sigma,
    seed   = seed,
    tpr    = tpr, 
    alpha  = alpha,
    seed   = seed
  )
}

# A function for two-way between-subject ANOVA
twowayANOVApwr <- function(effect, iter, n1, mu, sigma, alpha, seed) {
  
  # Evaluate if effects are specified
  effects <- c("Main Effect 1", "Main Effect 2", "Interaction")
  if (is.na(effect)||!effect %in% effects) {
    message("Specify which effect: Main or interaction")
    stop(call. = FALSE)
  }
  
  # Evaluate if parameters input are correct
  if (!is.vector(mu)||length(mu) != 4) {
    message("Mean group must be a vector of four!")
    stop(call. = FALSE)
  }
  
  # Re-scale the mu and sigma before calculating the power
  while (mu[1] != 0 | sigma != 1) {
    mu    = mu/sigma    # scale mu by sigma
    sigma = sigma/sigma # scale sigma to 1
    mu    = mu - mu[1]  # scale the mean of first group to 0
  }
  
  # Create a data frame to store the p-values from each iteration
  pval_data <- data.frame(matrix(NA, nrow = iter, ncol = 3))
  colnames(pval_data) <- c("pval_grp1", "pval_grp2", "pval_int")
  
  # Set seeds
  set.seed(seed)
  
  # For each iteration, generate data, do ANOVA, and record p-values
  for (i in 1:iter) {
    # Generate data
    grp1  <- c(rep(0, n*2), rep(1, n*2))
    grp2  <- c(rep(0, n), rep(1, n), rep(0, n), rep(1, n))
    value <- c(rnorm(n = n, mean = mu[1], sd = sigma), # grp1 = 0; grp2 = 0
               rnorm(n = n, mean = mu[2], sd = sigma), # grp1 = 0; grp2 = 1
               rnorm(n = n, mean = mu[3], sd = sigma), # grp1 = 1; grp2 = 0
               rnorm(n = n, mean = mu[4], sd = sigma)) # grp1 = 1; grp2 = 1
    data <- data.frame(grp1, grp2, value)
    
    # Anova analysis
    results <- summary.aov(lm(value ~ grp1 + grp2 + grp1:grp2, data))[[1]]
    
    # Store p-values of each iteration into the data frame
    pval_data[i, ] <- c(
      results[1, 5],  # p-value for main effect (group 1)
      results[2, 5],  # p-value for main effect (group 2)
      results[3, 5]   # p-value for interaction effect
    )
  }
  
  # Determine which effect that gets evaluated or shown
  if (effect == "Main Effect 1") {
    return(pwr_grp1 = sum(pval_data$pval_grp1 < alpha) / iter)
    
  } else if (effect == "Main Effect 2") {
    return(pwr_grp2 = sum(pval_data$pval_grp2 < alpha) / iter)
    
  } else if (effect == "Interaction") {
    return(pwr_int  = sum(pval_data$pval_int  < alpha) / iter)
    
  }
  
}

