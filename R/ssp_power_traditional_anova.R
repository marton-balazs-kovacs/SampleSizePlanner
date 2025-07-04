#' Determine sample size with the Classical power analysis method
#'
#' This method is used to estimate the minimum sample size that
#' a design needs to reach a statistical power, given a  desired
#' significance level and expected effect size.
#' 
#' @param effect Character. The effect of interest (main effect 1, main effect 2, interaction effect).
#' @param mu Numeric. The mean of the DV for each group.
#' @param sigma Numeric. The standard deviation of the DV for the groups.
#' @param tpr Numeric. The desired long-run probability of obtaining a significant result, given the means.
#' @param max_n Integer. The maximum number of participants per group (both groups are assumed to have equal sample size).
#' @param alpha Numeric. The level of significance.
#' @param iter Integer. The number of iterations.
#' @param seed Numeric. The seed for reproducibility.
#' 
#' @return The function returns a list of one named numeric vector.
#' The vector called `n1` contains the determined sample size per group
#' for the given design.
#' @export
#' @examples
#' \dontrun{
#' SampleSizePlanner::ssp_power_traditional_anova(
#'   tpr = 0.8, max_n = 400, mu = c(1, 1.2, 1.5, 1.3), sigma = 2,
#'   seed = NULL, alpha = 0.05
#'  )
#' }
ssp_power_traditional_anova <- function(
    effect = c("Main Effect 1", "Main Effect 2", "Interaction Effect"),
    iter = 1000, max_n, mu, sigma, seed = NULL, tpr, alpha = 0.05) {
  
  tpr_optim_res <- tpr_optim(
    fun    = twowayANOVApwr,
    effect = effect,
    iter   = iter,
    range  = c(4, max_n),
    mu     = mu,
    sigma  = sigma,
    seed   = seed,
    tpr    = tpr,
    alpha  = alpha
  )
  
  return(
    list(
      n1 = tpr_optim_res$n1,
      tpr_out = tpr_optim_res$tpr_out,
      effect = effect
    )
  )
}

# A function for two-way between-subject ANOVA
#' @rdname ssp_anova_bf
#' @param n1 Numeric. The sample size per group during the tpr optimization process.
twowayANOVApwr <- function(effect, iter, n1, mu, sigma, alpha, seed) {
  
  # Evaluate if effects are specified
  effects <- c("Main Effect 1", "Main Effect 2", "Interaction Effect")
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
  while (mu[1] != 0 || sigma != 1) {
    mu <- mu / sigma        # scale mu by sigma
    sigma     <- sigma / sigma            # scale sigma to 1
    mu <- mu - mu[1] # scale the mean of first group to 0
  }
  
  # Create a data frame to store the p-values from each iteration
  pval_data <- data.frame(matrix(NA, nrow = iter, ncol = 3))
  colnames(pval_data) <- c("pval_grp1", "pval_grp2", "pval_int")
  
  # Set seeds
  set.seed(seed)
  
  # For each iteration, generate data, do ANOVA, and record p-values
  for (i in 1:iter) {
    # Generate data
    grp1  <- c(rep(0, n1*2), rep(1, n1*2))
    grp2  <- c(rep(0, n1), rep(1, n1), rep(0, n1), rep(1, n1))
    value <- c(rnorm(n = n1, mean = mu[1], sd = sigma),
               rnorm(n = n1, mean = mu[2], sd = sigma),
               rnorm(n = n1, mean = mu[3], sd = sigma),
               rnorm(n = n1, mean = mu[4], sd = sigma))
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
  } else if (effect == "Interaction Effect") {
    return(pwr_int  = sum(pval_data$pval_int  < alpha) / iter)
  }
}
