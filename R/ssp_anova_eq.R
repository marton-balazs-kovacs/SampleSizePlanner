#########################################################################################
#' Determine sample size with the Bayesian Interval Equivalence Method 
#' 
#' Script for pre-calculation.
#' The present method provides an expected sample size such that
#' compelling evidence in the form of a Bayes factor can be collected
#' for a given eq band with a certain long-run probability.
#' 
#' @param effect Character. The effect of interest (main effect A, main effect B).
#' @param mu Numeric. The unstandardized mean of the DV for each group.
#' @param sigma Numeric. The standard deviation of the DV for the groups.
#' @param eq_band Numeric. The margin of the standardized equivalence region.
#' @param tpr Numeric. The desired long-run probability of obtaining a Bayes factor higher than Thresh, given the means.
#' @param thresh Numeric. The threshold of the Bayes Factor which is fixed to 10 in the ShinyApp.
#' @param prior_scale Numeric. The scale of the Cauchy prior which is fixed to 1 / sqrt(2) in the ShinyApp.
#' @param max_n Numeric. The maximum group size which is fixed to 500 in the ShinyApp.
#' @param iter Numeric. The number of iterations to calculate the TPR which is fixed to 1000 in the ShinyApp.
#' @param post_iter Numeric.  The number of iterations to estimate the posterior distribution which is fixed to 1000 in the ShinyApp.
#' @param prior_location Numeric. The scale of the Cauchy prior which is fixed to 1 / sqrt(2) in the ShinyApp.
#' @param seed Numeric. Seed for the random calculations.
#'
#' @return The function returns a list of two named numeric vectors.
#'   The second `n1` the determined sample size per group.
#'   The third `tpr_out` is the TPR corresponding to the determined sample sizes.
#'   
#' @export
############################SSP Function for EQ##########################################
# function to calculate the minimum sample size for a 2-way anova with the EQ method
ssp_anova_eq <- function(mu, effect, eq_band, tpr, thresh, prior_scale, iter, post_iter = 1000, sigma = 1, prior_location = 0, max_n = 10001, seed = NULL) {
  
  result <- tpr_optim(
    fun = twoway_ANOVA_eq_pwr,
    range = c(10, max_n),
    mu = mu,
    effect = effect,
    eq_band = eq_band,
    tpr = tpr,
    thresh = thresh,
    prior_scale = prior_scale,
    sigma = sigma,
    iter = iter,
    post_iter = post_iter,
    prior_location = prior_location,
    seed = seed
  )
  
  return(result)
}



############################Equivalence Interval Function################################
# function for pre-calculations to calculate the tpr for a 2-way anova with the
# Bayesian Equivalence Interval Method
#' @rdname ssp_anova_eq
#' @param n Numeric. The sample size per group during the tpr optimization process.
twoway_ANOVA_eq_pwr  <- function(
    n      = n1,
    effect = effect,
    eq_band = eq_band,
    iter   = iter,
    post_iter = post_iter,
    mu     = mu,
    sigma  = sigma,
    thresh  = thresh,
    prior_scale = prior_scale,
    prior_location = prior_location,
    seed   = NULL) {
  
  # Set seeds
  set.seed(seed)
  
  # Create a data frame to store the bayes factors from each iteration
  bf_data <- data.frame(matrix(NA, nrow = iter, ncol = 2))
  colnames(bf_data) <- c("bf_grp1", "bf_grp2")
  
  # For each iteration, generate data, do ANOVA, and calculate Equivalence Interval Bayes Factor
  for (i in 1:iter) {
    
    # Generate data
    grp1 <- as.factor(rep(c(0, 1), each = 2*n))
    grp2 <- as.factor(rep(c(0, 1), each = n, times = 2))
    value <- c(rnorm(n = n, mean = mu[1], sd = sigma),   # grp1 = 0; grp2 = 0
               rnorm(n = n, mean = mu[2], sd = sigma),   # grp1 = 0; grp2 = 1
               rnorm(n = n, mean = mu[3], sd = sigma),   # grp1 = 1; grp2 = 0
               rnorm(n = n, mean = mu[4], sd = sigma))   # grp1 = 1; grp2 = 1
    data <- data.frame(grp1, grp2, value)
    
    # Equivalence Interval Bayes Factor Method
    # Main Effects
    results1 <- BayesFactor::lmBF(value ~ grp1, data = data, rscaleEffects = prior_scale, progress = FALSE)
    results2 <- BayesFactor::lmBF(value ~ grp2, data = data, rscaleEffects = prior_scale, progress = FALSE)
    
    # Sample from posterior distribution with for each main effect
    posterior1 <- BayesFactor::posterior(results1, iterations = post_iter, progress = FALSE)
    posterior2 <- BayesFactor::posterior(results2, iterations = post_iter, progress = FALSE)
    
    # grab the delta distributions for each main effect
    delta1 <- (posterior1[,3]-posterior1[,2])/base::sqrt(posterior1[,4])
    delta2 <- (posterior2[,3]-posterior2[,2])/base::sqrt(posterior2[,4])
    
    # area inside the equivalence interval of posterior distributions
    post_1_dens <- (base::sum(delta1<eq_band) - base::sum(delta1<(-eq_band))) / post_iter 
    post_2_dens <- (base::sum(delta2<eq_band) - base::sum(delta2<(-eq_band))) / post_iter
    
    # Sample from prior cauchy distribution and calculate area inside the interval
    prior_dens <- (stats::pcauchy(eq_band, location = prior_location, scale = prior_scale) 
                   - stats::pcauchy(-eq_band, location = prior_location, scale = prior_scale))
    
    # Calculate bayes factors BF.01
    bf_1 <- (post_1_dens / (1-post_1_dens)) / (prior_dens / (1 - prior_dens))
    bf_2 <- (post_2_dens / (1-post_2_dens)) / (prior_dens / (1 - prior_dens))
    
    # Store Bayes Factors of each iteration into the data frame
    bf_data[i, ] <- c(
      bf_1,    # Bayes factor for main effect (group 1)
      bf_2     # Bayes factor for main effect (group 2)
    )
  }
  # Determine which effect that gets evaluated or shown
  # Calculate the long-run probability of obtaining a Bayes Factor [01] > thresh
  if (effect == "Main Effect 1") {
    return(pwr_grp1 = base::sum(bf_data[,1]> thresh) / iter)
  } else if (effect == "Main Effect 2") {
    return(pwr_grp2 = base::sum(bf_data[,2]> thresh) / iter)
  } 
  
}

