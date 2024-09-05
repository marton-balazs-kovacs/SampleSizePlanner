##########################################################################################
#' Determine sample size with the Region of Practical Equivalence Method (ROPE)
#' 
#' Script for pre-calculations!
#' The present method provides an expected sample size such that
#' compelling evidence in the form of the Highest Density Interval can be collected
#' for a given eq band with a certain long-run probability.
#' 


############################SSP Function for ROPE##########################################
# function to calculate the minimum sample size for a 2-way anova with the ROPE method
ssp_anova_rope <- function(mu, effect, eq_band, tpr, ci, prior_scale, iter, post_iter = 1000, sigma = 1, prior_location = 0, max_n = 10001) {
  
  result <- tpr_optim(
    fun = twoway_ANOVA_rope_pwr,
    range = c(10, max_n),
    mu = mu,
    effect = effect,
    eq_band = eq_band,
    tpr = tpr,
    ci = ci,
    prior_scale = prior_scale,
    sigma = sigma,
    iter = iter,
    post_iter = post_iter,
    prior_location = prior_location
  )
  
  return(result)
}

############################ROPE Function##################################################
# function to calculate the tpr for a 2-way anova with the ROPE method
twoway_ANOVA_rope_pwr  <- function(
    n      = n1,
    effect = effect,
    eq_band = eq_band,
    iter   = iter,
    post_iter = post_iter,
    mu     = mu,
    sigma  = sigma,
    ci = ci,
    prior_scale = prior_scale,
    prior_location = prior_location,
    seed   = NULL)
{
  
  # Create a data frame to store the bayes factors from each iteration
  rope_data <- data.frame(matrix(NA, nrow = iter, ncol = 2))
  colnames(rope_data) <- c("rope_grp1", "rope_grp2")
  
  # For each iteration, generate data, do ANOVA, and calculate ROPE
  for (i in 1:iter) {
    
    # Generate data
    grp1 <- as.factor(rep(c(0, 1), each = 2*n))
    grp2 <- as.factor(rep(c(0, 1), each = n, times = 2))
    value <- c(rnorm(n = n, mean = mu[1], sd = sigma),   # grp1 = 0; grp2 = 0
               rnorm(n = n, mean = mu[2], sd = sigma),   # grp1 = 0; grp2 = 1
               rnorm(n = n, mean = mu[3], sd = sigma),   # grp1 = 1; grp2 = 0
               rnorm(n = n, mean = mu[4], sd = sigma))   # grp1 = 1; grp2 = 1
    data <- data.frame(grp1, grp2, value)
    
    # ROPE Method
    # Main Effects
    results1 <- BayesFactor::lmBF(value ~ grp1, data = data, rscaleEffects = prior_scale, progress = FALSE)
    results2 <- BayesFactor::lmBF(value ~ grp2, data = data, rscaleEffects = prior_scale, progress = FALSE)
    
    # Sample from posterior distribution with for each main effect
    posterior1 <- BayesFactor::posterior(results1, iterations = post_iter, progress = FALSE)
    posterior2 <- BayesFactor::posterior(results2, iterations = post_iter, progress = FALSE)
    
    # grab the delta distributions for each main effect
    delta1 <- (posterior1[,3]-posterior1[,2])/base::sqrt(posterior1[,4])
    delta2 <- (posterior2[,3]-posterior2[,2])/base::sqrt(posterior2[,4])
    
    # Calculate the 95% HDI
    HDI_1 <- bayestestR::hdi(delta1, ci = ci)     
    HDI_2 <- bayestestR::hdi(delta2, ci = ci)
    
    # Check if HDI within eq band and store counts of each iteration into the data frame
    rope_data[i, ] <- c(
      ifelse(HDI_1[1,3]>-eq_band & HDI_1[1,4]<eq_band, 1, 0),   # ROPE count for main effect (group 1)
      ifelse(HDI_2[1,3]>-eq_band & HDI_2[1,4]<eq_band, 1, 0)    # ROPE count for main effect (group 2)
    )
  }
  
  # Determine which effect that gets evaluated or shown
  # Calculate the long-run probability of obtaining HDI within eq band
  if (effect == "Main Effect 1") {
    return(pwr_grp1 = base::sum(rope_data[,1]) / iter)
  } else if (effect == "Main Effect 2") {
    return(pwr_grp2 = base::sum(rope_data[,2]) / iter)
  } 
}

