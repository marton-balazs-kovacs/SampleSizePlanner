#' Determine sample size with Bayes Factor Design Analysis (BFDA)
#' 
#' The present method provides an expected sample size such that
#' compelling evidence in the form of a Bayes factor can be collected
#' for a given effect size with a certain long-run probability when
#' allowing for sequential testing.
#' 
#' @param mu Numeric. The expected population mean values.
#' @param thresh Integer. The Bayes factor threshold for inference.
#' @param tpr Numeric. The long-run probability of obtaining a Bayes factor at least
#'   as high as the critical threshold favoring superiority, given mu.
#' @param iter Integer. The number of simulations.
#' @param prior_scale Numeric. Scale of the Cauchy prior distribution.
#' @param max_n Integer. The maximum number of participants per group (all groups are assumed to have equal sample size).
#' 
#' @return The function returns a list of four named numeric vectors.
#' The first `n1` is the range of determined sample sizes for the given design.
#' The second `tpr_out` is the range of TPRs that were provided as a parameter.
#' The third `effect` is the type of effect to evaluate, such as "Main Effect 1," "Main Effect 2," or "Interaction Effect."
#' @export
#' @examples
#' \dontrun{
#' SampleSizePlanner::ssp_anova_bf(
#'   tpr = 0.8, max_n = 400, mu = c(1, 1.2, 1.5, 1.3), sigma = 2,
#'   seed = NULL, thresh = 10, prior_scale = 1 / sqrt(2)
#'  )
#' }

ssp_anova_bf <- function(
    effect = c("Main Effect 1", "Main Effect 2", "Interaction Effect"),
    iter = 1000, max_n, mu, sigma, seed = NULL, tpr, thresh = 10, prior_scale = 1 / sqrt(2)) {
  
  tpr_optim_res <- tpr_optim(
    fun    = twoway_ANOVA_bf_pwr,
    effect = effect,
    iter   = iter,
    range  = c(4, max_n),
    mu     = mu,
    sigma  = sigma,
    seed   = seed,
    tpr    = tpr,
    thresh  = thresh,
    prior_scale = prior_scale
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
twoway_ANOVA_bf_pwr  <- function(
    effect = effect,
    iter   = iter,
    n      = n1,
    mu     = mu,
    sigma  = sigma,
    thresh  = 10,
    prior_scale = 1 / sqrt(2),
    seed   = NULL) {
  
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
  
  # Arrange order so that m1_1 < m1_2 & m2_1 < m2_2   
  sorted_mu <- c(sort(mu[1:2]), sort(mu[3:4]))
  # sort that m1_1 < m2_1 
  if (sorted_mu[3] < sorted_mu[1]) {
    sorted_mu <- c(sorted_mu[c(3:4, 1:2)])
  }
  
  # Re-scale the mu and sigma before calculating the power
  while (sorted_mu[1] != 0 | sigma != 1) {
    sorted_mu    = sorted_mu/sigma           # scale mu by sigma
    sigma        = sigma/sigma               # scale sigma to 1
    sorted_mu    = sorted_mu - sorted_mu[1]  # scale the mean of first group to 0
  }
 
  # Create a data frame to store the p-values from each iteration
  bf_value_data <- data.frame(matrix(NA, nrow = iter, ncol = 3))
  colnames(bf_value_data) <- c("bf_grp1", "bf_grp2", "bf_int")
  
  # Set seeds
  set.seed(seed)
  
  # For each iteration, generate data, do ANOVA, and record p-values
  for (i in 1:iter) {
    # Generate data
    
    grp1 <- as.factor(rep(c(0, 1), each = 2*n))
    grp2 <- as.factor(rep(c(0, 1), each = n, times = 2))
    
    value <- c(rnorm(n = n, mean = sorted_mu[1], sd = sigma), # grp1 = 0; grp2 = 0
               rnorm(n = n, mean = sorted_mu[2], sd = sigma), # grp1 = 0; grp2 = 1
               rnorm(n = n, mean = sorted_mu[3], sd = sigma), # grp1 = 1; grp2 = 0
               rnorm(n = n, mean = sorted_mu[4], sd = sigma)) # grp1 = 1; grp2 = 1
    data <- data.frame(grp1, grp2, value)
    
    # Anova analysis
    results <- BayesFactor::anovaBF(value ~ grp1 * grp2, data = data, 
                                    whichModels = "withmain", rscaleEffects = prior_scale,
                                    progress = FALSE)
    bf_value <- BayesFactor::extractBF(results)[, 1]
    
    # Store p-values of each iteration into the data frame
    bf_value_data[i, ] <- c(
      bf_value[1],  # p-value for main effect (group 1)
      bf_value[2],  # p-value for main effect (group 2)
      bf_value[4] / bf_value[3]   # p-value for interaction effect (grp1 + grp2 + grp1:grp2 vs. grp1 + grp2)
    )
  }

  # Determine which effect that gets evaluated or shown
  if (effect == "Main Effect 1") {
    return(pwr_grp1 = sum(bf_value_data$bf_grp1 >= thresh) / iter)
  } else if (effect == "Main Effect 2") {
    return(pwr_grp2 = sum(bf_value_data$bf_grp2 >= thresh) / iter)
  } else if (effect == "Interaction Effect") {
    return(pwr_int = sum(bf_value_data$bf_int >= thresh) / iter)
  }
  
}
