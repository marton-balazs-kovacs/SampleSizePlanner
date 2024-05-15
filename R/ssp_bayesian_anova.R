
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
#'   effect = "Interaction Effect", tpr = 0.8, max_n = 10001, mu = c(1.5, 1.5, 0, 1), sigma = 2,
#'   seed = NULL, thresh = 10, prior_scale = 1 / sqrt(2)
#'  )
#' }

ssp_anova_bf <- function(effect,  mu, tpr, thresh , prior_scale, iter, max_n = 10001, max_bf = 1e8, sigma = 1, seed = NULL) {

  tpr_optim_res <- tpr_optim(
    fun    = twoway_ANOVA_bf_pwr,
    range  = c(4, max_n),
    tpr    = tpr,
    effect = effect,
    iter   = iter,
    mu     = mu,
    sigma  = sigma,
    seed   = seed,
    thresh  = thresh,
    max_bf = max_bf,
    prior_scale = prior_scale
  )

  return(
    tpr_optim_res
  )
}

# A function for two-way between-subject ANOVA
twoway_ANOVA_bf_pwr  <- function(
    effect = effect,
    iter   = iter,
    n      = n1,
    mu     = mu,
    sigma  = sigma,
    thresh = thresh,
    max_bf = max_bf,
    prior_scale = prior_scale,
    seed   = NULL) {

  # Create a data frame to store the bf-values from each iteration
  bf_value_data <- data.frame(matrix(NA, nrow = iter, ncol = 3))
  colnames(bf_value_data) <- c("bf_grp1", "bf_grp2", "bf_int")

  # Set seeds
  set.seed(seed)

  # For each iteration, generate data, do ANOVA, and record bf-values
  for (i in 1:iter) {
    # Generate data

    grp1 <- as.factor(rep(c(0, 1), each = 2*n))
    grp2 <- as.factor(rep(c(0, 1), each = n, times = 2))

    value <- c(rnorm(n = n, mean = mu[1], sd = sigma), # grp1 = 0; grp2 = 0
               rnorm(n = n, mean = mu[2], sd = sigma), # grp1 = 0; grp2 = 1
               rnorm(n = n, mean = mu[3], sd = sigma), # grp1 = 1; grp2 = 0
               rnorm(n = n, mean = mu[4], sd = sigma)) # grp1 = 1; grp2 = 1
    data <- data.frame(grp1, grp2, value)

    # Anova analysis
    results <- BayesFactor::anovaBF(value ~ grp1 * grp2, data = data,
                                    whichModels = "withmain", rscaleEffects = prior_scale,
                                    progress = FALSE)
    bf_value <- BayesFactor::extractBF(results)[, 1]
    # print(n)
    # print(bf_value)
    # Store bf-values of each iteration into the data frame
    if (bf_value[3] == Inf) {             # fixing infinite bf_value in the denominator to max 10e6
      bf_value[3] <- .Machine$integer.max}
    bf_value_data[i, ] <- c(
      bf_value[1],  # bf-value for main effect (group 1)
      bf_value[2],  # bf-value for main effect (group 2)
      bf_value[4] / bf_value[3]   # bf-value for interaction effect (grp1 + grp2 + grp1:grp2 vs. grp1 + grp2)
    )

    # evaluating if upper bound of BF is exceeded repeatedly
    if (i==11 & all(bf_value_data[1:10,1] > max_bf) & effect == "Main Effect 1") {
      return(pwr_grp1 = 1)
      break
    }
    if (i==11 & all(bf_value_data[1:10,2] > max_bf) & effect == "Main Effect 2") {
      return(pwr_grp2 = 1)
      break
    }
    if (i==11 & all(bf_value_data[1:10,3] > max_bf) & effect == "Interaction Effect") {
      return(pwr_int = 1)
      break
    }
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

