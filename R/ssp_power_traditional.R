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
#' \dontrun{
#' SampleSizePlanner::ssp_power_traditional(tpr = 0.8, delta = 0.5, max_n = 5000, alpha = 0.05)
#' }
ssp_power_traditional <- function(tpr, delta, max_n, alpha = 0.05) {
  n1 <- power_optim(fun = traditional, range = c(4, max_n), delta = delta, tpr = tpr, alpha = alpha)
  list(n1 = n1)
}

traditional <- function(n1, delta, alpha = .05) {
  1 - pt(qt((1-alpha), n1*2-2), n1*2-2, delta/sqrt(2/n1))
}

