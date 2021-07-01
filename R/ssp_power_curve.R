#' Determine sample size for the power curve method
#' 
#' The power curve shows how changes in effect size modify
#' the statistical power of a test. It is is similar to a
#' classical power analysis but instead of calculating the
#' appropriate sample size for one hypothesized population effect
#' size, the method calculates the required sample size for a range
#' of plausible population effect sizes. To plot the results use the
#' \code{\link{plot_power_curve}} function.
#' 
#' @param delta Numeric. A range of hypothetical population effect sizes.
#' @param tpr Numeric. The desired long-run probabilities of obtaining a significant result with a one-sided t-test, given each value of Delta.
#' @param max_n Integer. The maximum number of participants per group (both groups are assumed to have equal sample size).
#' @param alpha Numeric. The level of significance. 
#' 
#' @return The function returns a list of three named numeric vectors.
#' The first `delta` is the range of deltas provided for the function.
#' The second `n1` the determined sample size per group.
#' The third `npower` is the TPR corresponding to the determined sample sizes
#' with the given delta.
#' @export
#' @examples 
#' \dontrun{
#' SampleSizePlanner::ssp_power_curve(tpr = 0.8, delta = seq(0.1, 0.9, 0.01), max_n = 5000)
#' }
ssp_power_curve <- function(delta, tpr, max_n = 5000, alpha = 0.05) {
  n1 <- NULL
  npower <- NULL
  for (i in 1:length(delta)) {
    res = power_optim(fun = traditional, range = c(5, max_n), delta[i], tpr = tpr, alpha = alpha)
    n1[i] = res$n1
    npower[i] = res$npower
  }
  
  list(
    delta = delta,
    n1 = n1,
    npower = npower
  )
}
