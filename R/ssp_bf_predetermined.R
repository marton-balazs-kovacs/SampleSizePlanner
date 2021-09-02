#' Determine sample size with Predetermined sample size with Bayes factor method
#' 
#' The present method calculates the corresponding default Bayes factor for
#' a t-test statistic with Cauchy prior distribution for several sample sizes.
#' The function returns the optimal sample size needed to reach the TPR for a given
#' Bayes factor threshold to detect an expected population effect size. If a range of
#' possible population effect sizes are plausible under the given hypothesis,
#' the function can calculate the optimal sample sizes for the given range of
#' effect sizes and present the results in a figure by using the \code{\link{plot_power_curve}} function.
#' 
#' @param tpr Numeric. The long-run probability of obtaining a Bayes factor at least as high as the critical threshold favoring superiority, given Delta.
#' @param delta Numeric. The expected population effect size or a range of expected effect sizes.
#' @param max_n Integer. The maximum number of participants per group (both groups are assumed to have equal sample size).
#' @param thresh Integer. Critical threshold for the Bayes factor.
#' @param tol Numeric. Relative accuracy requested.
#' @param granularity Numeric. Relative precision of the tpr estimates, higher values mean more precision.
#' @param prior_scale Numeric. Scale of the Cauchy prior distribution.
#' 
#' @return The function returns a list of three named numeric vectors.
#' The first `delta` is the range of deltas provided for the function.
#' The second `n1` the determined sample size per group.
#' The third `tpr_out` is the TPR corresponding to the determined sample sizes
#' with the given delta.
#' @export
#' @examples
#' \dontrun{
#' SampleSizePlanner::ssp_bf_predetermined(tpr = 0.8, delta = 0.5, thresh = 10, max_n = 5000)
#' }
ssp_bf_predetermined <- function(tpr, delta, thresh = 10, max_n = 5000, tol = 1e-4, granularity = 300, prior_scale = 1/sqrt(2)) {
  n1 <- NULL
  tpr_out <- NULL
  for (i in 1:length(delta)) {
    res = tpr_optim(
      fun = super_bf,
      range = c(5, max_n),
      delta = delta[i],
      tpr = tpr,
      thresh = thresh,
      tol = tol,
      granularity = granularity,
      prior_scale = prior_scale)
    n1[i] = res$n1
    tpr_out[i] = res$tpr_out
  }
  list(
    delta = delta,
    n1 = n1,
    tpr_out = tpr_out
  )
}

#' @rdname ssp_bf_predetermined
super_bf <- function(n1, delta, thresh, tol, granularity, prior_scale) {
  n2 = n1
  t = seq(stats::qt(.005, n1+n2-2, delta/sqrt(1/n1+1/n2)), 
          stats::qt(.995, n1+n2-2, delta/sqrt(1/n1+1/n2)), length = granularity)
  bf = 1
  i = 0
  while (bf < thresh & i < granularity) {
    i = i + 1
    bf = exp(BayesFactor::ttest.tstat(t = t[i], n1 = n1, n2 = n2, nullInterval = c(0, Inf), rscale = prior_scale)[['bf']])
  }
  
  if (i == granularity) {
    tpr_out = 0
  } else {
    bf = 1
    j = length(t) + 1
    while (bf < thresh) {
      j = j - 1
      bf = exp(BayesFactor::ttest.tstat(t = t[j], n1 = n1, n2 = n2, nullInterval = c(0, Inf), rscale = prior_scale)[['bf']])
      }
    tpr_out = stats::pt(t[j], n1+n2-2, delta/sqrt(1/n1+1/n2)) - stats::pt(t[i], n1+n2-2, delta/sqrt(1/n1+1/n2))
    }
  tpr_out
}