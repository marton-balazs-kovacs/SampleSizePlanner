#' Determine sample size with Predetermined sample size with Bayes factor method
#' 
#' The present method calculates the corresponding default Bayes factor for
#' a t-test statistic with Cauchy prior distribution centered on zero with
#' scale parameter 1/sqrt(2) for several sample sizes.
#' 
#' @param tpr Numeric. The long-run probability of obtaining a Bayes factor at least as high as the critical threshold favoring superiority, given Delta.
#' @param delta Numeric. The expected population effect size or a range of expected effect sizes.
#' @param max_n Integer. The maximum number of participants per group (both groups are assumed to have equal sample size).
#' @param thresh Integer. Critical threshold for the Bayes factor.
#' @param tol Numeric. TODO
#' @param granularity Numeric. TODO
#' 
#' @return The function returns a list of three named numeric vectors.
#' The first `delta` is the range of deltas provided for the function.
#' The second `n1` the determined sample size per group.
#' The third `npower` is the TPR corresponding to the determined sample sizes
#' with the given delta.
#' @export
#' @examples
#' \dontrun{
#' SampleSizePlanner::ssp_bf_predetermined(tpr = 0.8, delta = 0.5, thresh = 10, max_n = 5000)
#' }
ssp_bf_predetermined <- function(tpr, delta, thresh = 10, max_n = 5000, tol = 1e-4, granularity = 300) {
  n1 <- NULL
  npower <- NULL
  for (i in 1:length(delta))
  {
    res = power_optim(fun = super_bf, range = c(5, max_n),  delta = delta[i], tpr = tpr, thresh = thresh, tol = tol, granularity = granularity)
    n1[i] = res$n1
    npower[i] = res$npower
  }
  list(
    delta = delta,
    n1 = n1,
    npower = npower
  )
}

super_bf <- function(n1, delta, thresh, tol, granularity) {
  n2 = n1
  t = seq(qt(.005, n1+n2-2, delta/sqrt(1/n1+1/n2)), 
          qt(.995, n1+n2-2, delta/sqrt(1/n1+1/n2)), length = granularity)
  bf = 1
  i = 0
  while (bf < thresh) {
    i = i + 1
    bf = exp(BayesFactor::ttest.tstat(t = t[i], n1 = n1, n2 = n2, nullInterval = c(0, Inf), rscale = 1/sqrt(2))[['bf']])
  }
  bf = 1
  j = length(t) + 1
  while (bf < thresh) {
    j = j - 1
    bf = exp(BayesFactor::ttest.tstat(t = t[j], n1 = n1, n2 = n2, nullInterval = c(0, Inf), rscale = 1/sqrt(2))[['bf']])
  }
  npower = pt(t[j], n1+n2-2, delta/sqrt(1/n1+1/n2)) - pt(t[i], n1+n2-2, delta/sqrt(1/n1+1/n2))
  
  npower
}