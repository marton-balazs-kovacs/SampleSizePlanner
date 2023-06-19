#' Determine sample size with Non-inferiority Bayes factor method
#' 
#' A Bayesian statistical testing approach aimed at establishing
#' non-inferiority of one condition compared to the other.
#' 
#' @param tpr Numeric. The desired long-run probability of obtaining a Bayes factor at least as high as the Threshold, given Delta.
#' @param delta Numeric. The expected population effect size.
#' @param ni_margin Numeric. A non-negative number representing the non-inferiority margin.
#' @param thresh Integer. Critical threshold for the Bayes factor.
#' @param tol Numeric. Relative accuracy requested.
#' @param granularity Numeric. Relative precision of the tpr estimates, higher values mean more precision.
#' @param prior_location Numeric. Location of the Cauchy prior distribution.
#' @param prior_scale Numeric. Scale of the Cauchy prior distribution.
#' @param max_n Integer. The maximum number of participants per group (both groups are assumed to have equal sample size).
#' 
#' @return The function returns a list of two named numeric vectors.
#' The first `n1`  the determined sample size per group.
#' The third `tpr_out` is the TPR corresponding to the determined sample sizes.
#' @export
#' @examples
#' \dontrun{
#' SampleSizePlanner::ssp_infer_bf(tpr = 0.8, delta = 0, ni_margin = 0.2, thresh = 10)
#' }
ssp_infer_bf <- function(tpr, ni_margin, delta, thresh = 10, tol = 1e-4, granularity = 300, prior_location = 0, prior_scale = 1/sqrt(2), max_n = 10001) {
  result <- tpr_optim(
    fun = infer_bf,
    range = c(5, max_n),
    delta = delta,
    tpr = tpr,
    ni_margin = ni_margin,
    thresh = thresh,
    tol = tol,
    granularity = granularity,
    prior_location = prior_location,
    prior_scale = prior_scale)
  result
}

#' @rdname ssp_infer_bf
#' @importFrom stats qt pt pcauchy
infer_bf <- function(n1, ni_margin, delta, thresh, tol, granularity, prior_location, prior_scale) {
  n2 = n1
  t = seq(stats::qt(.005, n1+n2-2, (delta+ni_margin)/sqrt(1/n1+1/n2)), 
          stats::qt(.995, n1+n2-2, (delta+ni_margin)/sqrt(1/n1+1/n2)), length = granularity)
  bf = 1
  i = 0
  while (bf < thresh & i < granularity) {
    i = i + 1
    res <- bf10_t(t = t[i], n1 = n1, n2 = n2, independentSamples = TRUE, prior.location = prior_location, prior.scale = prior_scale, prior.df = 1, rel.tol = tol)
    bf <- res$BFplus0 / res$BFmin0
  }
  if (i == granularity) {
    tpr_out = 0
  } else {
    bf = 1
    j = length(t) + 1
    while (bf < thresh) {
      j = j-1
      res <- bf10_t(t = t[j], n1 = n1, n2 = n2, independentSamples = TRUE, prior.location = prior_location, prior.scale = prior_scale, prior.df = 1, rel.tol = tol)
      bf <- res$BFplus0 / res$BFmin0
    }
    tpr_out = stats::pt(t[j], n1+n2-2, (delta+ni_margin)/sqrt(1/n1+1/n2)) - stats::pt(t[i], n1+n2-2, (delta+ni_margin)/sqrt(1/n1+1/n2))
  }
  tpr_out
}