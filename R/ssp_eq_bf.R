#' Determine sample size with Equivalence interval Bayes factor method
#' 
#' A Bayesian statistical testing approach aimed at establishing
#' equivalence between two groups.
#' 
#' @param tpr Numeric. The desired long-run probability of obtaining a Bayes factor at least as high as the Threshold, given Delta.
#' @param delta Numeric. The expected population effect size.
#' @param eq_band Numeric. The chosen width of the equivalence region.
#' @param thresh Integer. Critical threshold for the Bayes factor.
#' @param tol Numeric. TODO
#' @param granularity Numeric. TODO
#' 
#' @return The function returns a list of two named numeric vectors.
#' The first `n1`  the determined sample size per group.
#' The third `npower` is the TPR corresponding to the determined sample sizes.
#' @export
#' @examples
#' \dontrun{
#' SampleSizePlanner::ssp_eq_bf(tpr = 0.8, delta = 0, eq_band = 0.2, thresh = 10)
#' }
ssp_eq_bf <- function(tpr, eq_band, delta, thresh = 10, tol = 1e-4, granularity = 300) {
  est <- ssp_tost(tpr = tpr, eq_band = eq_band, delta = delta) %>% purrr::pluck("n1")
  result <- power_optim(fun = eq_bf, range = c(10, est), delta = delta, tpr = tpr, eq_band = eq_band, thresh = thresh, tol = tol, granularity = granularity)
  result
}

eq_bf <- function(n1, eq_band, delta, thresh, tol, granularity) {
  n2 = n1
  t = seq(qt(.005, n1+n2-2, delta/sqrt(1/n1+1/n2)), 
          qt(.995, n1+n2-2, delta/sqrt(1/n1+1/n2)), length = granularity)
  bf = 1
  i = 0
  while (bf < thresh & i < granularity) {
    i = i + 1
    upper = cdf_t(eq_band, t[i], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                   prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = tol)
    lower = cdf_t(-eq_band, t[i], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                   prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = tol)
    post_dens = upper - lower
    prior_dens = pcauchy(eq_band, scale = 1/sqrt(2)) - 
      pcauchy(-eq_band, scale = 1/sqrt(2))
    bf = (post_dens / prior_dens) / ((1 - post_dens) / (1 - prior_dens))
  }
  if (i==granularity) {
    npower = 0
  } else {
  bf = 1
  j = length(t) + 1
  while (bf < thresh)
  {
    j = j-1
    upper = cdf_t(eq_band, t[j], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                   prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = tol)
    lower = cdf_t(-eq_band, t[j], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                   prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = tol)
    post_dens = upper - lower
    prior_dens = pcauchy(eq_band, scale = 1/sqrt(2)) - 
      pcauchy(-eq_band, scale = 1/sqrt(2))
    bf = (post_dens / prior_dens) / ((1 - post_dens) / (1 - prior_dens))
  }
  npower = pt(t[j], n1+n2-2, delta/sqrt(1/n1+1/n2)) - pt(t[i], n1+n2-2, delta/sqrt(1/n1+1/n2))
  }
  npower
}