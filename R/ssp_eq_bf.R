#' Determine sample size with Equivalence interval Bayes factor method
#' 
#' A Bayesian statistical testing approach aimed at establishing
#' equivalence between two groups.
#' 
#' @param tpr Numeric. The desired long-run probability of obtaining a Bayes factor at least as high as the Threshold, given Delta.
#' @param delta Numeric. The expected population effect size.
#' @param eq_band Numeric. The chosen width of the equivalence region.
#' @param thresh Integer. Critical threshold for the Bayes factor.
#' @param tol Numeric. Relative accuracy requested.
#' @param granularity Numeric. Relative precision of the tpr estimates, higher values mean more precision.
#' @param prior_location Numeric. Location of the Cauchy prior distribution.
#' @param prior_scale Numeric. Scale of the Cauchy prior distribution.
#' 
#' @return The function returns a list of two named numeric vectors.
#' The first `n1`  the determined sample size per group.
#' The third `npower` is the TPR corresponding to the determined sample sizes.
#' @export
#' @examples
#' \dontrun{
#' SampleSizePlanner::ssp_eq_bf(tpr = 0.8, delta = 0, eq_band = 0.2, thresh = 10)
#' }
ssp_eq_bf <- function(tpr, eq_band, delta, thresh = 10, tol = 1e-4, granularity = 300, prior_location = 0, prior_scale = 1/sqrt(2)) {
  est <- ssp_tost(tpr = tpr, eq_band = eq_band, delta = delta) %>% purrr::pluck("n1")
  result <- power_optim(
    fun = eq_bf,
    range = c(10, est),
    delta = delta,
    tpr = tpr,
    eq_band = eq_band,
    thresh = thresh,
    tol = tol,
    granularity = granularity,
    prior_location = prior_location,
    prior_scale = prior_scale)
  result
}

#' @rdname ssp_eq_bf
#' @importFrom stats qt pt pcauchy
eq_bf <- function(n1, eq_band, delta, thresh, tol, granularity, prior_location, prior_scale) {
  n2 = n1
  t = seq(stats::qt(.005, n1+n2-2, delta/sqrt(1/n1+1/n2)), 
          stats::qt(.995, n1+n2-2, delta/sqrt(1/n1+1/n2)), length = granularity)
  bf = 1
  i = 0
  while (bf < thresh & i < granularity) {
    i = i + 1
    upper = cdf_t(eq_band, t[i], n1 = n1, n2 = n2, independentSamples = T, prior.location = prior_location, 
                   prior.scale = prior_scale, prior.df = 1, rel.tol = tol)
    lower = cdf_t(-eq_band, t[i], n1 = n1, n2 = n2, independentSamples = T, prior.location = prior_location, 
                   prior.scale = prior_scale, prior.df = 1, rel.tol = tol)
    post_dens = upper - lower
    prior_dens = stats::pcauchy(eq_band, location = prior_location, scale = prior_scale) - stats::pcauchy(-eq_band, location = prior_location, scale = prior_scale)
    bf = (post_dens / prior_dens) / ((1 - post_dens) / (1 - prior_dens))
    }
  if (i==granularity) {
    npower = 0
    } else {
      bf = 1
      j = length(t) + 1
      while (bf < thresh) {
        j = j-1
        upper = cdf_t(eq_band, t[j], n1 = n1, n2 = n2, independentSamples = T, prior.location = prior_location, 
                      prior.scale = prior_scale, prior.df = 1, rel.tol = tol)
        lower = cdf_t(-eq_band, t[j], n1 = n1, n2 = n2, independentSamples = T, prior.location = prior_location, 
                      prior.scale = prior_scale, prior.df = 1, rel.tol = tol)
        post_dens = upper - lower
        prior_dens = stats::pcauchy(eq_band, location = prior_location, scale = prior_scale) - stats::pcauchy(-eq_band, location = prior_location, scale = prior_scale)
        bf = (post_dens / prior_dens) / ((1 - post_dens) / (1 - prior_dens))
        }
      npower = stats::pt(t[j], n1+n2-2, delta/sqrt(1/n1+1/n2)) - stats::pt(t[i], n1+n2-2, delta/sqrt(1/n1+1/n2))
      }
  npower
}