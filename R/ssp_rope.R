#' Determine sample size with Region of Practical Equivalence (ROPE) method
#' 
#' The ROPE procedure identifies the 95% highest density interval
#' (HDI; other percentages are permissible as well) and determines
#' whether or not the HDI is fully contained within the equivalence interval.
#' 
#' @param tpr Numeric. The desired long run probability of having the HDI fully contained within the ROPE interval, given Delta.
#' @param delta Numeric. The expected population effect size.
#' @param eq_band Numeric. The chosen ROPE interval.
#' @param alpha Numeric. The level of significance.
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
ssp_rope <- function(tpr, eq_band, delta, alpha = .05, tol = 1e-4, granularity = 300) {
  est <- ssp_tost(tpr = tpr, eq_band = eq_band, delta = delta) %>% purrr::pluck("n1")
  result <- power_optim(fun = rope, range = round(est * c(0.5, 1.5)), delta = delta, tpr = tpr, eq_band = eq_band, alpha = alpha, tol = tol, granularity = granularity)
  result
}

rope <- function(n1, delta, eq_band, alpha, tol, granularity) {
  n2 = n1
  t = seq(qt(.005, n1+n2-2, delta/sqrt(1/n1+1/n2)), 
          qt(.995, n1+n2-2, delta/sqrt(1/n1+1/n2)), length = granularity)
  upper = 0.5
  lower = 0.5
  i = 0
  while (!(lower<(alpha/2) & upper>(1-alpha/2)) & i < granularity) {
    i = i+1
    upper = cdf_t(eq_band, t[i], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                  prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = tol)
    lower = cdf_t(-eq_band, t[i], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                  prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = tol)
  }
  if (i == granularity) {
    npower = 0
    } else {
      upper = 0.5
      lower = 0.5
      j = length(t) + 1
      
      while (!(lower<(alpha/2) & upper>(1-alpha/2))) {
        j = j-1
        upper = cdf_t(eq_band, t[j], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                      prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = tol)
        lower = cdf_t(-eq_band, t[j], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                      prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = tol)
        }
  
      npower = pt(t[j], n1+n2-2, delta/sqrt(1/n1+1/n2)) - pt(t[i], n1+n2-2, delta/sqrt(1/n1+1/n2))
      }
  npower
}