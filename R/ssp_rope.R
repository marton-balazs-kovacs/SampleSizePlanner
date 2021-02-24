rope <- function(n1, delta, band, alpha = .05, tol = 1e-4, granularity = 300) {
  n2 = n1
  t = seq(qt(.005, n1+n2-2, delta/sqrt(1/n1+1/n2)), 
          qt(.995, n1+n2-2, delta/sqrt(1/n1+1/n2)), length = granularity)
  upper = lower = 0.5
  i = 0
  while (!(lower<(alpha/2) & upper>(1-alpha/2))) {
    i = i+1
    upper = cdf_t(band, t[i], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                  prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = tol)
    lower = cdf_t(-band, t[i], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                  prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = tol)
  }
  upper = lower = 0.5
  j = length(t) + 1
  while (!(lower<(alpha/2) & upper>(1-alpha/2))) {
    j = j-1
    upper = cdf_t(band, t[j], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                  prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = tol)
    lower = cdf_t(-band, t[j], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                  prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = tol)
  }
  
  npower = pt(t[j], n1+n2-2, delta/sqrt(1/n1+1/n2)) - pt(t[i], n1+n2-2, delta/sqrt(1/n1+1/n2))
  
  npower
}

#' SSP ROPE
#' 
#' @export
ssp_rope <- function(opt, band, delta) {
  est <- ssp_tost(opt = opt, band = band, delta = delta) %>% purrr::pluck("n1")
  result <- power_optim(fun = rope, range = round(est * c(0.5, 1.5)), delta = delta, opt = opt, band = band)
  result
}
