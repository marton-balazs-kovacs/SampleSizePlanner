eq_bf <- function(n1, band, delta, thresh = 10, tol = 1e-4, granularity = 300) {
  n2 = n1
  t = seq(qt(.005, n1+n2-2, delta/sqrt(1/n1+1/n2)), 
          qt(.995, n1+n2-2, delta/sqrt(1/n1+1/n2)), length = granularity)
  bf = 1
  i = 0
  while (bf < thresh)
  {
    i = i + 1
    upper = cdf_t(band, t[i], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                   prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = tol)
    lower = cdf_t(-band, t[i], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                   prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = tol)
    post_dens = upper - lower
    prior_dens = pcauchy(band, scale = 1/sqrt(2)) - 
      pcauchy(-band, scale = 1/sqrt(2))
    bf = (post_dens / prior_dens) / ((1 - post_dens) / (1 - prior_dens))
  }
  bf = 1
  j = length(t) + 1
  while (bf < thresh)
  {
    j = j-1
    upper = cdf_t(band, t[j], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                   prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = tol)
    lower = cdf_t(-band, t[j], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                   prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = tol)
    post_dens = upper - lower
    prior_dens = pcauchy(band, scale = 1/sqrt(2)) - 
      pcauchy(-band, scale = 1/sqrt(2))
    bf = (post_dens / prior_dens) / ((1 - post_dens) / (1 - prior_dens))
  }
  npower = pt(t[j], n1+n2-2, delta/sqrt(1/n1+1/n2)) - pt(t[i], n1+n2-2, delta/sqrt(1/n1+1/n2))
  
  npower
}

ssp_eq_bf <- function(opt, band, delta, thresh) {
  est <- ssp_tost(opt = opt, band = band, delta = delta)[1] %>% purrr::flatten_dbl()
  power_optim(fun = eq_bf, range = c(50, est), delta = delta, opt = opt, band = band, thresh = thresh)
}