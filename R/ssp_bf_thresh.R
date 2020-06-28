super_bf <- function(n1, delta, thresh = 10, tol = 1e-4, granularity = 300) {
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

ssp_bf_thresh <- function(opt, band, delta, thresh) {
  est <- ssp_tost(opt = opt, band = band, delta = delta)[1] %>% purrr::flatten_dbl()
  power_optim(fun = super_bf, range = c(50, est), delta = delta, opt = opt, thresh = thresh)
}