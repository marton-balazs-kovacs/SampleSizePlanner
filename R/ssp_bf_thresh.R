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

ssp_bf_thresh <- function(opt = 0.8, band, delta, thresh, report_text = FALSE) {
  est <- ssp_tost(opt = opt, band = band, delta = delta) %>% purrr::pluck(., "n1")
  result <- power_optim(fun = super_bf, range = c(50, est), delta = delta, opt = opt, thresh = thresh)
  
  if (report_text) {
    glue::glue("In order to calculate the sample size we choose the non-sequential Bayes factor theshold (REF) method. We choose the power to be {opt} because ... The expected delta was {delta} as ... Our threshold was {thresh} respectively. The estimated sample size was {purrr::pluck(result, \"n1\")} with {purrr::pluck(result, \"npower\")} estimated power.")
  } else {
    result
  }
}