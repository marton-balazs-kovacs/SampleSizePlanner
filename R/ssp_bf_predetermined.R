ssp_bf_predetermined <- function(tpr = 0.8, delta, thresh, max_n = 5000) {
  n1 <- NULL
  npower <- NULL
  for (i in 1:length(delta))
  {
    res = power_optim(fun = super_bf, range = c(5, max_n),  delta = delta[i], tpr = tpr, thresh = thresh)
    n1[i] = res$n1
    npower[i] = res$npower
  }
  list(
    delta = delta,
    n1 = n1,
    npower = npower
  )
}

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