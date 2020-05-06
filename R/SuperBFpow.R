SuperBFpow <- function (n1, Band, delta, thresh = 10, Tol = 1e-4, granularity = 300) {
  n2 = n1
  t = seq(qt(.005, n1+n2-2, delta/sqrt(1/n1+1/n2)), 
          qt(.995, n1+n2-2, delta/sqrt(1/n1+1/n2)), length = granularity)
  BF = 1
  i = 0
  while (BF<thresh)
  {
    i = i+1
    BF = exp(BayesFactor::ttest.tstat(t = t[i], n1 = n1, n2 = n2, nullInterval = c(0, Inf), rscale = 1/sqrt(2))[['bf']])
  }
  BF = 1
  j = length (t)+1
  while (BF<thresh)
  {
    j = j-1
    BF = exp(BayesFactor::ttest.tstat(t = t[j], n1 = n1, n2 = n2, nullInterval = c(0, Inf), rscale = 1/sqrt(2))[['bf']])
  }
  npower = pt(t[j], n1+n2-2, delta/sqrt(1/n1+1/n2))-
    pt(t[i], n1+n2-2, delta/sqrt(1/n1+1/n2))

  output <-
    tibble::tibble(n1 = npower[1],
                   npower = npower[2])
  
  return(output)
}
