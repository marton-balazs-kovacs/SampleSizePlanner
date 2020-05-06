SeqBFpow <- function (delta, thresh = 10, power = 0.8, nRep = 1000) {
  Ns = BFs = c()
  for (i in 1:nRep)
  {
    cat ("\n", i)
    n1 = n2 = 4
    Plac = rnorm (n1, 0, 1)
    Treat = rnorm (n2, delta, 1)
    BF = as.vector (ttestBF (x = Treat, y = Plac, nullInterval = c(0, Inf), rscale = 1/sqrt(2)))[1]
    
    while (BF<thresh & BF>(1/thresh) & n1<1500)
    {
      n1 = n2 = n1+1
      Plac = c(Plac, rnorm (1, 0, 1))
      Treat = c(Treat, rnorm (1, delta, 1))
      BF = as.vector (ttestBF (x = Treat, y = Plac, nullInterval = c(0, Inf), rscale = 1/sqrt(2)))[1]
    }
    Ns[i] = n1
    BFs[i] = BF
  }
  nopt = sort(Ns)[round (length(Ns)*power)]
  BFHA = mean (BFs>thresh)
  cat ("\n n1 is ", nopt, ", power is ", power, 
       ", the proportion of times HA is supported is ", BFHA, sep = "")
  c(nopt, BFHA)
}