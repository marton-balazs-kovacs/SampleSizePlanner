EqBFpow = function (n1, Band, delta, thresh = 10, Tol = 1e-4, granularity = 300)
{
  n2 = n1
  t = seq (qt (.005, n1+n2-2, delta/sqrt(1/n1+1/n2)), 
           qt (.995, n1+n2-2, delta/sqrt(1/n1+1/n2)), length = granularity)
  BF = 1
  i = 0
  while (BF<thresh)
  {
    i = i+1
    Upper = cdf_t (Band, t[i], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                   prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = Tol)
    Lower = cdf_t (-Band, t[i], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                   prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = Tol)
    PostDens = Upper - Lower
    PriorDens = pcauchy (Band, scale = 1/sqrt(2)) - 
      pcauchy (-Band, scale = 1/sqrt(2))
    BF = (PostDens / PriorDens) / ((1-PostDens) / (1-PriorDens))
  }
  BF = 1
  j = length (t)+1
  while (BF<thresh)
  {
    j = j-1
    Upper = cdf_t (Band, t[j], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                   prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = Tol)
    Lower = cdf_t (-Band, t[j], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                   prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = Tol)
    PostDens = Upper - Lower
    PriorDens = pcauchy (Band, scale = 1/sqrt(2)) - 
      pcauchy (-Band, scale = 1/sqrt(2))
    BF = (PostDens / PriorDens) / ((1-PostDens) / (1-PriorDens))
  }
  npower = pt(t[j], n1+n2-2, delta/sqrt(1/n1+1/n2))-
    pt(t[i], n1+n2-2, delta/sqrt(1/n1+1/n2))
  
  # cat ("\n n1 is ", n1, ", power is ", npower, sep = "")
  npower
}