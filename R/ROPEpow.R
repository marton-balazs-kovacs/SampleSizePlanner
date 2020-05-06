ROPEpow <- function (n1, Band, delta, alpha = .05, Tol = 1e-4, granularity = 300)
{
  n2 = n1
  t = seq (qt (.005, n1+n2-2, delta/sqrt(1/n1+1/n2)), 
           qt (.995, n1+n2-2, delta/sqrt(1/n1+1/n2)), length = granularity)
  Upper = Lower = 0.5
  i = 0
  while (!(Lower<(alpha/2) & Upper>(1-alpha/2)))
  {
    i = i+1
    Upper = cdf_t(Band, t[i], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                   prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = Tol)
    Lower = cdf_t(-Band, t[i], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                   prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = Tol)
  }
  Upper = Lower = 0.5
  j = length (t)+1
  while (!(Lower<(alpha/2) & Upper>(1-alpha/2)))
  {
    j = j-1
    Upper = cdf_t(Band, t[j], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                   prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = Tol)
    Lower = cdf_t(-Band, t[j], n1 = n1, n2 = n2, independentSamples = T, prior.location = 0, 
                   prior.scale = 1/sqrt(2), prior.df = 1, rel.tol = Tol)
  }
  npower = pt(t[j], n1+n2-2, delta/sqrt(1/n1+1/n2))-
    pt(t[i], n1+n2-2, delta/sqrt(1/n1+1/n2))
  
  # cat ("\n n1 is ", n1, ", power is ", npower, sep = "")
  npower
}