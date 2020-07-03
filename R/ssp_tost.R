ssp_tost <- function(opt, band, delta, sigma = 1, nr = 1, alpha = .05, report_text = FALSE) {
  n1 = 4
  sigsq = sigma^2
  numint = 1000
  lc = numint + 1
  cl = 1e-10
  coevecc = c(1, rep(c(4, 2), numint / 2 - 1), 4, 1)
  npower = 0
  
  while (npower < opt & n1 < 10001) {
    n1 = n1 + 1
    n2 = nr * n1
    df = n1 + n2 - 2
    tcrit = qt(1 - alpha, df)
    nfac = 1/n1 + 1/n2
    var = sigsq * nfac
    std = sqrt(var)
    cu = (df * band^2) / (var * tcrit^2)
    int = cu - cl
    intl = int / numint
    cvec = cl + intl * (0:numint)
    wcpdf = (intl / 3) * coevecc * dchisq(cvec, df)
    st = sqrt(cvec / df) * tcrit
    npower = sum(wcpdf * (pnorm((band - delta) /std - st) - pnorm((-band - delta) / std + st)))
  }
  
  if (report_text) {
    glue::glue("In order to calculate the sample size we choose the Two One-Sided Tests of Equivalence (TOST; REF) method. We choose the power to be {opt} because ... The expected delta was {delta} as ... Our bandwidht was {band} respectively. The estimated sample sizes were {purrr::pluck(result, \"n1\")} and {purrr::pluck(result, \"n2\")} with {purrr::pluck(result, \"npower\")} estimated power.")
  } else {
    list(n1 = round(n1, 4),
         n2 = round(n2, 4),
         npower = round(npower, 4))
  }
}
