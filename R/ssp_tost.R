#' Determine sample size with Two One‐Sided Test (TOST) method
#' 
#' The function determines the appropriate sample size for the
#' expected effect with the Two One‐Sided Test method (TOST).
#' TOST is a frequentist statistical testing approach aimed
#' at establishing equivalence between two groups.
#' 
#' @param tpr Numeric. The desired long run probability of obtaining a significant result with TOST, given Delta.
#' @param eq_band Numeric. The chosen width of the region for practical equivalence, i.e. the SESOI.
#' @param delta Numeric. The expected population effect size. In most cases, this value will be zero.
#' @param sigma Numeric. TODO
#' @param nr Numeric. TODO
#' @param alpha Numeric. The level of significance.
#' 
#' @return The function returns a list of three named numeric vectors. The
#'   sample size for group 1 `n1`, the sample size for group 2 `n2` and
#'   the associated power `npower`.
#' @export
#' @examples 
#' \dontrun{
#' SampleSizePlanner::ssp_tost(tpr = 0.8, eq_band = 0.2, delta = 0)
#' }
ssp_tost <- function(tpr, eq_band, delta, sigma = 1, nr = 1, alpha = .05) {
  # Validation of function arguments
  assertthat::assert_that(is_positive_number(tpr))
  assertthat::assert_that(is_positive_number(eq_band))
  assertthat::assert_that(is_not_null(delta))
  
  max_n = 10001
  n1 = 4
  sigsq = sigma^2
  numint = 1000
  lc = numint + 1
  cl = 1e-10
  coevecc = c(1, rep(c(4, 2), numint / 2 - 1), 4, 1)
  npower = 0
  
  while (npower < tpr & n1 < max_n) {
    n1 = n1 + 1
    n2 = nr * n1
    df = n1 + n2 - 2
    tcrit = qt(1 - alpha, df)
    nfac = 1 / n1 + 1 / n2
    var = sigsq * nfac
    std = sqrt(var)
    cu = (df * eq_band^2) / (var * tcrit^2)
    int = cu - cl
    intl = int / numint
    cvec = cl + intl * (0:numint)
    wcpdf = (intl / 3) * coevecc * dchisq(cvec, df)
    st = sqrt(cvec / df) * tcrit
    npower = sum(wcpdf * (pnorm((eq_band - delta) / std - st) - pnorm((-eq_band - delta) / std + st)))
  }
  
  if (npower == 0) {
    stop("Your chosen power level cannot be achieved for n < 10001!")
  } else {
    return(
      list(n1 = round(n1, 4),
           n2 = round(n2, 4),
           npower = round(npower, 4)))
  }
}
