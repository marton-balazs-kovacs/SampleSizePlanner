#' Determine sample size with Bayes Factor Design Analysis (BFDA)
#' 
#' The present method estimates the long-run rates of misleading
#' evidence that one can expect for a specific research design if
#' using preset Bayes Factor thresholds and allowing for sequential testing.
#' 
#' @param delta Numeric. The expected population effect size.
#' @param thresh Integer. The Bayes factor threshold for inference.
#' @param tpr Numeric. The long-run probability of obtaining a Bayes factor at least
#'   as high as the critical threshold favoring superiority, given Delta.
#' @param n_rep Integer. the number of simulations.
#' 
#' @return The function returns a list of four named numeric vectors.
#' The first `tpr` is the range of TPRs that were provided as a parameter.
#' The second `n1` is the range of determined sample sizes for the given design.
#' The third `h0` is the frequency of Bayes factor providing evidence with the
#' given threshold for the null hypothesis.
#' The fourth `ha` is the same as `h0` but for the alternative hypothesis. 
#' @export
#' @examples
#' \dontrun{
#' SampleSizePlanner::ssp_bfda(tpr = 0.8, delta = 0.5, thresh = 10, n_rep = 1000)
#' }
ssp_bfda <- function(delta, thresh = 10, tpr = 0.8, n_rep = 1000) {
  Ns = NULL
  BFs = NULL
  for (i in 1:n_rep) {
    n = 10
    Plac = rnorm(n, 0, 1)
    Treat = rnorm(n, delta, 1)
    BF = BayesFactor::ttest.tstat(
      t = t.test(Treat, Plac)$statistic,
      n1 = n,
      n2 = n, 
      rscale = 1 / sqrt(2),
      nullInterval = c(0, Inf),
      simple = T)
    
    while (BF > (1 / thresh) & BF < thresh & n < 1500) {
      n = n + 1
      Plac = c(Plac, rnorm(1, 0, 1))
      Treat = c(Treat, rnorm(1, delta, 1))
      BF = BayesFactor::ttest.tstat(
        t = t.test(Treat, Plac)$statistic,
        n1 = n,
        n2 = n, 
        rscale = 1 / sqrt(2),
        nullInterval = c(0, Inf),
        simple = T)
    }
    Ns[i] = n
    BFs[i] = BF
  }

  return(
    list(
      tpr = tpr,
      n1 = round(quantile(Ns, probs = tpr, names = FALSE)),
      h0 = mean(BFs < (1 / thresh)),
      ha = mean(BFs > thresh)
      )
    )
  }
