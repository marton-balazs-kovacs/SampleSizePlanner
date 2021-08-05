#' Determine sample size with Bayes Factor Design Analysis (BFDA)
#' 
#' The present method provides an expected sample size such that
#' compelling evidence in the form of a Bayes factor can be collected
#' for a given effect size with a certain long-run probability when
#' allowing for sequential testing.
#' 
#' @param delta Numeric. The expected population effect size.
#' @param thresh Integer. The Bayes factor threshold for inference.
#' @param tpr Numeric. The long-run probability of obtaining a Bayes factor at least
#'   as high as the critical threshold favoring superiority, given Delta.
#' @param n_rep Integer. The number of simulations.
#' @param prior_scale Numeric. Scale of the Cauchy prior distribution.
#' @param max_n Integer. The maximum number of participants per group (both groups are assumed to have equal sample size).
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
ssp_bfda <- function(tpr = 0.8, delta, thresh = 10, n_rep = 1000, prior_scale = 1 / sqrt(2), max_n = 1500) {
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
      rscale = prior_scale,
      nullInterval = c(0, Inf),
      simple = T)
    
    while (BF > (1 / thresh) & BF < thresh & n < max_n) {
      n = n + 1
      Plac = c(Plac, stats::rnorm(1, 0, 1))
      Treat = c(Treat, stats::rnorm(1, delta, 1))
      BF = BayesFactor::ttest.tstat(
        t = t.test(Treat, Plac)$statistic,
        n1 = n,
        n2 = n, 
        rscale = prior_scale,
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
