#' Determine sample size with Bayes Factor Design Analysis (BFDA)
#' 
#' lorem ipsum
#' 
#' @param delta Numeric. the expected population effect size
#' @param thresh Integer. the Bayes factor threshold for inference
#' @param tpr Numeric. the long-run probability of obtaining a Bayes factor at least
#'   as high as the critical threshold favoring superiority, given Delta
#' @param n_rep Integer. the number of simulations
#' 
#' @return A list of two numeric values.
#' @export
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
