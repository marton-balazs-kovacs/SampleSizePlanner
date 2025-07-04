% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/informed_t_test_functions.R
\name{informed_t_test_functions}
\alias{informed_t_test_functions}
\alias{integrand_t}
\alias{posterior_t}
\alias{cdf_t}
\alias{quantile_t}
\alias{ciPlusMedian_t}
\alias{posterior_normal}
\alias{cdf_normal}
\alias{quantile_normal}
\alias{ciPlusMedian_normal}
\alias{bf10_t}
\alias{bf10_normal}
\title{Informed t-test functions}
\usage{
integrand_t(delta, t, n, nu, mu.delta, gamma, kappa)

posterior_t(
  delta,
  t,
  n1,
  n2 = NULL,
  independentSamples = FALSE,
  prior.location,
  prior.scale,
  prior.df,
  rel.tol = .Machine$double.eps^0.25
)

cdf_t(
  x,
  t,
  n1,
  n2 = NULL,
  independentSamples = FALSE,
  prior.location,
  prior.scale,
  prior.df,
  rel.tol = .Machine$double.eps^0.25
)

quantile_t(
  q,
  t,
  n1,
  n2 = NULL,
  independentSamples = FALSE,
  prior.location,
  prior.scale,
  prior.df,
  tol = 1e-04,
  max.iter = 100,
  rel.tol = .Machine$double.eps^0.25
)

ciPlusMedian_t(
  t,
  n1,
  n2 = NULL,
  independentSamples = FALSE,
  prior.location,
  prior.scale,
  prior.df,
  ci = 0.95,
  type = "two-sided",
  tol = 1e-04,
  max.iter = 100,
  rel.tol = .Machine$double.eps^0.25
)

posterior_normal(
  delta,
  t,
  n1,
  n2 = NULL,
  independentSamples = FALSE,
  prior.mean,
  prior.variance
)

cdf_normal(
  x,
  t,
  n1,
  n2 = NULL,
  independentSamples = FALSE,
  prior.mean,
  prior.variance,
  rel.tol = .Machine$double.eps^0.25
)

quantile_normal(
  q,
  t,
  n1,
  n2 = NULL,
  independentSamples = FALSE,
  prior.mean,
  prior.variance,
  tol = 1e-04,
  max.iter = 100,
  rel.tol = .Machine$double.eps^0.25
)

ciPlusMedian_normal(
  t,
  n1,
  n2 = NULL,
  independentSamples = FALSE,
  prior.mean,
  prior.variance,
  ci = 0.95,
  type = "two-sided",
  tol = 1e-04,
  max.iter = 100,
  rel.tol = .Machine$double.eps^0.25
)

bf10_t(
  t,
  n1,
  n2 = NULL,
  independentSamples = FALSE,
  prior.location,
  prior.scale,
  prior.df,
  rel.tol = .Machine$double.eps^0.25
)

bf10_normal(
  t,
  n1,
  n2 = NULL,
  independentSamples = FALSE,
  prior.mean,
  prior.variance,
  rel.tol = .Machine$double.eps^0.25
)
}
\description{
Functions created to calculate Bayes factor with informed priors
for t-tests. The functions were created for the
Gronau, Q. F., Ly, A., & Wagenmakers, E. J. (2019).
Informed Bayesian t-tests. \emph{The American Statistician.} paper.
The functions are retrieved from \href{https://osf.io/bsp6z/}{OSF}.
}
\keyword{internal}
