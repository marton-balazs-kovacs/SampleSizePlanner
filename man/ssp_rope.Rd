% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ssp_rope.R
\name{ssp_rope}
\alias{ssp_rope}
\alias{rope}
\title{Determine sample size with Region of Practical Equivalence (ROPE) method}
\usage{
ssp_rope(
  tpr,
  eq_band,
  delta,
  alpha = 0.05,
  tol = 1e-04,
  granularity = 300,
  prior_location = 0,
  prior_scale = 1/sqrt(2)
)

rope(n1, delta, eq_band, alpha, tol, granularity, prior_location, prior_scale)
}
\arguments{
\item{tpr}{Numeric. The desired long run probability of having the HDI fully contained within the ROPE interval, given Delta.}

\item{eq_band}{Numeric. The chosen ROPE interval.}

\item{delta}{Numeric. The expected population effect size.}

\item{alpha}{Numeric. The level of significance.}

\item{tol}{Numeric. Relative accuracy requested.}

\item{granularity}{Numeric. Relative precision of the tpr estimates, higher values mean more precision.}

\item{prior_location}{Numeric. Location of the Cauchy prior distribution.}

\item{prior_scale}{Numeric. Scale of the Cauchy prior distribution.}
}
\value{
The function returns a list of three named numeric vectors.
The first `delta` is the range of deltas provided for the function.
The second `n1` the determined sample size per group.
The third `tpr_out` is the TPR corresponding to the determined sample sizes
with the given delta.
}
\description{
The ROPE procedure identifies the 95% highest density interval
(HDI; other percentages are permissible as well) and determines
whether or not the HDI is fully contained within the equivalence interval.
}
\examples{
\dontrun{
SampleSizePlanner::ssp_rope(tpr = 0.8, eq_band = 0.2, delta = 0, thresh = 10, prior_scale = 1/sqrt(2))
}
}
