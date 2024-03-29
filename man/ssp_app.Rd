% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ssp_app.R
\name{ssp_app}
\alias{ssp_app}
\title{Determine sample size with A Priori Precision (APP) method}
\usage{
ssp_app(confidence, closeness)
}
\arguments{
\item{confidence}{Numeric. The desired probability of obtaining the sample mean with the desired closeness to the population mean.}

\item{closeness}{Numeric. The desired closeness of the sample mean to the population mean defined in standard deviation.}
}
\value{
The function returns a list of one named element. The determined
sample size for group one and two called `n1`.
}
\description{
The approach aims to plan a sample size based on how close
the researcher wishes both sample means to be to their
respective population parameter, and how confident the researcher
wants to be in this.
}
\examples{
\dontrun{
SampleSizePlanner::ssp_app(closeness = 0.2, confidence = 0.95)
}
}
