
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SampleSizePlanner

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of SampleSizePlanner is to …

## Usage

SampleSizePlanner can be used either via the web app or via R.

### Using the web app

You can use the app at
<https://martonbalazskovacs.shinyapps.io/SampleSizePlanner/>.

You can alternatively run the app locally on your own computer by
following these instructions:

Install the development version (SampleSizePlanner is not available from
CRAN) from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("marton-balazs-kovacs/SampleSizePlanner")
```

Running the app.

``` r
SampleSizePlanner::run_app()
```

### Using the package

You can use the package after installation from R. The package can be
used to determine the sample sizes with the nine different methods
presented in the paper. Using the package allows users bigger freedom as
more parameters could be set as in the app. However, the justification
reports cannot be created with the package. Please refer to the paper if
you would like to see example justifications for each method.

## Contribution

We are open to new ideas and feature requests.

Please note that the ‘SampleSizePlanner’ project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to
this project, you agree to abide by its terms.
