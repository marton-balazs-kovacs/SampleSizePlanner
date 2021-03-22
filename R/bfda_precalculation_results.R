#' Bayes Factor Design Analysis Precalculation Results
#' 
#' Dataset that contains precalculated sample sizes with the
#' BFDA method. We ran each calculation with 10000 iteration. The
#' dataset contains the input and output values of these calculations.
#' 
#' @section Remark:
#'   If none of the precalculated values suit your sample size
#'   determination plan, than feel free the calculations with the R package
#'   by using the \code{\link{ssp_bfda}} function.
#' 
#' @format A dataframe with 1230 rows and 9 variables:
#' \describe{
#'   \item{iterate}{numeric, unique id of iteration}
#'   \item{tpr}{numeric, The long-run probability of obtaining a Bayes factor at least as high as the critical threshold favoring superiority, given Delta.}
#'   \item{delta}{numeric, The expected population effect size.}
#'   \item{thresh}{numeric, The Bayes factor threshold for inference. Either 3, 6 or 10.}
#'   \item{n1}{numeric, The determined sample size per group.}
#'   \item{tpr_out}{numeric, The TPR associated with the resulting sample sizes.}
#'   \item{h0}{numeric, The frequency of the Bayes factor providing evidence for the null hypothesis with the given threshold.}
#'   \item{ha}{numeric, The frequency of the Bayes factor providing evidence for the alternative hypothesis with the given threshold.}
#'   \item{error_message}{character, The error message in case of an error.}
#'}
"bfda_precalculation_results"