#' Bayesian ANOVA Pre calculation Results
#' 
#' Data set that contains pre calculated sample sizes with the
#' Bayesian ANOVA for 2*2 designs. 1000 iterations per conditions were used.
#' The maximum sample size per group was set to 500.
#' The data set contains the input and output values of these calculations.
#' 
#' @section Remark:
#'   If none of the pre calculated values suit your sample size
#'   determination plan, than feel free the calculations with the R package
#'   by using the \code{\link{ssp_anova_bf}} function.
#' 
#' @format A dataframe with 5550 rows and 17 variables:
#' \describe{
#'   \item{filename}{character, The name of the file.}
#'   \item{batch_id}{numeric, unique id of batch in which the calculation was run parallel}
#'   \item{calculation_id}{numeric, unique id of calculation within the batch}
#'   \item{n1}{numeric, The determined sample size per group.}
#'   \item{tpr_out}{numeric, The desired long-run probability of the HDI fully falling inside the ROPE, given the means.}
#'   \item{tpr_in}{numeric, The desired long-run probability of the HDI fully falling inside the ROPE, given the means.}
#'   \item{prior_scale}{numeric, The scale of the Cauchy prior which is fixed to 1 / sqrt(2) in the ShinyApp.}
#'   \item{error_message}{character, The error message in case of an error.}
#'   \item{result_not_null}{logical, Whether the precalculation led to an error or not.}
#'   \item{effect}{character, The effect of interest (main effect A, main effect B).}
#'   \item{mu}{numeric, The mean of the DV for each group.}
#'   \item{thresh}{numeric, The threshold of the Bayes Factor which is fixed to 10 in the ShinyApp.}
#'   \item{prior_location}{numeric, The location of the Cauchy prior which is fixed to 0.}
#'   \item{sigma}{numeric, The standard deviation of the DV for the groups.}

#'}
"bayes_anova_data"