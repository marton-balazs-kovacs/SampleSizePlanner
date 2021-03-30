#' Function to plot the results of the power curve calculation
#' 
#' The function plots the power curve based on the results
#' of the \code{\link{ssp_power_curve}} function.
#' 
#' @param delta Numeric. The range of sample size provided for the \code{\link{ssp_power_curve}} function.
#' @param n1 Numeric. The determined sample sizes for each `delta`.
#' @param animated Logical. if TRUE the output plot is animated.
#' 
#' @return The function returns a `ggplot2` or a `plotly` object.
#' @export
#' @examples 
#' \dontrun{
#' # Determine the sample sizes for each delta
#' curve_data <- SampleSizePlanner::ssp_power_curve(tpr = 0.8, delta = seq(0.1, 0.9, 0.01), max_n = 5000)
#' # Plot the power curve
#' SampleSizePlanner::plot_power_curve(delta = curve_data$delta, n1 = curve_data$n1, animated = FALSE)
#' }
plot_power_curve <- function(delta, n1, animated = FALSE) {
  gg <- 
    ggplot2::ggplot() +
    ggplot2::aes(x = delta,
                 y = n1,
                 text = paste("n:", n1,
                              "<br>delta: ", delta)) +
    ggplot2::geom_point(shape = 21, colour = "black", fill = "white", size = 3, stroke = 0.5) +
    ggplot2::scale_y_continuous(limits = c(0, 1500),
                                labels = c("0", "500", "1000", "1500"),
                                breaks = c(0, 500, 1000, 1500)) +
    ggplot2::scale_x_continuous(limits = c(0, max(delta))) +
    ggplot2::labs(x = "Delta",
                  y = "Planned sample size") +
    ggplot2::theme_classic()
  if (animated) {
    gg %>% 
      plotly::ggplotly(tooltip = "text")
  } else {
    gg
  }
}