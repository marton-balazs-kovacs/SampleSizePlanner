ssp_power_curve <- function(delta1, delta2, opt, animated = FALSE) {
  noptim <- NULL
  delta <- seq(delta1, delta2, 0.01)
  for (i in 1:length(delta)) {
    noptim[i] = power_optim(fun = traditional, range = c(5, 2000), delta[i], opt = opt)$n1
    }
  gg <- ggplot2::ggplot() +
    ggplot2::aes(x = delta,
                 y = noptim,
                 text = paste("n:", noptim,
                              "<br>delta: ", delta)) +
    ggplot2::geom_point(shape = 21, colour = "black", fill = "white", size = 3, stroke = 0.5) +
    ggplot2::scale_y_continuous(limits = c(0, 1500),
                                labels = c("0", "500", "1000", "1500"),
                                breaks = c(0, 500, 1000, 1500)) +
    ggplot2::scale_x_continuous(limits = c(0, delta2),
                                labels = as.character(delta),
                                breaks = delta) +
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
