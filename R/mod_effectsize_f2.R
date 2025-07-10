#' mod_effectsize_f2_ui and mod_effectsize_f2_server
#' @description Submodule for displaying computed f2 effect size.
#' @noRd

mod_effectsize_f2_ui <- function(id) {
  ns <- NS(id)
  tags$div(
    style = "margin-top:1px; margin-bottom: 10px;",
    textOutput(ns("f2"))
  )
}

mod_effectsize_f2_server <- function(id, mu, sigma, effect) {
  moduleServer(id, function(input, output, session) {
    output$f2 <- renderText({
      req(all(is.numeric(as.vector(mu()))) && sigma() > 0)
      f2 <- get_f2(mu = as.vector(mu()), sigma = sigma())
      eff <- effect()
      f2_out <- switch(
        eff,
        "Main Effect 1" = f2[1],
        "Main Effect 2" = f2[2],
        f2[3]  # fallback to interaction
      )
      paste("Respective effect size is f2 =", round(f2_out, 2))
    })
  })
}
