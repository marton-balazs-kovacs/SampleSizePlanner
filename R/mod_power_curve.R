# Module UI
  
#' @title   mod_power_curve_ui and mod_power_curve_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_power_curve
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_power_curve_ui <- function(id){
  tagList(
    sidebarPanel(
      h1("Power curve"),
      sliderInput(NS(id, "opt"), "Power", min = 0, max = 1, value = 0.8, step = 0.1),
      sliderInput(NS(id, "delta"), "Delta", min = 0, max = 1, value = c(0.1, 0.9), step = 0.1),
      actionButton(NS(id, "calculate"), "Calculate sample size ")),
    mainPanel(
      plotly::plotlyOutput(NS(id, "power_curve_plot"))
      # div(id = "download_power_curve",
      #     downloadButton(NS(id, "report")))
    )
  )
}
    
# Module Server
    
#' @rdname mod_power_curve
#' @export
#' @keywords internal
    
mod_power_curve_server <- function(id, activate_menu, method_menu, activate_question, method_question){

    moduleServer(id, function(input, output, session) {
      
      waitress <- waiter::Waitress$new("#tost_ui_1-tost_output", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)
      
      observe({
        if (input$calculate) {
          shinyjs::enable("report")
          shinyjs::runjs("$('#power_curve_plot').removeAttr('title');")
        } else{
          shinyjs::disable("report")
          shinyjs::runjs("$('#power_curve_plot').attr('title', 'Please run the calculation first');")
        }
      })
      
      power_curve_result <- eventReactive(input$calculate, {
        Band <- NA
        noptim <- NULL
        delta <- seq(input$delta[1], input$delta[2], 0.01)
        for (i in 1:length(delta)) {
          noptim[i] = PowerOptim(Fun = Tpow, Range = c(5, 2000), Arguments = c(Band, delta[i]), Opt = input$opt)$n1
          }
        return(
          list(
            noptim = noptim,
            delta = delta
            )
          )
        })

      # params <- eventReactive(input$calculate, {
      #   list(
      #   )
      # })
      
    output$power_curve_plot <- plotly::renderPlotly({
     gg <- ggplot2::ggplot() +
        ggplot2::aes(x = power_curve_result()$delta,
                     y = power_curve_result()$noptim,
                     text = paste("n:", power_curve_result()$noptim,
                                  "<br>delta: ", power_curve_result()$delta)) +
        ggplot2::geom_point(shape = 21, colour = "black", fill = "white", size = 3, stroke = 0.5) +
        ggplot2::scale_y_continuous(limits = c(0, 1500),
                                    labels = c("0", "500", "1000", "1500"),
                                    breaks = c(0, 500, 1000, 1500)) +
        ggplot2::scale_x_continuous(limits = c(0, 1),
                                    labels = c("0.1", "0.3", "0.5", "0.7", "0.9"),
                                    breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
        ggplot2::labs(x = "Delta",
                      y = "Planned sample size") +
        ggplot2::theme_classic()
     
     plotly::ggplotly(gg, tooltip = "text") %>%
       plotly::config(displayModeBar = F) 

    })
    
  })
}
    
## To be copied in the UI
# mod_power_curve_ui("power_curve_ui_1")
    
## To be copied in the server
# mod_power_curve_server("power_curve_ui_1")
 
