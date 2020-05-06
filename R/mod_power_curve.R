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
  
  )
}
    
# Module Server
    
#' @rdname mod_power_curve
#' @export
#' @keywords internal
    
mod_power_curve_server <- function(id, activate_menu, method_menu, activate_question, method_question){

    moduleServer(id, function(input, output, session) {
    modal <- function() {
      
      modalDialog(
        footer = actionButton(NS(id, "close_modal"), label = "Close"),
        size = "m",
        h1("Power curve"),
        sliderInput(NS(id, "opt"), "Power", min = 0, max = 1, value = 0.8, step = 0.1),
        sliderInput(NS(id, "delta"), "Delta", min = 0, max = 1, value = c(0.1, 0.9), step = 0.1),
        actionButton(NS(id, "calculate"), "Ready, set, go!"),
        plotOutput(NS(id, "power_curve_plot"))
      )
    }
    
    observeEvent(activate_menu(), {
      
      if(method_menu() == "Power curve") {
        showModal(modal())}
      
    })
    
    observeEvent(activate_question(), {
      
      if(method_question() == "Power curve") {
        showModal(modal())}
      
    })
    
    observeEvent(input$close_modal, {
      
      removeModal()
      
    })
    
    power_curve_result <- eventReactive(input$calculate, {
      Band <- NA
      Noptim <- NULL
      delta <- seq(input$delta[1], input$delta[2], 0.01)
      for (i in 1:length(delta)) {
        Noptim[i] = PowerOptim(Fun = Tpow, Range = c(5, 2000), Arguments = c(Band, delta[i]), Opt = input$opt)[1]
      }
      
      return(
        list(noptim = Noptim,
             delta = delta)
      )
      
    })

    output$power_curve_plot <- renderPlot({
      req(power_curve_result())
      
      plot (x = power_curve_result()$delta, y = power_curve_result()$noptim, bty = 'n', ylim = c(0, 1500), axes = F,
            xlab = expression(delta), ylab = "Planned sample size")
      axis (1, seq (.1, .9, .2)); axis (2, seq (0, 1500, 500), las = 1)
    })
    
  })
}
    
## To be copied in the UI
# mod_power_curve_ui("power_curve_ui_1")
    
## To be copied in the server
# mod_power_curve_server("power_curve_ui_1")
 
