# Module UI
  
#' @title   mod_traditional_power_ui and mod_traditional_power_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_traditional_power
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_traditional_power_ui <- function(id){

  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_traditional_power
#' @export
#' @keywords internal
    
mod_traditional_power_server <- function(id, activate_menu, method_menu, activate_question, method_question){
  
  moduleServer(id, function(input, output, session) {
    
    modal <- function() {
      
      modalDialog(
        footer = actionButton(NS(id, "close_modal"), label = "Close modal"),
        h1("Traditional Power"),
        sliderInput(NS(id, "opt"), "Power", min = 0, max = 1, value = 0.8, step = 0.1),
        sliderInput(NS(id, "delta"), "Delta", min = 0, max = 1, value = 0, step = 0.1),
        numericInput(NS(id, "n_min"), "Minimum sample size", 20),
        numericInput(NS(id, "n_max"), "Maximum sample size", 200),
        actionButton(NS(id, "calculate"), "Ready, set, go!"),
        textOutput(NS(id, "tpow_output"))
      )
    }
    
    observeEvent(activate_menu(), {
      
      if(method_menu() == "Traditional power") {
        showModal(modal())}
      
    })
    
    observeEvent(activate_question(), {
      
      if(method_question() == "Traditional power") {
        showModal(modal())}
      
    })
    
    observeEvent(input$close_modal, {
      
      removeModal()
      
    })
    
    tpow_result <- eventReactive(input$calculate, {
      Band <- NA
      result <- PowerOptim(Fun = Tpow, Range = c(input$n_min, input$n_max), Arguments = c(Band, input$delta), Opt = input$opt)
      if(!is.null(result)) {
        return(paste("\n \n Optimal n1 is ", result[1], 
                   ", resulting power is ", result[2], sep = ""))
      } else {
        "Your chosen power level cannot be achieved within this range of sample sizes!"
        }
      })
    
    output$tpow_output <- renderText({
      tpow_result()
    })
    
  })
}
    
## To be copied in the UI
# mod_traditional_power_ui("traditional_power_ui_1")
    
## To be copied in the server
# mod_traditional_power_server("traditional_power_ui_1")
 
