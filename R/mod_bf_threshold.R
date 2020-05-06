# Module UI
  
#' @title   mod_bf_threshold_ui and mod_bf_threshold_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_bf_threshold
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_bf_threshold_ui <- function(id){

  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_bf_threshold
#' @export
#' @keywords internal
    
mod_bf_threshold_server <- function(id, activate_menu, method_menu, activate_question, method_question){
  
  moduleServer(id, function(input, output, session) {
    
    modal <- function() {
      
      modalDialog(
        easyClose = TRUE,
        footer = actionButton(NS(id, "close_modal"), label = "Close"),
        h1("Decide BF threshold"),
        sliderInput(NS(id, "delta"), "Delta", min = 0, max = 1, value = 0.5, step = 0.1),
        selectInput(NS(id, "tresh"), "Threshold", choices = c(3, 6, 10),
                    selected = 10),
        actionButton(NS(id, "calculate"), "Ready, set, go!"),
        textOutput(NS(id, "bf_threshold_output"))
      )
    }
    
    observeEvent(activate_menu(), {
      
      if(method_menu() == "Bf treshold") {
        showModal(modal())}
      
    })
    
    observeEvent(activate_question(), {
      
      if(method_question() == "Bf treshold") {
        showModal(modal())}
      
    })
    
    observeEvent(input$close_modal, {
      
      removeModal()
      
    })
    
    bf_threshold_result <- eventReactive(input$calculate, {
      Band <- 0.2
      power <- 0.8
      Est <- TOSTss(Opt = power, Band = Band, delta = 0.5)
      Out <- PowerOptim(Fun = SuperBFpow, Range = c(50, Est), Arguments = c(Band, input$delta), Opt = power)
      Out
      })
    
    output$bf_threshold_output <- renderText({
      paste("\n n1 is ", bf_threshold_result()$n1, ", power is ", bf_threshold_result()$npower, sep = "")
    })
    
  })
}
    
## To be copied in the UI
# mod_bf_threshold_ui("bf_threshold_ui_1")
    
## To be copied in the server
# mod_bf_threshold_server("bf_threshold_ui_1")
 
