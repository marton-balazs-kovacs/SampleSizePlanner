# Module UI
  
#' @title   mod_apriori_precision_ui and mod_apriori_precision_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_apriori_precision
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_apriori_precision_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_apriori_precision
#' @export
#' @keywords internal
    
mod_apriori_precision_server <- function(id, activate_menu, method_menu, activate_question, method_question){
  
  moduleServer(id, function(input, output, session) {
    
    modal <- function() {
      
      modalDialog(
        footer = actionButton(NS(id, "close_modal"), label = "Close"),
        h1("APP (a-priori precision)"),
        sliderInput(NS(id, "closeness"), "Closeness", min = 0, max = 1, value = 0.2, step = 0.1),
        sliderInput(NS(id, "confidence"), "Confidence", min = 0, max = 1, value = 0.95, step = 0.1),
        actionButton(NS(id, "calculate"), "Ready, set, go!"),
        textOutput(NS(id, "app_output"))
      )
    }
    
    observeEvent(activate_menu(), {
      
      if(method_menu() == "APP") {
        showModal(modal())}
      
    })
    
    observeEvent(activate_question(), {
      
      if(method_question() == "APP") {
        showModal(modal())}
      
    })
    
    observeEvent(input$close_modal, {
      
      removeModal()
      
    })
    
    app_result <- eventReactive(input$calculate, {
      APP(confidence = input$confidence, closeness = input$closeness)
    })
    
    output$app_output <- renderText({
      app_result()
    })
    
  })
}
    
## To be copied in the UI
# mod_apriori_precision_ui("apriori_precision_ui_1")
    
## To be copied in the server
# mod_apriori_precision_server("apriori_precision_ui_1")