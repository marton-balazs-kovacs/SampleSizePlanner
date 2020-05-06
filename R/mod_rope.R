# Module UI
  
#' @title   mod_rope_ui and mod_rope_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_rope
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_rope_ui <- function(id){

  tagList(
  
  )
}

# Module Server
    
#' @rdname mod_rope
#' @export
#' @keywords internal
    
mod_rope_server <- function(id, activate_menu, method_menu, activate_question, method_question){
  
  moduleServer(id, function(input, output, session) {
    
    modal <- function() {
      
      modalDialog(
        footer = actionButton(NS(id, "close_modal"), label = "Close"),
        h1("ROPE"),
        h2("Effect size"),
        sliderInput(NS(id, "opt"), "Power", min = 0, max = 1, value = 0.8, step = 0.1),
        sliderInput(NS(id, "band"), "Band", min = 0, max = 1, value = 0.2, step = 0.1),
        sliderInput(NS(id, "delta"), "Delta", min = 0, max = 2, value = 0, step = 0.1),
        actionButton(NS(id, "calculate"), "Ready, set, go!"),
        textOutput(NS(id, "rope_output"))
      )
    }
    
    observeEvent(activate_menu(), {
      
      if(method_menu() == "ROPE") {
        showModal(modal())}
      
    })
    
    observeEvent(activate_question(), {
      
      if(method_question() == "ROPE") {
        showModal(modal())}
      
    })
    
    observeEvent(input$close_modal, {
      
      removeModal()
      
    })
    
    rope_result <- eventReactive(input$calculate, {
      "This function still needs work to run. It is here for UI check."
    })
    
    output$rope_output <- renderText({
      rope_result()
    })
    
  })
}
    
## To be copied in the UI
# mod_rope_ui("rope_ui_1")
    
## To be copied in the server
# mod_rope_server("rope_ui_1")
 
