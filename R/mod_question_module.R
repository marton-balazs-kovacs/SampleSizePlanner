# Module UI
  
#' @title   mod_question_module_ui and mod_question_module_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_question_module
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_question_module_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}
    
# Module Server
    
#' @rdname mod_question_module
#' @export
#' @keywords internal
    
mod_question_module_server <- function(input, output, session, data){
  ns <- session$ns
  
  output$question <- renderText({
    
  })
  
  # add reactive ui print question
  # add reactive ui print button with option left under the question (use actionBttn)
  # add reactive ui print button with option right under the question (use actionBttn)

  }
    
## To be copied in the UI
# mod_question_module_ui("question_module_ui_1")
    
## To be copied in the server
# callModule(mod_question_module_server, "question_module_ui_1")