# Module UI
  
#' @title   mod_intro_module_ui and mod_intro_module_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_intro_module
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_intro_module_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    fixedPanel(
      textOutput(ns("intro")),
      bottom = "65%", left = "10%", right = "10%", width = "auto"
    ),
    fixedPanel(
      shinyWidgets::actionBttn(
        inputId = ns("next_button"),
        label = "Let's plan!",
        style = "minimal",
        color = "danger"),
      bottom = "30%", right = "50%", width = "auto"
    )
  )
}
    
# Module Server
    
#' @rdname mod_intro_module
#' @export
#' @keywords internal
    
mod_intro_module_server <- function(input, output, session){
  ns <- session$ns
  
  output$intro <- renderText({
    "Make me disappear please!"
  })
  
  return(reactive(input$next_button))
}
    
## To be copied in the UI
# mod_intro_module_ui("intro_module_ui_1")
    
## To be copied in the server
# callModule(mod_intro_module_server, "intro_module_ui_1")
 
