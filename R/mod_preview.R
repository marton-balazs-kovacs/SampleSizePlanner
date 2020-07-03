#' preview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_preview_ui <- function(id){
  tagList(
    textOutput(NS(id, "show_preview"))
  )
}
    
#' preview Server Function
#'
#' @noRd 
mod_preview_server <- function(id, activate, output_text, method){
  moduleServer(id, function(input, output, session) {
  # Render preview
  output$show_preview <- renderText({
    req(activate())
    output_text()
    })
  })
}
    
## To be copied in the UI
# mod_preview_ui("preview")
    
## To be copied in the server
# mod_preview_server("preview")
 
