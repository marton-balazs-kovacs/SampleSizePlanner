#' download UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_download_ui <- function(id){
  tagList(
    div(class = "download-btn",
        downloadButton(NS(id, "report")))
  )
}
    
#' download Server Function
#'
#' @noRd 
mod_download_server <- function(id, activate, output_text, method){
  moduleServer(id, function(input, output, session) {
  # Add downloadbutton enable logic
  observe({
    if (activate()) {
      shinyjs::enable("report")
      shinyjs::runjs("$('.download-btn').removeAttr('title');")
    } else{
      shinyjs::disable("report")
      shinyjs::runjs("$('.download-btn').attr('title', 'Please run the calculation first');")
    }
  })
  
  # Download the output
  output$report <- downloadHandler(
    filename = function() {
      paste0(method, "_", Sys.Date(), ".txt")
    },
    content = function(file) {
      writeLines(output_text(), file)
    })
  })
}
    
## To be copied in the UI
# mod_download_ui("download")
    
## To be copied in the server
# mod_download_server("download")
 
