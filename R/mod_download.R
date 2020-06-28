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
mod_download_server <- function(id, activate, input_file, params, format = "word_document"){
  moduleServer(id, function(input, output, session) {
  # Create output file extension based on the format
  ext <- switch(format,
                word_document = ".doc",
                html_document = ".html")
  
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
      paste0(tools::file_path_sans_ext(input_file), "_", Sys.Date(), ext)
    },
    content = function(file) {
      report_path <- file.path("inst/app/www/", input_file)
      file.copy(input_file, report_path, overwrite = TRUE)
      callr::r(
        render_report,
        list(input = report_path, output = file, format = format, params = params())
      )
    })
  })
}
    
## To be copied in the UI
# mod_download_ui("download")
    
## To be copied in the server
# mod_download_server("download")
 
