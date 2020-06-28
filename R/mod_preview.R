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
    htmlOutput(NS(id, "show_preview"))
  )
}
    
#' preview Server Function
#'
#' @noRd 
mod_preview_server <- function(id, activate, input_file, params){
  moduleServer(id, function(input, output, session) {
  ns <- session$ns
  
  # Render preview
  output$show_preview <- renderUI({
    if (activate()) {
      addResourcePath("tmp", tempdir())
      report_path <- file.path("inst/app/www/", input_file)
      file.copy(input_file, report_path, overwrite = TRUE)
      temp_file <- fs::file_temp(ext = ".html")
      callr::r(
        render_report,
        list(input = report_path, output = temp_file, format = "html_document", params = params())
        )
      return(tags$iframe(style = "height:400px; width:100%", src = file.path("tmp", basename(temp_file)), seamless = "seamless"))
      } else {
        return(tags$iframe(style = "height:400px; width:100%", src = "www/on_load_placeholder.html", seamless = "seamless"))
        }
    })
  })
}
    
## To be copied in the UI
# mod_preview_ui("preview")
    
## To be copied in the server
# mod_preview_server("preview")
 
