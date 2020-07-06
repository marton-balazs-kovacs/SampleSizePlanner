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
    mainPanel(
      h3("Instructions"),
      tags$ul(tags$li("Please set the parameters and run the sample size planning calculation with the \"Calculate sample size\" button."),
              tags$li("After running the calculation you can download the results in a general template text in word format."),
              tags$li("After download, please complement the text where it is need with your reasoning behind each decision and include the text in your manuscript."),
              tags$li("Please wait for a calulation to finish before you attempt to download the output or open a new tab.")),
    div(style = "height:120px;", textOutput(NS(id, "show_preview"))),
    div(class = "download-btn",
        downloadButton(NS(id, "report"))))
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
# mod_preview_ui("preview")
    
## To be copied in the server
# mod_preview_server("preview")
 
