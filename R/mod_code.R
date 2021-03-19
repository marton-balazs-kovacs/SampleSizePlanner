#' code UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_code_ui <- function(id){
  tagList(
    # Show code
    div(
      verbatimTextOutput(NS(id, "show_code"), placeholder = TRUE)),
    # Download button
    div(
      downloadButton(NS(id, "report"), label = "Download"),
      actionButton(NS(id, "clip"), label = "Copy")
    )
  )
}
    
#' code Server Function
#'
#' @noRd 
mod_code_server <- function(id, code_parameters, method){
  moduleServer(id, function(input, output, session) {
    # Create code text
    code_text <- reactive({
      show_code(
        method = method,
        code_parameters = code_parameters()
      )
    })
    
    # Show code
    output$show_code <- renderText(code_text())
    
    # Download the code
    output$report <- downloadHandler(
      filename = function() {
        paste0(method, "_", Sys.Date(), ".R")
      },
      content = function(file) {
        writeLines(code_text(), file)
      })
    
    # Copy the code
    observeEvent(input$clip, {
      clipr::write_clip(code_text())
    })
  })
}
    
## To be copied in the UI
# mod_code_ui("code")
    
## To be copied in the server
# mod_code_server("code")
 
