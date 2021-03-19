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
    # Show output
    wellPanel(
      # style = "height:120px;",
      textOutput(NS(id, "show_preview"))
      ),
    # Download button
    div(
      class = "download-justification-btn",
      downloadButton(NS(id, "report"), label = "Download"),
      actionButton(NS(id, "clip"), label = "Copy")
      )
    )
}
    
#' preview Server Function
#'
#' @noRd 
mod_preview_server <- function(id, activate, output_parameters, method){
  moduleServer(id, function(input, output, session) {
    # Create justification text
    justification_text <- reactive({
      req(activate())
      justification(
        method = method,
        output_parameters = isolate(output_parameters())
        )
      })
    
    # Show justification
    output$show_preview <- renderText(justification_text())
    
    # Add downloadbutton enable logic
    observe({
      if (activate()) {
        shinyjs::enable("report")
        shinyjs::enable("clip")
        shinyjs::runjs("$('.download-justification-btn').removeAttr('title');")
        } else{
          shinyjs::disable("report")
          shinyjs::disable("clip")
          shinyjs::runjs("$('.download-justification-btn').attr('title', 'Please run the calculation first');")
          }
      })
  
    # Download the output
    output$report <- downloadHandler(
      filename = function() {
        paste0(method, "_", Sys.Date(), ".txt")
        },
      content = function(file) {
        writeLines(justification_text(), file)
        })
    
    # Copy output
    observeEvent(input$clip, {
      clipr::write_clip(justification_text())
      })
    })
  }
    
## To be copied in the UI
# mod_preview_ui("preview")
    
## To be copied in the server
# mod_preview_server("preview")
 
