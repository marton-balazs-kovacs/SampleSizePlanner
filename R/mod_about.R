# Module UI
  
#' @title   mod_about_ui and mod_about_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_about
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_about_ui <- function(id){
  
  tagList(
    fixedPanel(
      shinyWidgets::actionBttn(inputId = NS(id, "open_about"),
                               label = "How to use",
                               style = "minimal",
                               color = "danger"),
      top = "8%", left = "4%", width = "auto"
    )
  )
}
    
# Module Server
    
#' @rdname mod_about
#' @export
#' @keywords internal
    
mod_about_server <- function(id){
  
  moduleServer(id, function(input, output, session) {
  modal <- function() {
    
    modalDialog(
      easyClose = TRUE,
      footer = modalButton("Close Modal"),
      includeMarkdown("inst/app/www/about.Rmd"))
    }
  
  observeEvent(input$open_about, {
    showModal(modal())})
  
  })
  }
    
## To be copied in the UI
# mod_about_ui("about_ui_1")
    
## To be copied in the server
# mod_about_server("about_ui_1")
 
