# Module UI
  
#' @title   mod_article_module_ui and mod_article_module_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_article_module
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_article_module_ui <- function(id){
  
  tagList(
    uiOutput(NS(id, "open_tab")),
    fixedPanel(
      shinyWidgets::actionBttn(
        inputId = NS(id, "article"),
        label = "Open article", 
        style = "minimal",
        color = "danger"),
      bottom = "8%", right = "4%", width = "auto"
      )
  )
}
    
# Module Server
    
#' @rdname mod_article_module
#' @export
#' @keywords internal
    
mod_article_module_server <- function(id){
  
  moduleServer(id, function(input, output, session) {
  
  output$open_tab <- renderUI({
    req(input$article > 0)
    
    tags$script(paste0("window.open('", "https://psyarxiv.com/", "', '_blank')"))
  })
  })
}
    
## To be copied in the UI
# mod_article_module_ui("article_module_ui_1")
    
## To be copied in the server
# mod_article_module_server("article_module_ui_1")
 
