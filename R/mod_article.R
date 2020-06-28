# Module UI
  
#' @title   mod_article_ui and mod_article_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_article
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_article_ui <- function(id){
  
  tagList(
    uiOutput(NS(id, "pdf_view"))
  )
}
    
# Module Server
    
#' @rdname mod_article
#' @export
#' @keywords internal
    
mod_article_server <- function(id){
  
  moduleServer(id, function(input, output, session) {
    # Setup loadingbar
    waitress <- waiter::Waitress$new("#article-pdf_view", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)
    
    output$pdf_view <- renderUI({
      tags$iframe(style = "height:600px; width:100%", src = "https://mfr.de-1.osf.io/render?url=https://osf.io/sq89t/?direct%26mode=render%26action=download%26mode=render")
    })
  })
}

## To be copied in the UI
# mod_article_ui("article")
    
## To be copied in the server
# mod_article_server("article")
 
