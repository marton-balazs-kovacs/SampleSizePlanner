# Module UI
  
#' @title   mod_flowchart_ui and mod_flowchart_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_flowchart
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_flowchart_ui <- function(id){
  
  tagList(
    
    )
}
    
# Module Server
    
#' @rdname mod_flowchart
#' @export
#' @keywords internal
    
mod_flowchart_server <- function(id){
  
  moduleServer(id, function(input, output, session) {
  
  })
}
    
## To be copied in the UI
# mod_flowchart_ui("flowchart_ui_1")
    
## To be copied in the server
# mod_flowchart_server("flowchart_ui_1")