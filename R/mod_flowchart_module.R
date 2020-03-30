# Module UI
  
#' @title   mod_flowchart_module_ui and mod_flowchart_module_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_flowchart_module
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_flowchart_module_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    shinyglide::screen(
      DiagrammeR::grVizOutput(ns("dg"))
    ),
    fixedPanel(
      shinyWidgets::actionBttn(
        inputId = ns("flowchart"),
        label = "Open flowchart", 
        style = "minimal",
        color = "danger"),
      bottom = "5%", left = "6%", width = "auto"
    )
  )
}
    
# Module Server
    
#' @rdname mod_flowchart_module
#' @export
#' @keywords internal
    
mod_flowchart_module_server <- function(input, output, session){
  ns <- session$ns
  
  graph <- open_graph("inst/app/www/flowchart.dgr")
  
  output$dg <- renderGrViz({
    render_graph(graph, layout = "neato")
  })
}
    
## To be copied in the UI
# mod_flowchart_module_ui("flowchart_module_ui_1")
    
## To be copied in the server
# callModule(mod_flowchart_module_server, "flowchart_module_ui_1")