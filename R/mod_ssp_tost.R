# Module UI
  
#' @title   mod_ssp_tost_ui and mod_ssp_tost_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_ssp_tost
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_ssp_tost_ui <- function(id){
  tagList(
    sidebarLayout(
    sidebarPanel(
      h1("TOST"),
      sliderInput(NS(id, "opt"), "Power", min = 0, max = 1, value = 0.8, step = 0.1),
      sliderInput(NS(id, "band"), "Band", min = 0, max = 1, value = 0.2, step = 0.1),
      sliderInput(NS(id, "delta"), "Delta", min = 0, max = 2, value = 0, step = 0.1),
      actionButton(NS(id, "calculate"), "Calculate sample size")
      ),
    mod_preview_ui(NS(id, "preview"))
    )
  )
}
    
# Module Server
    
#' @rdname mod_ssp_tost
#' @export
#' @keywords internal
    
mod_ssp_tost_server <- function(id){

  moduleServer(id, function(input, output, session) {
    # Setup loadingbar
    # waitress <- waiter::Waitress$new("#tost-preview-show_preview", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)
    
    # Calculate results
    tost_result <- eventReactive(input$calculate, {
      # waitress$start()
      ssp_tost(opt = input$opt, band = input$band, delta = input$delta, report_text = TRUE)
      })
    
    # Render preview
    mod_preview_server("preview", activate = reactive(input$calculate), output_text = tost_result, method = "tost")
    })
}
    
## To be copied in the UI
# mod_ssp_tost_ui("tost")
    
## To be copied in the server
# mod_ssp_tost_server("tost")
 
