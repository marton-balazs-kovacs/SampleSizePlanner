# Module UI
  
#' @title   mod_ssp_app_ui and mod_ssp_app_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_ssp_app
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_ssp_app_ui <- function(id){
  
  tagList(
    sidebarLayout(
    sidebarPanel(
      h1("APP (a-priori precision)"),
      sliderInput(NS(id, "closeness"), "Closeness", min = 0, max = 1, value = 0.2, step = 0.1),
      sliderInput(NS(id, "confidence"), "Confidence", min = 0, max = 1, value = 0.95, step = 0.1),
      actionButton(NS(id, "calculate"), "Calculate sample size")),
    mainPanel(
      mod_preview_ui(NS(id, "preview")),
      mod_download_ui(NS(id, "download"))
    )
    )
  )
}
    
# Module Server
    
#' @rdname mod_ssp_app
#' @export
#' @keywords internal
    
mod_ssp_app_server <- function(id){
  
  moduleServer(id, function(input, output, session) {
    # Setup loadingbar
    # waitress <- waiter::Waitress$new("#app-preview-show_preview", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)
    
    # Calculate results
    app_result <- eventReactive(input$calculate, {
      # waitress$start()
      ssp_app(confidence = input$confidence, closeness = input$closeness, report_text = TRUE)
    })
  
    # Render preview
    mod_preview_server("preview", activate = reactive(input$calculate), output_text = app_result, method = "app")
    
    # Download the output
    mod_download_server("download", activate = reactive(input$calculate), output_text = app_result, method = "app")
  })
}
    
## To be copied in the UI
# mod_ssp_app_ui("app")
    
## To be copied in the server
# mod_ssp_app_server("app")