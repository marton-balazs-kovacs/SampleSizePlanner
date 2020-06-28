# Module UI
  
#' @title   mod_ssp_bf_thresh_ui and mod_ssp_bf_thresh_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_ssp_bf_thresh
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_ssp_bf_thresh_ui <- function(id){
  tagList(
    sidebarLayout(
    sidebarPanel(
      h1("Decide BF threshold"),
      sliderInput(NS(id, "delta"), "Delta", min = 0, max = 2, value = 0.5, step = 0.1),
      selectInput(NS(id, "thresh"), "Threshold", choices = c(10, 6, 3), selected = 10),
      actionButton(NS(id, "calculate"), "Calculate sample size")),
    mainPanel(
      mod_preview_ui(NS(id, "preview")),
      mod_download_ui(NS(id, "download"))
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_ssp_bf_thresh
#' @export
#' @keywords internal
    
mod_ssp_bf_thresh_server <- function(id){
  
  moduleServer(id, function(input, output, session) {
    # Setup loadingbar
    waitress <- waiter::Waitress$new("#bf_thresh-preview-show_preview", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)
    
    # Calculate results
    bf_thresh_result <- eventReactive(input$calculate, {
      Waitress$start()
      ssp_bf_thresh(opt = 0.8,
                      band = 0.2,
                      delta = input$delta,
                      thresh = as.integer(input$thresh))
      })
    
    # Send params to RMD
    params <- eventReactive(input$calculate, {
      list(
        n1 = bf_thresh_result()$n1,
        output_power = bf_thresh_result()$npower,
        input_power = 0.8,
        band = 0.2,
        delta = input$delta,
        thresh = input$thresh
      )
    })
    
    # Render preview
    mod_preview_server("preview", activate = reactive(input$calculate), input_file = "bf_thresh_output.Rmd", params = params)
    
    # Download the output
    mod_download_server("download", activate = reactive(input$calculate), input_file = "bf_thresh_output.Rmd", params = params)
  })
}
    
## To be copied in the UI
# mod_ssp_bf_thresh_ui("bf_thresh")
    
## To be copied in the server
# mod_ssp_bf_thresh_server("bf_thresh")
 
