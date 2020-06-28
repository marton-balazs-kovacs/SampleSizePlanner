# Module UI
  
#' @title   mod_ssp_aipe_ui and mod_ssp_aipe_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_ssp_aipe
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_ssp_aipe_ui <- function(id){
  
  tagList(
    sidebarLayout(
    # Settings
    sidebarPanel(
      h1("Accuracy In Parameter Estimation"),
      sliderInput(NS(id, "opt"), "Confidence", min = 0, max = 1, value = 0.8, step = 0.1),
      sliderInput(NS(id, "delta"), "Delta", min = 0, max = 2, value = 0.5, step = 0.1),
      numericInput(NS(id, "n_min"), "Minimum sample size", value = 20, min = 5),
      numericInput(NS(id, "n_max"), "Maximum sample size", value = 200, max = 2000),
      actionButton(NS(id, "calculate"), "Calculate sample size")),
    # Output
    mainPanel(
      mod_preview_ui(NS(id, "preview")),
      mod_download_ui(NS(id, "download"))
      )
    )
  )
  }
    
# Module Server
    
#' @rdname mod_ssp_aipe
#' @export
#' @keywords internal
    
mod_ssp_aipe_server <- function(id){
  
  moduleServer(id, function(input, output, session) {
    # Setup loadingbar
    waitress <- waiter::Waitress$new("#aipe-preview-show_preview", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)

    # Calculate results
    aipe_result <- eventReactive(input$calculate, {
      waitress$start()
      ssp_aipe(n_min = input$n_min,
                 n_max = input$n_max,
                 delta = input$delta,
                 opt = input$opt)
    })
    
    # Send params to RMD
    params <- eventReactive(input$calculate, {
      list(
        n_min = input$n_min,
        n_max = input$n_max,
        output_n = aipe_result()$n1,
        output_confidence = aipe_result()$npower,
        input_power = input$opt,
        delta = input$delta
      )
    })
    
    # Render preview
    mod_preview_server("preview", activate = reactive(input$calculate), input_file = "aipe_output.Rmd", params = params)
    
    # Download the output
    mod_download_server("download", activate = reactive(input$calculate), input_file = "aipe_output.Rmd", params = params)
  })
}
    
## To be copied in the UI
# mod_ssp_aipe_ui("aipe")
    
## To be copied in the server
# mod_ssp_aipe_server("aipe")
 
