# Module UI
  
#' @title   mod_ssp_power_curve_ui and mod_ssp_power_curve_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_ssp_power_curve
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_ssp_power_curve_ui <- function(id){
  tagList(
    sidebarLayout(
    sidebarPanel(
      h1("Power curve"),
      sliderInput(NS(id, "opt"), "Power", min = 0, max = 1, value = 0.8, step = 0.1),
      sliderInput(NS(id, "delta"), "Delta", min = 0, max = 2, value = c(0.1, 0.9), step = 0.1),
      actionButton(NS(id, "calculate"), "Calculate sample size")),
    mainPanel(
      mod_preview_ui(NS(id, "preview")),
      mod_download_ui(NS(id, "download"))
    )
    )
  )
}
    
# Module Server
    
#' @rdname mod_ssp_power_curve
#' @export
#' @keywords internal
    
mod_ssp_power_curve_server <- function(id){

    moduleServer(id, function(input, output, session) {
      # Setup loadingbar
      waitress <- waiter::Waitress$new("#curve-preview-show_preview", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)

      # Calculate results
      curve_result <- eventReactive(input$calculate, {
        waitress$start()
        ssp_power_curve(delta1 = input$delta[1],
                    delta2 = input$delta[2],
                    opt = input$opt,
                    animated = TRUE)
        })
      
      # Send params to RMD
      params <- eventReactive(input$calculate, {
        list(
          plot = curve_result(),
          delta1 = input$delta[1],
          delta2 = input$delta[2],
          input_power = input$opt
        )
      })
      
      # Render preview
      mod_preview_server("preview", activate = reactive(input$calculate), input_file = "curve_output.Rmd", params = params)
      
      # Download the output
      mod_download_server("download", activate = reactive(input$calculate), input_file = "curve_output.Rmd", params = params, format = "html_document")
    
  })
}
    
## To be copied in the UI
# mod_ssp_power_curve_ui("curve")
    
## To be copied in the server
# mod_ssp_power_curve_server("curve")
 
