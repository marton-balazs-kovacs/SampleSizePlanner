# Module UI
  
#' @title   mod_ssp_eq_bf_ui and mod_ssp_eq_bf_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_ssp_eq_bf
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_ssp_eq_bf_ui <- function(id){
  
  tagList(
    sidebarLayout(
    sidebarPanel(
      h1("Interval Equiv BF"),
      sliderInput(NS(id, "delta"), "Delta", min = 0, max = 2, value = 0, step = 0.1),
      h2("Threshold"),
      selectInput(NS(id, "thresh"), "Threshold", choices = c(10, 6, 3),
                  selected = 10),
      actionButton(NS(id, "calculate"), "Calculate sample size")),
    mainPanel(
      mod_preview_ui(NS(id, "preview")),
      mod_download_ui(NS(id, "download"))
    )
    )
  )
}
    
# Module Server
    
#' @rdname mod_ssp_eq_bf
#' @export
#' @keywords internal
    
mod_ssp_eq_bf_server <- function(id){
  
  moduleServer(id, function(input, output, session) {
    # Setup loadingbar
    waitress <- waiter::Waitress$new("#eq_bf-preview-show_preview", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)
    
    # Calculate results
    eq_bf_result <- eventReactive(input$calculate, {
      waitress$start()
      ssp_eq_bf(opt = 0.8, 
                  band = 0.2,
                  delta = input$delta,
                  thresh = as.integer(input$thresh))
    })
    
    # Send params to RMD
    params <- eventReactive(input$calculate, {
      list(
        n1 = eq_bf_result()$n1,
        output_power = eq_bf_result()$npower,
        input_power = 0.8,
        band = 0.2,
        delta = input$delta,
        thresh = as.integer(input$thresh)
      )
    })

    # Render preview
    mod_preview_server("preview", activate = reactive(input$calculate), input_file = "eq_bf_output.Rmd", params = params)
    
    # Download the output
    mod_download_server("download", activate = reactive(input$calculate), input_file = "eq_bf_output.Rmd", params = params)
    
  })
}
    
## To be copied in the UI
# mod_ssp_eq_bf_ui("eq_bf")
    
## To be copied in the server
# mod_ssp_eq_bf_server("eq_bf")
 
