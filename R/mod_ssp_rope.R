# Module UI
  
#' @title   mod_ssp_rope_ui and mod_ssp_rope_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_ssp_rope
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_ssp_rope_ui <- function(id){
  tagList(
    sidebarLayout(
    sidebarPanel(
      h1("ROPE"),
      sliderInput(NS(id, "opt"), "Power", min = 0, max = 1, value = 0.8, step = 0.1),
      sliderInput(NS(id, "band"), "Band", min = 0, max = 1, value = 0.2, step = 0.1),
      sliderInput(NS(id, "delta"), "Delta", min = 0, max = 2, value = 0, step = 0.1),
      actionButton(NS(id, "calculate"), "Calculate sample size")),
    mainPanel(
      mod_preview_ui(NS(id, "preview")),
      mod_download_ui(NS(id, "download"))
    )
    )
  )
}

# Module Server
    
#' @rdname mod_ssp_rope
#' @export
#' @keywords internal
    
mod_ssp_rope_server <- function(id){
  
  moduleServer(id, function(input, output, session) {
    # Setup loadingbar
    waitress <- waiter::Waitress$new("#rope-preview-show_preview", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)
    
    # Calculate results
    rope_result <- eventReactive(input$calculate, {
      waitress$start()
      ssp_rope(opt = input$opt, band = input$band, delta = input$delta)
    })
    
    # Send params to RMD
    params <- eventReactive(input$calculate, {
      list(
        n1 = rope_result()$n1,
        output_power = rope_result()$npower,
        input_power = input$opt,
        band = input$band,
        delta = input$delta
      )
    })
    
    # Render preview
    mod_preview_server("preview", activate = reactive(input$calculate), input_file = "rope_output.Rmd", params = params)
    
    # Download the output
    mod_download_server("download", activate = reactive(input$calculate), input_file = "rope_output.Rmd", params = params)

  })
}
    
## To be copied in the UI
# mod_ssp_rope_ui("rope")
    
## To be copied in the server
# mod_ssp_rope_server("rope")
 
