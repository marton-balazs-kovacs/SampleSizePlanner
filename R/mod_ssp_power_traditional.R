# Module UI
  
#' @title   mod_ssp_power_traditional_ui and mod_ssp_power_traditional_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_ssp_power_traditional
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_ssp_power_traditional_ui <- function(id){
  tagList(
    sidebarLayout(
    sidebarPanel(
      h1("Traditional Power"),
      sliderInput(NS(id, "opt"), "Power", min = 0, max = 1, value = 0.8, step = 0.1),
      sliderInput(NS(id, "delta"), "Delta", min = 0, max = 1, value = 0.5, step = 0.1),
      numericInput(NS(id, "n_min"), "Minimum sample size", 20),
      numericInput(NS(id, "n_max"), "Maximum sample size", 200),
      actionButton(NS(id, "calculate"), "Calculate sample size")),
    mainPanel(
      mod_preview_ui(NS(id, "preview")),
      mod_download_ui(NS(id, "download"))
      )
    )
  )
  }
    
# Module Server
    
#' @rdname mod_ssp_power_traditional
#' @export
#' @keywords internal
    
mod_ssp_power_traditional_server <- function(id, activate_menu, method_menu, activate_question, method_question){
  
  moduleServer(id, function(input, output, session) {
    waitress <- waiter::Waitress$new("#traditional-preview-show_preview", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)
    
    # Calculate results
    traditional_result <- eventReactive(input$calculate, {
      waitress$start()
      ssp_power_traditional(n_min = input$n_min,
                        n_max = input$n_max,
                        delta = input$delta,
                        opt = input$opt)
    })
    
    # Send params to RMD
    params <- eventReactive(input$calculate, {
      list(
        n1 = traditional_result()$n1,
        output_power = traditional_result()$npower,
        input_power = input$opt,
        delta = input$delta,
        n_min = input$n_min,
        n_max = input$n_max
      )
    })

    # Render preview
    mod_preview_server("preview", activate = reactive(input$calculate), input_file = "traditional_output.Rmd", params = params)
    
    # Download the output
    mod_download_server("download", activate = reactive(input$calculate), input_file = "traditional_output.Rmd", params = params)
  })
}
    
## To be copied in the UI
# mod_ssp_power_traditional_ui("traditional")
    
## To be copied in the server
# mod_ssp_power_traditional_server("traditional")
 
