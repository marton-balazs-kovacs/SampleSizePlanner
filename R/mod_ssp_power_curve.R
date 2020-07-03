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
      plotly:: plotlyOutput(NS(id, "preview")),
      div(class = "download-btn",
          downloadButton(NS(id, "report")))
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
      # waitress <- waiter::Waitress$new("#curve-preview-show_preview", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)
      
      # Add downloadbutton enable logic
      observe({
        if (input$calculate) {
          shinyjs::enable("report")
          shinyjs::runjs("$('.download-btn').removeAttr('title');")
        } else{
          shinyjs::disable("report")
          shinyjs::runjs("$('.download-btn').attr('title', 'Please run the calculation first');")
        }
      })
      
      # Calculate results
      curve_result <- eventReactive(input$calculate, {
        # waitress$start()
        ssp_power_curve(delta1 = input$delta[1], delta2 = input$delta[2], opt = input$opt, animated = FALSE)
        })
      
      # Show preview
      output$preview <- plotly::renderPlotly({
        curve_result()  %>% 
          plotly::ggplotly(tooltip = "text") %>%
          plotly::config(displayModeBar = F)
      })
      
      # Download the output
      output$report <- downloadHandler(
        filename = function() {
          paste0("power_curve", "_", Sys.Date(), ".png")
        },
        content = function(file) {
          ggplot2::ggsave(file, plot = curve_result(), device = "png")
        },
        contentType = "image/png")
    
  })
}
    
## To be copied in the UI
# mod_ssp_power_curve_ui("curve")
    
## To be copied in the server
# mod_ssp_power_curve_server("curve")
 
