# Module UI
  
#' @title   mod_ssp_bfda_ui and mod_ssp_bfda_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_ssp_bfda
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_ssp_bfda_ui <- function(id){

  tagList(
    sidebarLayout(
    sidebarPanel(
      h1("BFDA"),
      tags$p("To compute this sample size estimation method takes too long, therefore we do not provide an online tool for it within the app. However, you can set your parameters that will update the code accordingly and copy paste the code into an R session with the \"clip\" button. After this, you can run the computation on your own local computer."),
      sliderInput(NS(id, "delta"), "Delta", min = 0, max = 2, value = 0, step = 0.1),
      selectInput(NS(id, "tresh"), "Threshold", choices = c(3, 6, 10), selected = 10),
      actionButton(NS(id, "clip"), "Clip")),
    mainPanel(
      verbatimTextOutput(NS(id, "bfda_output"))
    )
    )
  )
}
    
# Module Server
    
#' @rdname mod_ssp_bfda
#' @export
#' @keywords internal
    
mod_ssp_bfda_server <- function(id){
  
  moduleServer(id, function(input, output, session) {
    # Setup loadingbar
    # waitress <- waiter::Waitress$new("#bfda-preview-show_preview", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)
    
    #  Create code to copy
    bfda_result <- reactive({
      # waitress$start()
      paste("# Download package",
          "install.packages(\"SampleSizePlanner\")",
          "# Load package",
          "library(SampleSizePlanner)",
          "# Run calculation",
          stringr::str_c("power_bfda(delta = ", input$delta, ", thresh = ", input$tresh, ", power = 0.8, nRep = 1000)"),
          sep = "\n")
    })
    
    # Show preview
    output$bfda_output <- renderText({
      bfda_result()
    })
    
    # Clip code
    observeEvent(input$clip, {
      clipr::write_clip(bfda_result())
    })
    
  })
}
    
## To be copied in the UI
# mod_ssp_bfda_ui("bfda")
    
## To be copied in the server
# mod_ssp_bfda_server("bfda")
 
