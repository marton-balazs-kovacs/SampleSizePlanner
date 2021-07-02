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
mod_ssp_app_ui <- function(id) {
  tagList(
    # Method
    h1("A-priori precision (APP)", class = "subtitle"),
    sidebarLayout(
      sidebarPanel(
        # Panel title
        h3("Determine your sample size", class = "subtitle"),
        # Method description
        p("The approach aims to plan a sample size based on how close the researcher wishes both sample means to be to their respective population parameter, and how confident the researcher wants to be in this."),
        # Calculation settings
        ## Closeness input
        sliderInput(
          NS(id, "closeness"),
          name_with_info(
            "Closeness",
            "The desired closeness of the sample mean to the population mean defined in standard deviation."),
          min = 0,
          max = 1,
          value = 0.2,
          step = 0.1),
        ## Confidence input
        sliderInput(
          NS(id, "confidence"),
          name_with_info(
            "Confidence",
            "The desired probability of obtaining the sample mean with the desired closeness to the population mean."),
          min = 0, 
          max = 1,
          value = 0.95, 
          step = 0.1),
        # Run calculation
        actionButton(NS(id, "calculate"), "Calculate sample size", class = "calculate-btn"),
        # Show the results of the calculation
        wellPanel(
          class = "panel-calculate",
          htmlOutput(NS(id, "calculate_output")))),
      # Output
      mainPanel(
        wellPanel(
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Justification",
              # Panel title
              h3("Justify your sample size"),
              # Create justification text
              actionButton(NS(id, "justification"), "Create justification report", class = "calculate-btn"),
              # Show justification text
              mod_preview_ui(NS(id, "preview"))),
            tabPanel(
              "Code",
              # Panel title
              h3("Function call to use in R"),
              mod_code_ui(NS(id, "code")))
          )
        )
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_ssp_app
#' @export
#' @keywords internal
mod_ssp_app_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Setup loadingbar
    # waitress <- waiter::Waitress$new("#app-preview-show_preview", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)
    
    # Calculate results
    app_result <- eventReactive(input$calculate, {
      # waitress$start()
      ssp_app(confidence = input$confidence, closeness = input$closeness)
    })
    
    # Show calculated results
    output$calculate_output <- renderUI({
      HTML(
        glue::glue(
          "<b>n1:</b> {n1}",
          n1 = app_result()$n1
        )
      )
    })
    
    # Add justification enable logic
    observe({
      if (input$calculate) {
        shinyjs::enable("justification")
      } else{
        shinyjs::disable("justification")
      }
    })
    
    # Set output parameters
    output_parameters <- reactive({
      list(
        confidence = input$confidence,
        closeness = input$closeness,
        n1 = app_result()$n1
      )
    })
    
    # Render justification
    mod_preview_server(
      "preview",
      activate = reactive(input$justification),
      output_parameters = output_parameters,
      method = "app")
    
    # Set code parameters
    code_parameters <- reactive({
      list(
        closeness = input$closeness,
        confidence = input$confidence
      )
    })
    
    # Render code preview
    mod_code_server(
      "code",
      code_parameters = code_parameters,
      method = "app")
  })
}
    
## To be copied in the UI
# mod_ssp_app_ui("app")
    
## To be copied in the server
# mod_ssp_app_server("app")