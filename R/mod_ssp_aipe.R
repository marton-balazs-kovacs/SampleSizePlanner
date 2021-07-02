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
mod_ssp_aipe_ui <- function(id) {
  tagList(
    # Method
    h1("Accuracy In Parameter Estimation (AIPE)", class = "method-title"),
    sidebarLayout(
      sidebarPanel(
        # Panel title
        h3("Determine your sample size", class = "subtitle"),
        # Method description
        p("A sample size estimation method used for parameter estimation. The approach aims to find the required sample size,
          such that the confidence interval has a certain expected width. "),
        # Calculation settings
        ## Confidence level input
        sliderInput(
          NS(id, "confidence_level"),
          name_with_info(
            "Confidence Level",
            "The desired level of confidence."),
          min = 0,
          max = 1,
          value = 0.8,
          step = 0.1),
        ## Delta input
        sliderInput(
          NS(id, "delta"),
          name_with_info(
            "Delta",
            "The expected population effect size."),
          min = 0,
          max = 2,
          value = 0.5,
          step = 0.1),
        ## Width input
        sliderInput(
          NS(id, "width"),
          name_with_info(
            "Width",
            "The desired width of the confidence interval, given Delta."),
          min = 0,
          max = 1,
          value = 0.2,
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
              # Justification for confidence level
              selectizeInput(
                NS(id, "confidence_level_justification"),
                label = "Confidence level",
                choices = c(
                  "it is the common standard in the field",
                  "it is the journal publishing requirement",
                  "other..."),
                multiple = FALSE,
                options = list(create = TRUE)),
              # Justification for width
              selectizeInput(
                NS(id, "width_justification"),
                label = "Width",
                choices = c(
                  "previous studies on this topic reported a similar region of practical equivalence",
                  "of the following substantive reasons: ...",
                  "other..."),
                multiple = FALSE,
                options = list(create = TRUE)),
              # Justification for delta
              selectizeInput(
                NS(id, "delta_justification"),
                label = "Delta",
                choices = c(
                  "previous results published in ...",
                  "of the following substantive reasons: ...",
                  "other..."),
                multiple = FALSE,
                options = list(create = TRUE)),
              # Create justification text
              actionButton(NS(id, "justification"), "Create justification report", class = "calculate-btn"),
              # Show justification text
              mod_preview_ui(NS(id, "preview"))
              ),
            tabPanel(
              "Code",
              # Panel title
              h3("Function call to use in R"),
              mod_code_ui(NS(id, "code"))
              )
            )
          )
        )
      )
    )
  }
    
# Module Server
    
#' @rdname mod_ssp_aipe
#' @export
#' @keywords internal
    
mod_ssp_aipe_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Setup loadingbar
    # waitress <- waiter::Waitress$new("#aipe-preview-show_preview", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)

    # Calculate results
    aipe_result <- eventReactive(input$calculate, {
      # waitress$start()
      ssp_aipe(delta = input$delta, confidence_level = input$confidence_level, width = input$width)
    })
    
    # Show calculated results
    output$calculate_output <- renderUI({
      HTML(
        glue::glue(
          "<b>n1:</b> {n1}",
          n1 = aipe_result()$n1
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
        confidence_level = input$confidence_level,
        width = input$width,
        delta = input$delta,
        delta_justification = input$delta_justification,
        width_justification = input$width_justification,
        confidence_level_justification = input$confidence_level_justification,
        n1 = aipe_result()$n1
      )
    })
    
    # Render justification
    mod_preview_server(
      "preview",
      activate = reactive(input$justification),
      output_parameters = output_parameters,
      method = "aipe")
    
    # Set code parameters
    code_parameters <- reactive({
      list(
        confidence_level = input$confidence_level,
        width = input$width,
        delta = input$delta
      )
    })
    
    # Render code preview
    mod_code_server(
      "code",
      code_parameters = code_parameters,
      method = "aipe"
    )
  })
}
    
## To be copied in the UI
# mod_ssp_aipe_ui("aipe")
    
## To be copied in the server
# mod_ssp_aipe_server("aipe")
 
