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
mod_ssp_power_traditional_ui <- function(id) {
  tagList(
    # Method
    h1("Classical power analysis"),
    sidebarLayout(
      sidebarPanel(
        # Panel title
        h3("Determine your sample size", class = "subtitle"),
        # Method description
        p("This method is used to estimate the minimum sample size that a design needs to reach a statistical power, given a  desired significance level and expected effect size."),
        # Calculation settings
        ## TPR input
        sliderInput(
          NS(id, "tpr"),
          list(
            "True Positive Rate (TPR)",
            HTML('<i class="fas fa-info"; title="The desired long-run probability of obtaining a significant result with a one-sided t-test, given Delta."></i>')),
          min = 0,
          max = 1,
          value = 0.8,
          step = 0.1),
        ## Input Delta
        sliderInput(
          NS(id, "delta"),
          list(
            "Delta",
            HTML('<i class="fas fa-info"; title="The expected population effect size."></i>')),
          min = 0,
          max = 2,
          value = 0.5,
          step = 0.1),
        ## Input Maximum n
        numericInput(
          NS(id, "max_n"),
          list(
            "Maximum N",
            HTML('<i class="fas fa-info"; title="The maximum number of participants per group (both groups are assumed to have equal sample size)."></i>')),
          min = 10,
          max = 20000,
          value = 5000,
          step = 1),
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
              # Justification for TPR
              selectizeInput(
                NS(id, "tpr_justification"),
                label = "True Positive Rate (TPR)",
                choices = c(
                  "it is the common standard in the field",
                  "it is the journal publishing requirement",
                  "other..."),
                multiple = FALSE,
                options = list(create = TRUE)),
              selectizeInput(
                NS(id, "delta_justification"),
                label = "Delta",
                choices = c(
                  "previous results published in ...",
                  "our reasoning that ...",
                  "other..."),
                multiple = FALSE,
                options = list(create = TRUE)),
              # Create justification text
              actionButton(NS(id, "justification"), "Create justification report", class = "calculate-btn"),
              # Show justification text
              mod_preview_ui(NS(id, "preview"))),
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
    
#' @rdname mod_ssp_power_traditional
#' @export
#' @keywords internal
    
mod_ssp_power_traditional_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    # waitress <- waiter::Waitress$new("#traditional-preview-show_preview", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)
    
    # Calculate results
    traditional_result <- eventReactive(input$calculate, {
      # waitress$start()
      ssp_power_traditional(max_n = input$max_n, delta = input$delta, tpr = input$tpr)
    })
    
    # Show calculated results
    output$calculate_output <- renderUI({
      HTML(
        glue::glue(
          "<b>n1:</b> {n1}<br/><b>npower:</b> {npower}",
          n1 = traditional_result()$n1,
          npower = traditional_result()$npower
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
        tpr = input$tpr,
        delta = input$delta,
        delta_justification = input$delta_justification,
        tpr_justification = input$tpr_justification,
        n1 = traditional_result()$n1,
        npower = traditional_result()$npower
      )
    })
    
    # Render preview
    mod_preview_server(
      "preview",
      activate = reactive(input$justification),
      output_parameters = output_parameters,
      method = "traditional")
    
    # Set code parameters
    code_parameters <- reactive({
      list(
        tpr = input$tpr,
        max_n = input$max_n,
        delta = input$delta
      )
    })
    
    # Render code preview
    mod_code_server(
      "code",
      code_parameters = code_parameters,
      method = "traditional")
  })
}
    
## To be copied in the UI
# mod_ssp_power_traditional_ui("traditional")
    
## To be copied in the server
# mod_ssp_power_traditional_server("traditional")
 
