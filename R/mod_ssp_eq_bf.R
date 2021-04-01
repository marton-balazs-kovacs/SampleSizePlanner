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
mod_ssp_eq_bf_ui <- function(id) {
  tagList(
    # Method
    h1("Interval equivalence Bayes factor", class = "method-title"),
    sidebarLayout(
      sidebarPanel(
        # Panel title
        h3("Determine your sample size", class = "subtitle"),
        # Method description
        p("A Bayesian statistical testing approach aimed at establishing equivalence between two groups."),
        # Calculation settings
        ## TPR input
        sliderInput(
          NS(id, "tpr"),
          HTML(
            '<div title="The desired long-run probability of obtaining a Bayes factor at least as high as the Threshold, given Delta.">',
            'True Positive Rate (TPR)',
            '<i class="fas fa-info"></i>',
            '</div>'),
          min = 0,
          max = 1,
          value = 0.8,
          step = 0.01),
        sliderInput(
          NS(id, "eq_band"),
          HTML(
            '<div title="The chosen width of the equivalence region.">',
            'Equivalence Band (EqBand)',
            '<i class="fas fa-info"></i>',
            '</div>'),
          min = 0,
          max = 1,
          value = 0.2,
          step = 0.1),
        sliderInput(
          NS(id, "delta"),
          HTML(
            '<div title="The expected population effect size.">',
            'Delta',
            '<i class="fas fa-info"></i>',
            '</div>'),
          min = 0,
          max = 2,
          value = 0,
          step = 0.1),
        selectInput(
          NS(id, "thresh"),
          HTML(
            '<div title="Critical threshold for the Bayes factor.">',
            'Threshold',
            '<i class="fas fa-info"></i>',
            '</div>'),
          choices = c(10, 6, 3),
          selected = 10),
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
                NS(id, "eq_band_justification"),
                label = "Equivalence Band (EqBand)",
                choices = c(
                  "previous studies reported a similar equivalence region",
                  " of the following substantive reasons: ...",
                  "other..."),
                multiple = FALSE,
                options = list(create = TRUE)),
              selectizeInput(
                NS(id, "delta_justification"),
                label = "Delta",
                choices = c(
                  "we expected no difference between the groups",
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
    
#' @rdname mod_ssp_eq_bf
#' @export
#' @keywords internal
  
mod_ssp_eq_bf_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Setup loadingbar
    # waitress <- waiter::Waitress$new("#eq_bf-preview-show_preview", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)
    
    # Calculate results
    eq_bf_result <- eventReactive(input$calculate, {
      # waitress$start()
      ssp_eq_bf(tpr = input$tpr, eq_band = input$eq_band, delta = input$delta, thresh = as.integer(input$thresh))
    })
    
    # Show calculated results
    output$calculate_output <- renderUI({
      HTML(
        glue::glue(
          "<b>n1:</b> {n1}<br/><b>Resulting TPR:</b> {round(npower, 1)}",
          n1 = eq_bf_result()$n1,
          npower = round(eq_bf_result()$npower, 2)
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
        eq_band = input$eq_band,
        delta = input$delta,
        delta_justification = input$delta_justification,
        eq_band_justification = input$eq_band_justification,
        tpr_justification = input$tpr_justification,
        n1 = eq_bf_result()$n1,
        npower = eq_bf_result()$npower,
        thresh = input$thresh
      )
    })
    
    # Render justification
    mod_preview_server(
      "preview",
      activate = reactive(input$justification),
      output_parameters = output_parameters,
      method = "eq_bf")
    
    # Set code parameters
    code_parameters <- reactive({
      list(
        tpr = input$tpr,
        eq_band = input$eq_band,
        delta = input$delta,
        thresh = input$thresh
      )
    })
    
    # Render code preview
    mod_code_server(
      "code",
      code_parameters = code_parameters,
      method = "eq_bf")
  })
}
    
## To be copied in the UI
# mod_ssp_eq_bf_ui("eq_bf")
    
## To be copied in the server
# mod_ssp_eq_bf_server("eq_bf")
 