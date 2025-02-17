# Module UI
  
#' @title   mod_ssp_bf_thresh_ui and mod_ssp_bf_thresh_server
#' @description  A shiny Module.
#'
#' @rdname mod_ssp_bf_thresh
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_ssp_bf_predetermined_ui <- function(id) {
  tagList(
    # Method
    h1("Predetermined sample size with Bayes factor", class = "method-title"),
    sidebarLayout(
      sidebarPanel(
        # Panel title
        h3("Determine your sample size", class = "subtitle"),
        # Method description
        p("The present method calculates the corresponding default Bayes factor for a t-test statistic with Cauchy prior distribution centered on zero with scale parameter 1/sqrt(2) for several sample sizes."),
        # Calculation settings
        ## TPR input
        sliderInput(
          NS(id, "tpr"),
          name_with_info(
            "True Positive Rate (TPR)",
            "The long-run probability of obtaining a Bayes factor at least as high as the critical threshold favoring superiority, given Delta."),
          min = 0,
          max = 1,
          value = 0.8,
          step = 0.01),
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
        ## Maximum N input
        numericInput(
          NS(id, "max_n"),
          name_with_info(
            "Maximum N",
            "The maximum number of participants per group (both groups are assumed to have equal sample size)."),
          min = 10,
          max = 20000,
          value = 5000,
          step = 1),
      selectInput(
        NS(id, "thresh"),
        name_with_info(
          "Threshold",
          "Critical threshold for the Bayes factor."),
        choices = c(10, 6, 3),
        selected = 10),
      selectInput(
        NS(id, "prior_scale"),
        name_with_info(
          "Prior Scale",
          "Scale of the Cauchy prior distribution."),
        choices = c("1/sqrt(2)", "1", "sqrt(2)"),
        selected = "1/sqrt(2)"),
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
              p("The template justification boilerplate sentences should be supplemented with further details based on the context of the research."),
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
                  "of the following substantive reasons: ...",
                  "other..."),
                multiple = FALSE,
                options = list(create = TRUE)),
              # Create justification text
              actionButton(NS(id, "justification"), "Create justification report", class = "calculate-btn justification-btn"),
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
    
#' @rdname mod_ssp_bf_thresh
#' @export
#' @keywords internal
mod_ssp_bf_predetermined_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Setup loadingbar
    # waitress <- waiter::Waitress$new("#bf_thresh-preview-show_preview", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)
    
    # Calculate results
    bf_predetermined_result <- eventReactive(input$calculate, {
      # Waitress$start()
      prior_scale <- switch(
        input$prior_scale,
        "1/sqrt(2)" = 1/sqrt(2),
        "1" = 1,
        "sqrt(2)" = sqrt(2)
      )
      
      ssp_bf_predetermined(
        tpr = input$tpr,
        delta = input$delta,
        thresh = as.integer(input$thresh),
        max_n = input$max_n,
        prior_scale = prior_scale)
      })
    
    # Show calculated results
    output$calculate_output <- renderUI({
      HTML(
        glue::glue(
          "<b>n1:</b> {n1}<br/><b>Resulting TPR:</b> {tpr_out}",
          n1 = bf_predetermined_result()$n1,
          tpr_out = round(bf_predetermined_result()$tpr_out, 2)
        )
      )
    })
    
    # Add justification enable logic
    observe({
      if (input$calculate && !is.na(bf_predetermined_result()$n1)) {
        shinyjs::enable("justification")
        shinyjs::runjs("$('.justification-btn').removeAttr('title');")
      } else{
        shinyjs::disable("justification")
        shinyjs::runjs("$('.justification-btn').attr('title', 'Please run the calculation first');")
      }
    })

    # Set output parameters
    output_parameters <- reactive({
      list(
        tpr = input$tpr,
        tpr_justification = input$tpr_justification,
        delta = input$delta,
        delta_justification = input$delta_justification,
        n1 = bf_predetermined_result()$n1,
        tpr_out = bf_predetermined_result()$tpr_out,
        thresh = as.integer(input$thresh),
        prior_scale = input$prior_scale
      )
    })
    
    # Render justification
    mod_preview_server(
      "preview",
      activate = reactive(input$justification),
      deactivate = reactive(input$calculate),
      output_parameters = output_parameters,
      method = "bf_predetermined")
    
    # Set code parameters
    code_parameters <- reactive({
      list(
        tpr = input$tpr,
        thresh = input$thresh,
        delta = input$delta,
        max_n = input$max_n,
        prior_scale = input$prior_scale
      )
    })
    
    # Render code preview
    mod_code_server(
      "code",
      code_parameters = code_parameters,
      method = "bf_predetermined")
  })
}
    
## To be copied in the UI
# mod_ssp_bf_predetermined_ui("bf_predetermined")
    
## To be copied in the server
# mod_ssp_bf_predetermined_server("bf_predetermined")
 
