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
mod_ssp_bfda_ui <- function(id) {
  tagList(
    # Method
    h1("Bayes Factor Design Analysis (BFDA)", class = "method-title"),
    sidebarLayout(
      sidebarPanel(
        # Panel title
        h3("Determine your sample size", class = "subtitle"),
        # Method description
        p("The present method estimates the long-run rates of misleading evidence that one can expect for a specific research design if using preset Bayes Factor thresholds and allowing for sequential testing."),
        # Calculation settings
        sliderInput(
          NS(id, "tpr"),
          list(
            "True Positive Rate (TPR)",
            HTML('<i class="fas fa-info"; title="The long-run probability of obtaining a Bayes factor at least as high as the critical threshold favoring superiority, given Delta."></i>')),
          min = 0,
          max = 1, 
          value = 0.8, 
          step = 0.1),
        ## Delta input
        sliderInput(
          NS(id, "delta"),
          list(
            "Delta",
            HTML('<i class="fas fa-info"; title="The expected population effect size."></i>')),
          min = 0, 
          max = 2, 
          value = 0,
          step = 0.1),
        selectInput(
          NS(id, "thresh"), 
          list(
            "Threshold",
            HTML('<i class="fas fa-info"; title="The Bayes factor threshold for inference"></i>')),
          choices = 3,
          selected = 3),
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
              selectizeInput(NS(id, "tpr_justification"),
                             label = "True Positive Rate (TPR)",
                             choices = c(
                               "it is the common standard in the field",
                               "it is the journal publishing requirement",
                               "other..."),
                             multiple = FALSE,
                             options = list(create = TRUE)),
              # Justification for Delta
              selectizeInput(NS(id, "delta_justification"),
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
    
#' @rdname mod_ssp_bfda
#' @export
#' @keywords internal
mod_ssp_bfda_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Setup loadingbar
    # waitress <- waiter::Waitress$new("#bfda-preview-show_preview", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)
    
    # Calculate results
    bfda_result <- eventReactive(input$calculate, {
      # waitress$start()
      bfda_precalculation_results %>% 
        dplyr::filter(
          dplyr::near(tpr, input$tpr),
          dplyr::near(delta, input$delta),
          thresh == as.integer(input$thresh)
        ) %>% 
        dplyr::select(n1, tpr_out, h0, ha, error_message) %>% 
        as.list()
    })
    
    # Show calculated results
    output$calculate_output <- renderUI({
      HTML(
        glue::glue(
          "<b>n1:</b> {n1}<br/><b>tpr:</b> {tpr_out}<br/><b>ha:</b> {ha}<br/><b>h0:</b> {h0}",
          n1 = bfda_result()$n1,
          tpr_out = bfda_result()$tpr_out,
          ha = bfda_result()$ha,
          h0 = bfda_result()$h0
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
        n1 = bfda_result()$n1,
        error_message = bfda_result()$error_message,
        thresh = input$thresh
      )
    })
    
    # Render justification
    mod_preview_server(
      "preview",
      activate = reactive(input$justification),
      output_parameters = output_parameters,
      method = "bfda")
    
    # Set code parameters
    code_parameters <- reactive({
      list(
        tpr = input$tpr,
        delta = input$delta,
        thresh = input$thresh
      )
    })
    
    # Render code preview
    mod_code_server(
      "code",
      code_parameters = code_parameters,
      method = "bfda")
  })
}
    
## To be copied in the UI
# mod_ssp_bfda_ui("bfda")
    
## To be copied in the server
# mod_ssp_bfda_server("bfda")
 
