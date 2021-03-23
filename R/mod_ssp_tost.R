# Module UI
  
#' @title   mod_ssp_tost_ui and mod_ssp_tost_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_ssp_tost
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_ssp_tost_ui <- function(id) {
  tagList(
    # Method
    h1("Two One‐Sided Tests (TOST)", class = "method-title"),
    sidebarLayout(
      sidebarPanel(
        # Panel title
        h3("Determine your sample size", class = "subtitle"),
        # Method description
        p("TOST is a frequentist statistical testing approach aimed at establishing equivalence between two groups."),
        # Calculation settings
        ## TPR input
        sliderInput(
          NS(id, "tpr"),
          list(
            "True Positive Rate (TPR)",
            HTML('<i class="fas fa-info"; title="The desired long run probability of obtaining a significant result with TOST, given Delta."></i>')),
          min = 0,
          max = 1,
          value = 0.8,
          step = 0.1),
        ## EqBand input
        sliderInput(
          NS(id, "eq_band"),
          list(
            "Equivalence Band (EqBand)",
            HTML('<i class="fas fa-info"; title="The chosen width of the region for practical equivalence, i.e. the SESOI."></i>')),
          min = 0,
          max = 1,
          value = 0.2,
          step = 0.1),
        ## Delta input
        sliderInput(
          NS(id, "delta"),
          list(
            "Delta",
            HTML('<i class="fas fa-info"; title="The expected population effect size. In most cases, this value will be zero."></i>')),
          min = 0,
          max = 2, 
          value = 0, 
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
              # Justification for EqBand
              selectizeInput(
                NS(id, "eq_band_justification"),
                label = "Equivalence Band (EqBand)",
                choices = c(
                  "previous studies reported a similar SESOI ...",
                  "of the following substantive reasons: ...",
                  "other..."),
                multiple = FALSE,
                options = list(create = TRUE)),
              # Justification for Delta
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
    
#' @rdname mod_ssp_tost
#' @export
#' @keywords internal
mod_ssp_tost_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Setup loadingbar
    # waitress <- waiter::Waitress$new("#tost-preview-show_preview", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)
    
    # Calculate results
    tost_result <- eventReactive(input$calculate, {
      # waitress$start()
      ssp_tost(tpr = input$tpr, eq_band = input$eq_band, delta = input$delta)
      })
    
    # Show calculated results
    output$calculate_output <- renderUI({
      HTML(
        glue::glue(
          "<b>n1:</b> {n1}<br/><b>npower:</b> {npower}",
          n1 = tost_result()$n1,
          npower = tost_result()$npower
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
        n1 = tost_result()$n1,
        npower = tost_result()$npower
      )
    })

    # Render justification
    mod_preview_server(
      "preview",
      activate = reactive(input$justification),
      output_parameters = output_parameters,
      method = "tost")
    
    # Set code parameters
    code_parameters <- reactive({
      list(
        tpr = input$tpr,
        eq_band = input$eq_band,
        delta = input$delta
      )
    })
    
    # Render code preview
    mod_code_server(
    "code",
    code_parameters = code_parameters,
    method = "tost")
    })
}
    
## To be copied in the UI
# mod_ssp_tost_ui("tost")
    
## To be copied in the server
# mod_ssp_tost_server("tost")
 
