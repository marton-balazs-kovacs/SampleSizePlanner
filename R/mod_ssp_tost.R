# Module UI
  
#' @title   mod_ssp_tost_ui and mod_ssp_tost_server
#' @description  A shiny Module.
#'
#' @rdname mod_ssp_tost
#'
#' @keywords internal
#' @noRd 
#' @importFrom shiny NS tagList 
mod_ssp_tost_ui <- function(id) {
  tagList(
    # Method
    h1("Two One-Sided Tests (TOST)", class = "method-title"),
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
          name_with_info(
            "True Positive Rate (TPR)",
            "The desired long run probability of obtaining a significant result with TOST, given Delta."),
          min = 0,
          max = 1,
          value = 0.8,
          step = 0.01),
        ## EqBand input
        sliderInput(
          NS(id, "eq_band"),
          name_with_info(
            "Equivalence Band (EqBand)",
            "The chosen width of the region for practical equivalence, i.e. the SESOI."),
          min = 0,
          max = 1,
          value = 0.2,
          step = 0.1),
        ## Delta input
        sliderInput(
          NS(id, "delta"),
          name_with_info(
            "Delta",
            "The expected population effect size. In most cases, this value will be zero."),
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
              # Justification for EqBand
              selectizeInput(
                NS(id, "eq_band_justification"),
                label = "Equivalence Band (EqBand)",
                choices = c(
                  "previous studies reported a similar equivalence band ...",
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
    
#' @rdname mod_ssp_tost
#' @noRd
#' @keywords internal
mod_ssp_tost_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Setup loadingbar
    # waitress <- waiter::Waitress$new("#tost-preview-show_preview", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)
    
    # Calculate results
    tost_result <- eventReactive(input$calculate, {
      # waitress$start()
      safe_ssp_tost <- purrr::safely(ssp_tost)
      res <- safe_ssp_tost(tpr = input$tpr, eq_band = input$eq_band, delta = input$delta)
      if (is.null(res$error)) {
        res$result
        } else {
          res$error
          }
      })
    
    # Show calculated results
    output$calculate_output <- renderUI({
      if ("n1" %in% names(tost_result())) {
      HTML(
        glue::glue(
          "<b>n1:</b> {n1}<br/><b>Resulting TPR:</b> {tpr_out}",
          n1 = tost_result()$n1,
          tpr_out = round(tost_result()$tpr_out, 2)
          )
        )
      } else {
        HTML(
          glue::glue(
            "<b>{error_message}</b>",
            error_message = tost_result()$message
          )
        )
      }
    })
    
    # Add justification enable logic
    observe({
      if (input$calculate && "n1" %in% names(tost_result())) {
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
        eq_band = input$eq_band,
        delta = input$delta,
        delta_justification = input$delta_justification,
        eq_band_justification = input$eq_band_justification,
        tpr_justification = input$tpr_justification,
        n1 = tost_result()$n1,
        tpr_out = tost_result()$tpr_out
      )
    })

    # Render justification
    mod_preview_server(
      "preview",
      activate = reactive(input$justification),
      deactivate = reactive(input$calculate),
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
 
