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
          name_with_info(
            "True Positive Rate (TPR)",
            'The desired long-run probability of obtaining a Bayes factor at least as high as the Threshold, given Delta.'),
          min = 0.5,
          max = 0.95,
          value = 0.8,
          step = 0.05),
        sliderInput(
          NS(id, "eq_band"),
          name_with_info(
            "Equivalence Band (EqBand)",
            "The chosen width of the equivalence region."),
          min = 0.1,
          max = 0.5,
          value = 0.2,
          step = 0.05),
        sliderInput(
          NS(id, "delta"),
          name_with_info(
            "Delta",
            "The expected population effect size."),
          min = 0,
          max = 2,
          value = 0,
          step = 0.05),
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
                NS(id, "eq_band_justification"),
                label = "Equivalence Band (EqBand)",
                choices = c(
                  "previous studies reported the choice of a similar equivalence region",
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
              actionButton(NS(id, "justification"), "Create justification report", class = "calculate-btn justification-btn"),
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
    
    input_prior_scale <- reactive({
      switch(
        input$prior_scale,
        "1/sqrt(2)" = 1/sqrt(2),
        "1" = 1,
        "sqrt(2)" = sqrt(2)
        )
    })
    
    # Calculate results
    eq_bf_result <- eventReactive(input$calculate, {
      # waitress$start()
      eq_bf_precalculation_results %>% 
        dplyr::filter(
          dplyr::near(tpr, input$tpr),
          dplyr::near(delta, input$delta),
          dplyr::near(eq_band, input$eq_band),
          dplyr::near(prior_scale, input_prior_scale()),
          thresh == as.integer(input$thresh)
        ) %>% 
        dplyr::select(n1, tpr_out, error_message) %>% 
        as.list()
    })
    
    # Show calculated results
    output$calculate_output <- renderUI({
      if (!is.na(eq_bf_result()$n1)) {
        HTML(
          glue::glue(
            "<b>n1:</b> {n1}<br/><b>Resulting TPR:</b> {round(tpr_out, 1)}",
            n1 = eq_bf_result()$n1,
            tpr_out = round(eq_bf_result()$tpr_out, 2)
            )
          )
        } else {
          HTML(
            glue::glue(
              "<b>{error_message}</b>",
              error_message = eq_bf_result()$error_message
              )
          )
          }
    })
    
    # Add justification enable logic
    observe({
      if (input$calculate && !is.na(eq_bf_result()$n1)) {
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
        n1 = eq_bf_result()$n1,
        tpr_out = eq_bf_result()$tpr_out,
        thresh = input$thresh,
        prior_scale = input$prior_scale
      )
    })
    
    # Render justification
    mod_preview_server(
      "preview",
      activate = reactive(input$justification),
      deactivate = reactive(input$calculate),
      output_parameters = output_parameters,
      method = "eq_bf")
    
    # Set code parameters
    code_parameters <- reactive({
      list(
        tpr = input$tpr,
        eq_band = input$eq_band,
        delta = input$delta,
        thresh = input$thresh,
        prior_scale = input$prior_scale
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
 
