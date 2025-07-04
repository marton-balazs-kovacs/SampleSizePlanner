# Module UI
  
#' @title   mod_ssp_bfda_ui and mod_ssp_bfda_server
#' @description  A shiny Module.
#' 
#' @rdname mod_ssp_bfda
#'
#' @keywords internal
#' @noRd 
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
        p("The present method provides an expected sample size such that compelling evidence in the form of a Bayes factor can be collected for a given effect size with a certain long-run probability when allowing for sequential testing. Set delta = 0 to determine the sample size for H0 and delta > 0 for H1."),
        # Calculation settings
        sliderInput(
          NS(id, "tpr"),
          name_with_info(
            "True Positive Rate (TPR)",
            "The long-run probability of obtaining a Bayes factor at least as high as the critical threshold favoring superiority, given Delta."),
          min = 0.5,
          max = 0.95, 
          value = 0.8, 
          step = 0.05),
        ## Delta input
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
            "The Bayes factor threshold for inference"),
          choices = c(3, 6, 10),
          selected = 3),
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
    
#' @rdname mod_ssp_bfda
#' @noRd
#' @keywords internal
mod_ssp_bfda_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Setup loadingbar
    # waitress <- waiter::Waitress$new("#bfda-preview-show_preview", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)
    
    input_prior_scale <- reactive({
      switch(
        input$prior_scale,
        "1/sqrt(2)" = 1/sqrt(2),
        "1" = 1,
        "sqrt(2)" = sqrt(2)
      )
    })
    
    # Calculate results
    bfda_result <- eventReactive(input$calculate, {
      # waitress$start()
      
      bfda_precalculation_results %>% 
        dplyr::filter(
          dplyr::near(tpr_out, input$tpr),
          dplyr::near(delta, input$delta),
          thresh == as.integer(input$thresh),
          dplyr::near(prior_scale, input_prior_scale()),
        ) %>% 
        dplyr::select(n1, tpr_out, h0, ha, error_message) %>% 
        as.list()
    })
    
    # Show calculated results
    output$calculate_output <- renderUI({
      if (!is.na(bfda_result()$n1)) {
      HTML(
        glue::glue(
          "<b>n1:</b> {n1}<br/><b>TPR:</b> {tpr_out}<br/><b>Ha:</b> {ha}<br/><b>H0:</b> {h0}<br/><br/> \\
          <i>The numbers after Ha and H0 indicate the proportion of times either Ha (BF>threshold) and H0 (BF<(1/threshold)) are reached. These numbers do not necessarily sum to 1 as sometimes neither is reached before the maximum N.</i>",
          n1 = bfda_result()$n1,
          tpr_out = bfda_result()$tpr_out,
          ha = bfda_result()$ha,
          h0 = bfda_result()$h0
        )
      )
      } else {
        HTML(
          glue::glue(
            "<b>{error_message}</b> \\
            <i>Consider running the sample size calculation locally in R for more control over the parameters and detailed diagnostics.</i>",
            error_message = bfda_result()$error_message
          )
        )
      }
    })
    
    # Add justification enable logic
    observe({
      if (input$calculate && !is.na(bfda_result()$n1)) {
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
        delta = input$delta,
        delta_justification = input$delta_justification,
        tpr_justification = input$tpr_justification,
        n1 = bfda_result()$n1,
        error_message = bfda_result()$error_message,
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
      method = "bfda")
    
    # Set code parameters
    code_parameters <- reactive({
      list(
        tpr = input$tpr,
        delta = input$delta,
        thresh = input$thresh,
        prior_scale = input$prior_scale
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
 
