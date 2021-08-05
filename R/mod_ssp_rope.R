# Module UI
  
#' @title   mod_ssp_rope_ui and mod_ssp_rope_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_ssp_rope
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_ssp_rope_ui <- function(id){
  tagList(
    # Method
    h1("Region of Practical Equivalence (ROPE)", class = "method-title"),
    sidebarLayout(
      sidebarPanel(
        # Panel title
        h3("Determine your sample size", class = "subtitle"),
        # Method description
        p("The ROPE procedure identifies the 95% highest density interval (HDI; other percentages are permissible as well) and determines whether or not the HDI is fully contained within the equivalence interval."),
        # Calculation settings
        ## TPR input
        sliderInput(
          NS(id, "tpr"),
          name_with_info(
            "True Positive Rate (TPR)",
            "The desired long run probability of having the HDI fully contained within the ROPE interval, given Delta."),
          min = 0.5,
          max = 0.95,
          value = 0.8,
          step = 0.05),
        sliderInput(
          NS(id, "eq_band"),
          name_with_info(
            "Equivalence Band (EqBand)",
            "The chosen ROPE interval."),
          min = 0.1,
          max = 0.5,
          value = 0.2,
          step = 0.01),
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
                  "previous studies reported a similar region of practical equivalence",
                  "of the following substantive reasons: ...",
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
    
#' @rdname mod_ssp_rope
#' @export
#' @keywords internal
mod_ssp_rope_server <- function(id){
  moduleServer(id, function(input, output, session) {
    # Setup loadingbar
    # waitress <- waiter::Waitress$new("#rope-preview-show_preview", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)
    # Calculate results
    rope_result <- eventReactive(input$calculate, {
      # waitress$start()
      prior_scale <- switch(
        input$prior_scale,
        "1/sqrt(2)" = 1/sqrt(2),
        "1" = 1,
        "sqrt(2)" = sqrt(2)
      )
      
      rope_precalculation_results %>% 
        dplyr::filter(
          dplyr::near(tpr, input$tpr),
          dplyr::near(delta, input$delta),
          dplyr::near(eq_band, input$eq_band),
          prior_scale == prior_scale
        ) %>% 
        dplyr::select(n1, npower, error_message) %>% 
        as.list()
      })
    
    # Show calculated results
    output$calculate_output <- renderUI({
      HTML(
        glue::glue(
          "<b>n1:</b> {n1}<br/><b>Resulting TPR:</b> {npower}",
          n1 = rope_result()$n1,
          npower = round(rope_result()$npower, 2)
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
        n1 = rope_result()$n1,
        npower = rope_result()$npower,
        error_message = rope_result()$error_message,
        prior_scale = input$prior_scale
      )
    })
    
    # Render justification
    mod_preview_server(
      "preview",
      activate = reactive(input$justification),
      output_parameters = output_parameters,
      method = "rope")
    
    # Set code parameters
    code_parameters <- reactive({
      list(
        tpr = input$tpr,
        eq_band = input$eq_band,
        delta = input$delta,
        prior_scale = input$prior_scale
      )
    })
    
    # Render code preview
    mod_code_server(
      "code",
      code_parameters = code_parameters,
      method = "rope")
  })
}
    
## To be copied in the UI
# mod_ssp_rope_ui("rope")
    
## To be copied in the server
# mod_ssp_rope_server("rope")
 
