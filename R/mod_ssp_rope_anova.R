# Module UI

#' @title   mod_ssp_rope_anova_ui and mod_ssp_rope_anova_server
#' @description  A shiny Module.
#'
#' @rdname mod_ssp_rope_anova
#'
#' @keywords internal
#' @noRd 
#' @importFrom shiny NS tagList 
mod_ssp_rope_anova_ui <- function(id) {
  tagList(
    # Method
    h1("Region of Practical Equivalence (ROPE) - Two-way ANOVA"),
    sidebarLayout(
      sidebarPanel(
        # Panel title
        h3("Determine your sample size", class = "subtitle"),
        # Method description
        p("The ROPE procedure identifies the highest density interval (HDI) and determines whether or not the HDI is fully contained within the equivalence interval. The output shows results from pre-calculations."),
        # Calculation settings
        ## TPR input
        sliderInput(
          NS(id, "tpr"),
          name_with_info(
            "True Positive Rate (TPR)",
            "The desired long-run probability of the HDI fully falling inside the ROPE, given the means."),
          min = 0.7,
          max = 0.9,
          value = 0.8,
          step = 0.05),
        ## eq band input
        sliderInput(
          NS(id, "eq_band"),
          name_with_info(
            "Equivalence Band (EqBand)",
            "The margin of the standardized ROPE interval"),
          min = 0.1,
          max = 0.3,
          value = 0.2,
          step = 0.05),
        ## Input Choice of Effects
        selectizeInput(
          NS(id, "effect"),
          name_with_info(
            "Target Effect",
            "The effect of interest for the minimum sample size estimation"),
          choices = c(
            "Main Effect A" = "Main Effect 1",
            "Main Effect B" = "Main Effect 2",
            "Interaction Effect" = "Interaction Effect"
            )
          ),
        ## Input Mean for Each Group
        shinyMatrix::matrixInput(
          NS(id, "muMatrix"),
          label = name_with_info(
            "Mean of Each Group",
            "Specify the unstandardized mean of the dependent variable for each group."),
          value = matrix(c(1, 1.2, 1.5, 1.3), nrow = 1, ncol = 4,
                         dimnames = list(c("mu"),
                                         c("a1_b1", "a1_b2", 
                                           "a2_b1", "a2_b2"))),
          rows = list(names = TRUE),
          cols = list(names = TRUE),
          class = "numeric"),
        ## Input Standard Deviation for All Group
        shinyWidgets::formatNumericInput(
          NS(id, "sigma"),
          name_with_info(
            "Standard Deviation",
            "The standard deviation of the dependent variable for the groups (all groups are assumed to have the same standard deviation)."),
          value = as.numeric(1.2),
          format = "dotDecimalCharCommaSeparator",
          align = "left"),
        ## f2 effect size
        mod_effectsize_f2_ui(NS(id, "f2")),
        ## Iteration input
        selectInput(
          NS(id, "iter"),
          name_with_info(
            "Iterations",
            "Number of iterations for calculating the true positive rate. If specified higher (e.g. iter = 5000), the fluctuations become very small."),
          choices = c(5000),
          selected = 5000),
        ## HDI Confidence Level
        selectInput(
          NS(id, "ci"),
          name_with_info(
            "Highest Density Interval",
            "The percentage of the highest density interval"),
          choices = c(0.95)),
        ## Input prior scale
        selectInput(
          NS(id, "prior_scale"),
          name_with_info(
            "Prior Scale",
            "The scale of the Cauchy prior used to calculate posterior probabilities"),
          choices = c("1 / sqrt(2)"),
          selected = c("1 / sqrt(2)")),
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
              # selectizeInput(
              #   NS(id, "delta_justification"),
              #   label = "Delta",
              #   choices = c(
              #     "previous results published in ...",
              #     "our reasoning that ...",
              #     "other..."),
              #   multiple = FALSE,
              #   options = list(create = TRUE)),
              # Create justification text
              actionButton(NS(id, "justification"), "Create justification report", class = "calculate-btn justification-btn"),
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

#' @rdname mod_ssp_rope_anova
#' @noRd
#' @keywords internal

mod_ssp_rope_anova_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    ran_calculation <- reactiveVal(FALSE)
    
    # dynamic effect size
    mod_effectsize_f2_server("f2", mu = reactive(input$muMatrix), sigma = reactive(input$sigma), effect = reactive(input$effect))
    
    # Calculate results
    pre_result <- eventReactive(input$calculate, {
      ran_calculation(TRUE)
      extract_rope_anova(
        pre_data = rope_anova_data,
        effect_ui = input$effect,
        eq_band_ui = input$eq_band,
        mu_ui     = as.vector(input$muMatrix),
        sigma_ui  = input$sigma,
        tpr_ui    = input$tpr,
        prior_scale_ui = input$prior_scale,
        ci_ui = input$ci
      )
    })
    
    # erase if input changes
    observeEvent(
      list(
        input$tpr, input$effect, input$muMatrix, input$sigma, input$eq_band,
        input$max_n, input$iter, input$thresh, input$prior_scale, input$ci
      ), {
        ran_calculation(FALSE)
      }
    )
    
    # Show calculated results
    output$calculate_output <- renderUI({
      req(ran_calculation())
      res <- pre_result()
      if ("pre_n1" %in% names(res)) {
        HTML(
          glue::glue(
            "<b>n per group:</b> {n1}<br/><b>and mu:</b> {mu}<br/><b>Resulting TPR:</b> {tpr_out}<br/><b>TPR for:</b> {effect}",
            n1 = res$pre_n1[[1]],
            mu = paste(res$pre_mu, collapse = ", "),
            tpr_out = res$pre_tpr_out[[1]],
            effect = input$effect
          )
        )
      } else {
        HTML(
          glue::glue(
            "<b>{error_message}</b>\\
            <i>Consider running the sample size calculation locally in R for more control over the parameters and detailed diagnostics.</i>",
            error_message = res$message[[1]]
          )
        )
      }
    })
    
    # Add justification enable logic
    observe({
      calculated <-ran_calculation()
      res <- pre_result()
      if (calculated && "pre_n1" %in% names(res)) {
        shinyjs::enable("justification")
        shinyjs::runjs("$('.justification-btn').removeAttr('title');")
      } else {
        shinyjs::disable("justification")
        shinyjs::runjs("$('.justification-btn').attr('title', 'Please run the calculation first');")
      }
    })
    
    # Set output parameters
    output_parameters <- reactive({
      req(ran_calculation())
      req("pre_n1" %in% names(pre_result()))
      list(
        tpr = input$tpr,
        tpr_justification = input$tpr_justification,
        eq_band_justification = input$eq_band_justification,
        # delta = input$delta,
        # delta_justification = input$delta_justification,
        n1 = pre_result()$pre_n1[[1]],
        tpr_out = pre_result()$pre_tpr_out[[1]],
        eq_band = input$eq_band,
        ci = input$ci,
        prior_scale = input$prior_scale,
        mu = pre_result()$pre_mu,
        sigma = input$sigma,
        effect = input$effect
      )
    })
    
    
    # Render preview
    mod_preview_server(
      "preview",
      activate = reactive(input$justification && ran_calculation() && "pre_n1" %in% names(pre_result())),
      deactivate = reactive(!ran_calculation() || !("pre_n1" %in% names(pre_result()))),
      output_parameters = output_parameters,
      method = "rope-twoway-anova"
    )
    
    # Set code parameters
    code_parameters <- reactive({
      list(
        mu    = as.vector(input$muMatrix),
        effect = input$effect, 
        eq_band = input$eq_band,
        ci = input$ci,
        sigma  = input$sigma,
        iter = input$iter,
        tpr    = input$tpr, 
        prior_scale = input$prior_scale)
    })
    
    # Render code preview
    mod_code_server(
      "code",
      code_parameters = code_parameters,
      method = "rope-twoway-anova"
    )
  })
}

