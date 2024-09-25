# Module UI

#' @title   mod_ssp_bayesian_anova_ui and mod_ssp_bayesian_anova_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_ssp_bayesian_anova
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_ssp_bayesian_anova_ui <- function(id) {
  tagList(
    # Method
    h1("Bayesian - Two-way ANOVA"),
    sidebarLayout(
      sidebarPanel(
        # Panel title
        h3("Determine your sample size", class = "subtitle"),
        # Method description
        p("This method is used to estimate the minimum sample size that a design needs to reach a certain true positive rate, given a Bayes Factor threshold and expected effect size. The output shows results from pre-calculations"),
        # Calculation settings
        ## TPR input
        sliderInput(
          NS(id, "tpr"),
          name_with_info(
            "True Positive Rate (TPR)",
            "The desired long-run probability of obtaining a Bayes Factor larger than the threshold"),
          min = 0.7,
          max = 0.9,
          value = 0.8,
          step = 0.05),
        ## Input Choice of Effects
        selectizeInput(
          NS(id, "effect"),
          name_with_info(
            "Which effect's power you want to detect?",
            "Determine which effect of the ANOVA analysis, in which you want to check for true positive rate"),
          c("Main Effect 1", "Main Effect 2", "Interaction Effect")),
        ## Input Mean for Each Group
        shinyMatrix::matrixInput(
          NS(id, "muMatrix"),
          label = name_with_info(
            "Mean of Each Group",
            "Specify the mean for each group."),
          value = matrix(c(1, 1.2, 1.5, 1.3), nrow = 1, ncol = 4,
                         dimnames = list(c("mu"),
                                         c("a1_b1", "a1_b2", 
                                           "a2_b1", "a2_b2"))),
          rows = list(names = TRUE),
          cols = list(names = TRUE),
          class = "numeric"),
        ## Input Standard Deviation for All Group
        numericInput(
          NS(id, "sigma"),
          name_with_info(
            "Standard Deviation",
            "The standard deviation per group (all groups are assumed to have the same standard deviation)."),
          min = 1e-3,
          max = 10,
          value = 1.2,
          step = 0.1),
        ## Iteration input
        selectInput(
          NS(id, "iter"),
          name_with_info(
            "Iterations",
            "Number of iterations for calculating the true positive rate."),
          choices = c(1000),
          selected = 1000),
        ## Threshold input
        selectInput(
          NS(id, "thresh"),
          name_with_info(
            "Threshold",
            "Critical threshold for the Bayes factor."),
          choices = c(10),
          selected = 10),
        ## Input prior scale
        selectInput(
          NS(id, "prior_scale"),
          name_with_info(
            "Prior Scale",
            "The prior scale used to calculate posterior probabilities"),
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

#' @rdname mod_ssp_bayesian_anova
#' @export
#' @keywords internal

mod_ssp_bayesian_anova_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {

    
    # Calculate results
    pre_result <- eventReactive(input$calculate, {
      extract_bayesian_anova(
        pre_data = bayes_anova_data,
        effect_ui = input$effect, 
        mu_ui     = as.vector(input$muMatrix),
        sigma_ui  = input$sigma,
        tpr_ui    = input$tpr,
        prior_scale_ui = input$prior_scale,
        thresh_ui = input$thresh
      )
    })
    
    # Show calculated results
    output$calculate_output <- renderUI({
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
            "<b>{error_message}</b>",
            error_message = res$message[[1]]
          )
        )
      }
    })
    
    # Add justification enable logic
    observe({
      if (input$calculate && "pre_n1" %in% names(pre_result())) {
        shinyjs::enable("justification")
        shinyjs::runjs("$('.justification-btn').removeAttr('title');")
      } else {
        shinyjs::disable("justification")
        shinyjs::runjs("$('.justification-btn').attr('title', 'Please run the calculation first');")
      }
    })
    
    # Set output parameters
    output_parameters <- reactive({
      list(
        tpr = input$tpr,
        tpr_justification = input$tpr_justification,
        # delta = input$delta,
        # delta_justification = input$delta_justification,
        n1 = pre_result()$pre_n1[[1]],
        tpr_out = pre_result()$pre_tpr_out[[1]],
        thresh = input$thresh
      )
    })
    
    # Render preview
    mod_preview_server(
      "preview",
      activate = reactive(input$justification),
      deactivate = reactive(input$calculate),
      output_parameters = output_parameters,
      method = "bayesian-twoway-anova"
    )
    
    # Set code parameters
    code_parameters <- reactive({
      list(
        mu    = as.vector(input$muMatrix),
        effect = input$effect, 
        sigma  = input$sigma,
        iter = input$iter,
        tpr    = input$tpr, 
        thresh = input$thresh,
        prior_scale = input$prior_scale)
    })
    
    # Render code preview
    mod_code_server(
      "code",
      code_parameters = code_parameters,
      method = "bayesian-twoway-anova"
    )
  })
}

