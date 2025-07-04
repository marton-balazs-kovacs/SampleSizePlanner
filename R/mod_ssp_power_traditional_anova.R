# Module UI

#' @title   mod_ssp_power_traditional_anova_ui and mod_ssp_power_traditional_anova_server
#' @description  A shiny Module.
#'
#' @rdname mod_ssp_power_traditional_anova
#'
#' @keywords internal
#' @noRd 
#' @importFrom shiny NS tagList 
#' 
mod_ssp_power_traditional_anova_ui <- function(id) {
  tagList(
    
    # Method
    h1("Classical Power Analysis - Two-way ANOVA"),
    sidebarLayout(
      sidebarPanel(
        # Panel title
        h3("Determine your sample size", class = "subtitle"),
        # Method description
        p("This method is used to estimate the minimum sample size that a design needs to reach a certain level of statistical power, given a desired significance level and expected effect size."),
        # Calculation settings
        ## TPR input
        sliderInput(
          NS(id, "tpr"),
          name_with_info(
            "True Positive Rate (TPR)",
            "The desired long-run probability of obtaining a significant result, given the means."),
          min = 0,
          max = 1,
          value = 0.8,
          step = 0.01),
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
        ## Input Maximum N
        numericInput(
          NS(id, "max_n"),
          name_with_info(
            "Maximum N per group",
            "The maximum group size (all groups are assumed to have equal sample size)."),
          min = 10,
          max = 20000,
          value = 300,
          step = 1),
        ## Input Number of Iteration
        numericInput(
          NS(id, "iter"),
          name_with_info(
            "Number of iterations",
            "The number of iterations to calculate the true positive rate (power)"),
          min = 10,
          # max = 5000,
          value = 5000,
          step = 10),
        # Alpha
        numericInput(
          NS(id, "alpha"),
          name_with_info(
            "Alpha",
            "The significance level (Type error rate) to calculate the true positive rate (power)"),
          min = 0,
          max = 1,
          value = 0.05,
          step = 0.01),
        # Seed
        numericInput(
          NS(id, "seed"),
          name_with_info(
            "Seed",
            "Setting a seed for the reproducibility of the calculated samplesize."),
          value = 42,      
          min = 1,         
          step = 1      
        ),
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

#' @rdname mod_ssp_power_traditional_anova
#' @noRd
#' @keywords internal

mod_ssp_power_traditional_anova_server <- function(id) {

  moduleServer(id, function(input, output, session) {
    # waitress <- waiter::Waitress$new("#traditional-preview-show_preview", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)
    
    traditional_result <- eventReactive(input$calculate, {
      showModal(modalDialog("Processing... this can take a minute", footer=NULL))
      # waitress$start()
      safe_ssp_power_traditional_anova <- purrr::safely(ssp_power_traditional_anova)
      res <- safe_ssp_power_traditional_anova(
        effect = input$effect, 
        iter   = input$iter, 
        mu     = as.vector(input$muMatrix),
        sigma  = input$sigma,
        max_n  = input$max_n, 
        tpr    = input$tpr,
        alpha  = input$alpha,
        seed   = input$seed
      )
      removeModal()
      if (is.null(res$error)) {
        res$result
      } else {
        res$error
      }
      })
    
    # Show calculated results
    output$calculate_output <- renderUI({
      if ("n1" %in% names(traditional_result())) {
        HTML(
          glue::glue(
            "
            <b>n per group:</b> {n1}<br/>
            <b>Resulting TPR:</b> {tpr_out}<br/>
            <b>TPR for:</b> {effect}
            ",
            effect = traditional_result()$effect,
            n1 = traditional_result()$n1,
            tpr_out = round(traditional_result()$tpr_out, 2)
          )
        )
      } else {
        HTML(
          glue::glue(
            "<b>{error_message}</b>",
            error_message = traditional_result()$message
          )
        )
      }
    })
    
    # Add justification enable logic
    observe({
      if (input$calculate && "n1" %in% names(traditional_result())) {
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
        n1 = traditional_result()$n1,
        tpr_out = traditional_result()$tpr_out,
        mu = input$muMatrix,
        sigma = input$sigma,
        effect = input$effect,
        alpha = input$alpha
      )
    })

    # Render preview
    mod_preview_server(
      "preview",
      activate = reactive(input$justification),
      deactivate = reactive(input$calculate),
      output_parameters = output_parameters,
      method = "traditional-twoway-anova"
    )

    # Set code parameters
    code_parameters <- reactive({
      list(
        effect = input$effect,
        iter   = input$iter,
        max_n  = input$max_n,
        mu     = as.vector(input$muMatrix),
        sigma  = input$sigma,
        tpr    = input$tpr,
        alpha  = input$alpha
        # delta = input$delta
      )
    })

    # Render code preview
    mod_code_server(
      "code",
      code_parameters = code_parameters,
      method = "traditional-twoway-anova"
    )
  })
}

## To be copied in the UI
# mod_ssp_power_traditional_ui("traditional")

## To be copied in the server
# mod_ssp_power_traditional_server("traditional")