# Module UI

#' @title   mod_ssp_power_traditional_anova_ui and mod_ssp_power_traditional_anova_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_ssp_power_traditional_anova
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_ssp_power_traditional_anova_ui <- function(id) {
  tagList(
    # Method
    h1("Classical power analysis - Two-way ANOVA"),
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
            "The desired long-run probability of obtaining a significant result with a one-sided t-test, given Delta."),
          min = 0,
          max = 1,
          value = 0.8,
          step = 0.01),
        ## Input Choice of Effects
        selectizeInput(
          NS(id, "effect"),
          name_with_info(
            "Which effect's power you want to detect?",
            "Determine which effect of the ANOVA analysis, in which you want to check for power"),
          c("Main Effect A", "Main Effect B", "Interaction Effect")),
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
        ## Input Maximum N
        numericInput(
          NS(id, "max_n"),
          name_with_info(
            "Maximum N per group",
            "The maximum number of participants per group (all groups are assumed to have equal sample size)."),
          min = 10,
          max = 20000,
          value = 300,
          step = 1),
        ## Input Number of Iteration
        numericInput(
          NS(id, "iter"),
          name_with_info(
            "Number of iterations",
            "Numebr of iterations for calculating the power"),
          min = 10,
          max = 5000,
          value = 500,
          step = 10),
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
                  "our reasoning that ...",
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
#' @export
#' @keywords internal

mod_ssp_power_traditional_anova_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    # waitress <- waiter::Waitress$new("#traditional-preview-show_preview", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)
  
    # Calculate results
    traditional_result <- eventReactive(input$calculate, {
      # waitress$start()
      safe_ssp_power_traditional_anova <- purrr::safely(ssp_power_traditional_anova)
      res <- safe_ssp_power_traditional_anova(
        effect = input$effect, 
        iter   = input$iter, 
        mu     = as.vector(input$muMatrix),
        sigma  = input$sigma,
        max_n  = input$max_n, 
        tpr    = input$tpr, 
      )
      
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
        tpr_out = traditional_result()$tpr_out
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
        tpr    = input$tpr
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