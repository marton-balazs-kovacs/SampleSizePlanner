# Module UI
  
#' @title   mod_ssp_power_curve_ui and mod_ssp_power_curve_server
#' @description  A shiny Module.
#'
#' @rdname mod_ssp_power_curve
#'
#' @keywords internal
#' @noRd 
#' @importFrom shiny NS tagList 
mod_ssp_power_curve_ui <- function(id) {
  tagList(
    # Method
    h1("Power curve"),
    sidebarLayout(
      sidebarPanel(
        # Panel title
        h3("Determine your sample size", class = "subtitle"),
        # Method description
        p("The power curve shows how changes in effect size modify the statistical power of a test. It is is similar to a classical power analysis but instead of calculating the appropriate sample size for one hypothesized population effect size, the method calculates the required sample size for a range of plausible population effect sizes."),
        # Calculation settings
        ## TPR input
        sliderInput(
          NS(id, "tpr"),
          name_with_info(
            "True Positive Rate (TPR)",
            "The desired long-run probabilities of obtaining a significant result with a one-sided t-test, given each value of Delta."),
          min = 0,
          max = 1,
          value = 0.8,
          step = 0.01),
        sliderInput(
          NS(id, "delta"),
          name_with_info(
            "Delta",
            "A range of hypothetical population effect sizes."),
          min = 0,
          max = 2,
          value = c(0.1, 0.9),
          step = 0.1),
        numericInput(
          NS(id, "max_n"),
          name_with_info(
            "Maximum N",
            "The maximum number of participants per group (both groups are assumed to have equal sample size)."),
          min = 10,
          max = 20000,
          value = 5000,
          step = 1),
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
                  "we have no clear expectation of the magnitude of delta",
                  "we expected the delta to be around...",
                  "other..."),
                multiple = FALSE,
                options = list(create = TRUE)),
              # Create justification text
              actionButton(NS(id, "justification"), "Create justification report", class = "calculate-btn justification-btn"),
              # Show justification text
              mod_preview_ui(NS(id, "preview")),
              wellPanel(
                style = "margin-top: 15px;",
                plotly:: plotlyOutput(NS(id, "figure_preview"))),
              div(class = "download-btn",
                  downloadButton(NS(id, "figure_download"), label = "Download figure"))
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
    
#' @rdname mod_ssp_power_curve
#' @noRd
#' @keywords internal
mod_ssp_power_curve_server <- function(id) {
    moduleServer(id, function(input, output, session) {
      # Setup loadingbar
      # waitress <- waiter::Waitress$new("#curve-preview-show_preview", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)
      
      # Create delta vector
      delta <- reactive({
        seq(input$delta[1], input$delta[2], 0.01)
      })
      
      # Calculate results
      curve_result <- eventReactive(input$calculate, {
        # waitress$start()
        ssp_power_curve(delta = delta(), tpr = input$tpr, max_n = input$max_n)
        })
      
      # Show calculated results
      output$calculate_output <- renderUI({
        if ("n1" %in% names(curve_result())) {
        HTML(
          glue::glue(
            "<b>n1:</b> {n1} <b>delta:</b> {delta}<br/>",
            n1 = curve_result()$n1,
            delta = curve_result()$delta
          )
        )
        } else {
          HTML(
            glue::glue(
              "<b>{error_message}</b>",
              error_message = curve_result()$message
            )
          )
        }
      })
      
      # Add downloadbutton enable logic
      observe({
        if (input$calculate  && "n1" %in% names(curve_result())) {
          shinyjs::enable("justification")
          shinyjs::runjs("$('.justification-btn').removeAttr('title');")
        } else{
          shinyjs::disable("justification")
          shinyjs::runjs("$('.justification-btn').attr('title', 'Please run the calculation first');")
        }
      })
      
      # Justification text
      ## List output parameters
      output_parameters <- reactive({
        list(
          delta = input$delta,
          delta_justification = input$delta_justification,
          tpr_justification = input$tpr_justification,
          n1 = curve_result()$n1
        )
      })
      
      ## Render justification text preview
      mod_preview_server(
        "preview",
        activate = reactive(input$justification),
        deactivate = reactive(input$calculate),
        output_parameters = output_parameters,
        method = "power_curve")
      
      # Figure
      ## Create figure
      shinyjs::disable("figure_download")
      figure <- reactiveVal(NULL)
      
      observeEvent(input$justification, {
        figure(
          plot_power_curve(
            delta = curve_result()$delta,
            n1 = curve_result()$n1,
            animated = FALSE)
        )
        shinyjs::enable("figure_download")
      })
      
      observeEvent(input$calculate, {
        shinyjs::disable("figure_download")
        figure(NULL)
      })
      
      ## Show figure preview
      output$figure_preview <- plotly::renderPlotly({
        if (is.null(figure())) {
          NULL
        } else {
        figure() %>% 
          plotly::ggplotly(tooltip = "text") %>% 
          plotly::config(displayModeBar = F)
        }
      })
      
      ## Download the figure
      output$figure_download <- downloadHandler(
        filename = function() {
          paste0("power_curve", "_", Sys.Date(), ".png")
        },
        content = function(file) {
          ggplot2::ggsave(file, plot = figure(), device = "png")
        },
        contentType = "image/png")
      
      # Set code parameters
      code_parameters <- reactive({
        list(
          tpr = input$tpr,
          delta_min = input$delta[1],
          delta_max = input$delta[2],
          max_n = input$max_n
        )
      })
      
      # Render code preview
      mod_code_server(
        "code",
        code_parameters = code_parameters,
        method = "power_curve")
  })
}
    
## To be copied in the UI
# mod_ssp_power_curve_ui("curve")
    
## To be copied in the server
# mod_ssp_power_curve_server("curve")
 
