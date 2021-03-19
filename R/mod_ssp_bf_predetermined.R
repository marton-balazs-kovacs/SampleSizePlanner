# Module UI
  
#' @title   mod_ssp_bf_thresh_ui and mod_ssp_bf_thresh_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_ssp_bf_thresh
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_ssp_bf_predetermined_ui <- function(id) {
  tagList(
    # Method
    h1("Decide Bayes factor threshold", class = "method-title"),
    sidebarLayout(
      sidebarPanel(
        # Panel title
        h3("Determine your sample size", class = "subtitle"),
        # Method description
        p("valami"),
        # Calculation settings
        ## Delta input
        sliderInput(
          NS(id, "delta"),
          "Delta",
          min = 0,
          max = 2,
          value = 0.5,
          step = 0.1),
        ## TPR input
        sliderInput(
          NS(id, "tpr"),
          "TPR",
          min = 0,
          max = 1,
          value = 0.8,
          step = 0.1),
        ## Maximum N input
        numericInput(
          NS(id, "max_n"),
          "Maximum N",
          min = 10,
          max = 20000,
          value = 5000,
          step = 1),
      selectInput(NS(id, "thresh"), "Threshold", choices = c(10, 6, 3), selected = 10),
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
                label = "Justify TPR",
                choices = c(
                  "it is the common standard in the field",
                  "it is the journal publishing requirement",
                  "other..."),
                multiple = FALSE,
                options = list(create = TRUE)),
              selectizeInput(
                NS(id, "delta_justification"),
                label = "Justify Delta",
                choices = c(
                  "previous results published in ...",
                  "our reasoning that ...",
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
    
#' @rdname mod_ssp_bf_thresh
#' @export
#' @keywords internal
mod_ssp_bf_predetermined_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Setup loadingbar
    # waitress <- waiter::Waitress$new("#bf_thresh-preview-show_preview", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)
    
    # Calculate results
    bf_predetermined_result <- eventReactive(input$calculate, {
      # Waitress$start()
      ssp_bf_predetermined(tpr = input$tpr, delta = input$delta, thresh = as.integer(input$thresh), max_n = input$max_n)
      })
    
    # Show calculated results
    output$calculate_output <- renderUI({
      HTML(
        glue::glue(
          "<b>n1:</b> {n1}<br/><b>npower:</b> {npower}",
          n1 = bf_predetermined_result()$n1,
          npower = bf_predetermined_result()$npower
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
        tpr_justification = input$tpr_justification,
        delta = input$delta,
        delta_justification = input$delta_justification,
        n1 = bf_predetermined_result()$n1,
        npower = bf_predetermined_result()$npower,
        thresh = as.integer(input$thresh)
      )
    })
    
    # Render justification
    mod_preview_server(
      "preview",
      activate = reactive(input$justification),
      output_parameters = output_parameters,
      method = "bf_predetermined")
    
    # Set code parameters
    code_parameters <- reactive({
      list(
        tpr = input$tpr,
        thresh = input$thresh,
        delta = input$delta,
        max_n = input$max_n
      )
    })
    
    # Render code preview
    mod_code_server(
      "code",
      code_parameters = code_parameters,
      method = "bf_predetermined")
  })
}
    
## To be copied in the UI
# mod_ssp_bf_predetermined_ui("bf_predetermined")
    
## To be copied in the server
# mod_ssp_bf_predetermined_server("bf_predetermined")
 
