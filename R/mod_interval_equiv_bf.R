# Module UI
  
#' @title   mod_interval_equiv_bf_ui and mod_interval_equiv_bf_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_interval_equiv_bf
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_interval_equiv_bf_ui <- function(id){
  
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_interval_equiv_bf
#' @export
#' @keywords internal
    
mod_interval_equiv_bf_server <- function(id, activate_menu, method_menu, activate_question, method_question){
  
  moduleServer(id, function(input, output, session) {
    
    modal <- function() {
      
      modalDialog(
        footer = actionButton(NS(id, "close_modal"), label = "Close"),
        h1("Interval Equiv BF"),
        h2("Effect size"),
        sliderInput(NS(id, "delta"), "Delta", min = 0, max = 1, value = 0, step = 0.1),
        h2("Threshold"),
        selectInput(NS(id, "tresh"), "Threshold", choices = c(3, 6, 10),
                    selected = 10),
        h2("Prior"),
        selectInput(NS(id, "distribution"), "Choose a distribution:",
                    choices = c("cdf_t", "cdf_normal"),
                    selected = "cdf_t"),
        selectInput(NS(id, "scale"), "Choose a scale:",
                    choices = "1/sqrt(2)",
                    selected = "1/sqrt(2)"),
        numericInput(NS(id, "location"), "Choose a location:", value = 0),
        numericInput(NS(id, "df"), "Choose a df:", value = 1),
        actionButton(NS(id, "calculate"), "Ready, set, go!"),
        textOutput(NS(id, "output$eq_bf_output"))
      )
    }
    
    observeEvent(activate_menu(), {
      
      if(method_menu() == "Interval Equiv BF") {
        showModal(modal())}
      
    })
    
    observeEvent(activate_question(), {
      
      if(method_question() == "Interval Equiv BF") {
        showModal(modal())}
      
    })
    
    observeEvent(input$close_modal, {
      
      removeModal()
      
    })
    
    eq_bf_result <- eventReactive(input$calculate, {
      "This function still needs work to run. It is here for UI check."
    })
    
    output$eq_bf_output <- renderText({
      eq_bf_result()
    })
    
  })
}
    
## To be copied in the UI
# mod_interval_equiv_bf_ui("interval_equiv_bf_ui_1")
    
## To be copied in the server
# mod_interval_equiv_bf_server("interval_equiv_bf_ui_1")
 
