# Module UI
  
#' @title   mod_tost_module_ui and mod_tost_module_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_tost_module
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_tost_module_ui <- function(id){

  tagList(
  )
}
    
# Module Server
    
#' @rdname mod_tost_module
#' @export
#' @keywords internal
    
mod_tost_module_server <- function(id, activate_menu, method_menu, activate_question, method_question){
  
  moduleServer(id, function(input, output, session) {
  modal <- function() {
    
    modalDialog(
      footer = actionButton(NS(id, "close_modal"), label = "Close modal"),
      h1("Let's have a TOAST!"),
      sliderInput(NS(id, "opt"), "Power", min = 0, max = 1, value = 0.8, step = 0.1),
      sliderInput(NS(id, "band"), "Band", min = 0, max = 1, value = 0.2, step = 0.1),
      sliderInput(NS(id, "delta"), "Delta", min = 0, max = 1, value = 0, step = 0.1),
      actionButton(NS(id, "calculate"), "Ready, set, go!"),
      textOutput(NS(id, "tost_output_n1")),
      textOutput(NS(id, "tost_output_npower"))
    )
  }
  
  observeEvent(activate_menu(), {
    
    if(method_menu() == "TOST") {
    showModal(modal())}
    
    })
  
  observeEvent(activate_question(), {
    
    if(method_question() == "TOST") {
      showModal(modal())}
    
  })
  
  observeEvent(input$close_modal, {

    removeModal()

  })
  
  tost_result <- eventReactive(input$calculate, {
    TOSTss(Opt = input$opt,
           Band = input$band,
           delta = input$delta)
  })
  
  output$tost_output_n1 <- renderText({
    paste("The estimated sample size is:", tost_result()$n1)
  })

  output$tost_output_npower <- renderText({
    paste("The resulting power is:", tost_result()$npower)
  })
  })
}
    
## To be copied in the UI
# mod_tost_module_ui("tost_module_ui_1")
    
## To be copied in the server
# mod_tost_module_server("tost_module_ui_1")
 
