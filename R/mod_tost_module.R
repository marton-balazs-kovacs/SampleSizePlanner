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
  
  ns <- NS(id)
  tagList(
    fixedPanel(
      uiOutput(ns("open")),
      bottom = "30%", right = "50%", width = "auto"
    )
  )
}
    
# Module Server
    
#' @rdname mod_tost_module
#' @export
#' @keywords internal
    
mod_tost_module_server <- function(input, output, session, input_data, menu_call){
  
  modal <- function() {
    ns <- session$ns
    
    modalDialog(
      easyClose = TRUE,
      footer = modalButton("Close Modal"),
      h1("Let's have a TOAST!"),
      sliderInput(ns("opt"), "Power", min = 0, max = 1, value = 0.8, step = 0.1),
      sliderInput(ns("band"), "Band", min = 0, max = 1, value = 0.2, step = 0.1),
      sliderInput(ns("delta"), "Delta", min = 0, max = 1, value = 0, step = 0.1),
      actionButton(ns("calculate"), "Ready, set, go!"),
      textOutput(ns("tost_output_n1")),
      textOutput(ns("tost_output_npower")),
      textOutput(ns("error"))
    )
  }
  
  observeEvent(input$open_modal, {
    showModal(modal())},
    ignoreInit = TRUE)
  
  
  output$open <- renderUI({
    if(input_data() == "TOST") {
      shinyWidgets::actionBttn(
        inputId = session$ns("open_modal"),
        label = "Show me more!",
        style = "minimal",
        color = "danger")
    }
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
  
  output$error <- renderText({
    menu_call()
  })
  
}
    
## To be copied in the UI
# mod_tost_module_ui("tost_module_ui_1")
    
## To be copied in the server
# callModule(mod_tost_module_server, "tost_module_ui_1")
 
