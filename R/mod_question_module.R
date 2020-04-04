# Module UI
  
#' @title   mod_question_module_ui and mod_question_module_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_question_module
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_question_module_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    fixedPanel(
      textOutput(ns("question")),
      bottom = "50%", left = "40%", width = "auto"
    ),
    fixedPanel(
      uiOutput(ns("left_button")),
      bottom = "30%", left = "10%", width = "auto"
    ),
    fixedPanel(
      uiOutput(ns("right_button")),
      bottom = "30%", right = "10%", width = "auto"
    )
  )
}
    
# Module Server
    
#' @rdname mod_question_module
#' @export
#' @keywords internal
    
mod_question_module_server <- function(input, output, session){
  
  ns <- session$ns
  
  current <- reactiveVal(value = dplyr::filter(question_data, id == 1))
  
  label_left <- reactive({current()$label_left})
  
  label_right <- reactive({current()$label_right})
    
  observeEvent(input$right, {
    right_option <- 
      dplyr::filter(question_data, id == current()$id_right)
    
    current(right_option)
  })
  
  observeEvent(input$left, {
    left_option <- 
      dplyr::filter(question_data, id == current()$id_left)
    
    current(left_option)
  })
  
  output$left_button <- renderUI({
    if(!is.na(current()$id_left)) {
      shinyWidgets::actionBttn(
        inputId = session$ns("left"),
        label = label_left(),
        style = "minimal",
        color = "danger")
    }
  })
  
  output$right_button <- renderUI({
    if(!is.na(current()$id_right)) {
      shinyWidgets::actionBttn(
        inputId = session$ns("right"),
        label = label_right(),
        style = "minimal",
        color = "danger")
    }
  })
  
  output$question <- renderText({
    current()$question
  })
  
  return(list(
    question = reactive({current()$question})
  ))

  }
    
## To be copied in the UI
# mod_question_module_ui("question_module_ui_1")
    
## To be copied in the server
# callModule(mod_question_module_server, "question_module_ui_1")