# Module UI
  
#' @title   mod_question_ui and mod_question_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_question
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_question_ui <- function(id){

  tagList(
    fixedPanel(
      textOutput(NS(id, "question")),
      bottom = "50%", left = "40%", width = "auto"
    ),
    fixedPanel(
      uiOutput(NS(id, "left_button")),
      bottom = "30%", left = "10%", width = "auto"
    ),
    fixedPanel(
      uiOutput(NS(id, "right_button")),
      bottom = "30%", right = "10%", width = "auto"
    ),
    fixedPanel(
      shinyWidgets::actionBttn(
        inputId = NS(id, "over"),
        label = "Start over",
        style = "minimal",
        color = "danger"),
      top = "8%", right = "4%", width = "auto"
    ),
    fixedPanel(
      uiOutput(NS(id, "open")),
      bottom = "30%", right = "45%", left = "45%", width = "auto"
    )
  )
}
    
# Module Server
    
#' @rdname mod_question
#' @export
#' @keywords internal
    
mod_question_server <- function(id){
  
  moduleServer(id, function(input, output, session) {
  
  current <- reactiveVal(value = dplyr::filter(question_data, id == 1))
  
  observeEvent(input$over, {
    over <- 
      dplyr::filter(question_data, id == 1)
    
    current(over)
  })
  
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
  
  output$open <- renderUI({
    if(current()$is_method == 1L) {
      shinyWidgets::actionBttn(
        inputId = session$ns("open_modal"),
        label = "Show me more!",
        style = "minimal",
        color = "danger")
      }
  })
  
  output$question <- renderText({
    current()$question
  })
  
  list(
    method = reactive(current()$question),
    activate = reactive(input$open_modal)
    )
  })
  }
    
## To be copied in the UI
# mod_question_ui("question_ui_1")
    
## To be copied in the server
# mod_question_server("question_ui_1")