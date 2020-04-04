# Module UI
  
#' @title   mod_menu_module_ui and mod_menu_module_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_menu_module
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_menu_module_ui <- function(id){
  
  ns <- NS(id)
  
  tagList(
    fixedPanel(
      shinyWidgets::actionBttn(
        inputId = ns("open_menu"),
        label = "Open menu", 
        style = "minimal",
        color = "danger"),
      bottom = "5%", right = "2%", width = "auto"
    )
  )
}
    
# Module Server
    
#' @rdname mod_menu_module
#' @export
#' @keywords internal
    
mod_menu_module_server <- function(input, output, session){
  
  method <- reactiveVal(value = NULL)
  
  modal <- function() {
    ns <- session$ns
    
    modalDialog(
      easyClose = TRUE,
      footer = modalButton("Close Modal"),
      h1("Sample size estimation methods"),
      selectInput(ns("method"), "Choose a method:",
                  choices = list(`Testing` = list("TOST", "ROPE"),
                                 `Estimation` = list()),
                  selected = NULL),
      actionButton(ns("go"), "Go!")
    )
  }
  
  observeEvent(input$open_menu, {
    showModal(modal())
  })
  
  observeEvent(input$go, {
    
    method(input$method)
    })
  
  return(list(
    method = reactive({method()})
  ))
}


    
## To be copied in the UI
# mod_menu_module_ui("menu_module_ui_1")
    
## To be copied in the server
# callModule(mod_menu_module_server, "menu_module_ui_1")
 
