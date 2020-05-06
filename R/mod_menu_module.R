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
  
  tagList(
    fixedPanel(
      shinyWidgets::actionBttn(
        inputId = NS(id, "open_menu"),
        label = "Open menu", 
        style = "minimal",
        color = "danger"),
      bottom = "8%", left = "4%", width = "auto"
    )
  )
}
    
# Module Server
    
#' @rdname mod_menu_module
#' @export
#' @keywords internal
    
mod_menu_module_server <- function(id){
  
  moduleServer(id, function(input, output, session) {
  
  modal <- function() {
    
    modalDialog(
      easyClose = TRUE,
      footer = modalButton("Close Modal"),
      h1("Sample size estimation methods"),
      selectInput(NS(id, "method"), "Choose a method:",
                  choices = list(`Testing` = list("TOST", "Traditional power", "Power curve", "ROPE", "Interval Equiv BF", "Bf treshold"),
                                 `Estimation` = list("APP")),
                  selected = NULL),
      actionButton(NS(id, "go"), "Go!")
    )
  }
  
  observeEvent(input$open_menu, {
    showModal(modal())
  })
  
  list(
    method = reactive(input$method),
    activate = reactive(input$go)
  )
  })
}


    
## To be copied in the UI
# mod_menu_module_ui("menu_module_ui_1")
    
## To be copied in the server
# mod_menu_module_server("menu_module_ui_1")
 
