#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  callModule(mod_question_module_server, "question_module_ui_1", data = flowchart_data)
}
