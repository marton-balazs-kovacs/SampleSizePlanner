#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  callModule(mod_article_module_server, "article_module_ui_1")
  mod_menu_output <- callModule(mod_menu_module_server, "menu_module_ui_1")
  mod_question_output <- callModule(mod_question_module_server, "question_module_ui_1")
  callModule(mod_tost_module_server, "tost_module_ui_1", input_data = mod_question_output$question, menu_call = mod_menu_output$method)
}
