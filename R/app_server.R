#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  callModule(mod_article_module_server, "article_module_ui_1")
  callModule(mod_question_module_server, "question_module_ui_1")
}
