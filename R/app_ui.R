#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Using the package: shinyjs
    shinyjs::useShinyjs(),
    # List the first level UI elements here 
    fluidPage(
      h1("SampleSizePlanner"),
      mod_article_module_ui("article_module_ui_1"),
      mod_menu_module_ui("menu_module_ui_1"),
      mod_question_module_ui("question_module_ui_1"),
      mod_tost_module_ui("tost_module_ui_1")
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'SampleSizePlanner')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
