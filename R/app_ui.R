#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    navbarPage(
      title = "Sample Size Planner",
        tabPanel("Flowchart",
                 tags$img(src = "www/flowchart.png", width = "80%", heigth = "80%", align = "left")),
        navbarMenu("Methods",
                   tabPanel("TOST",
                            mod_ssp_tost_ui("tost")),
                   tabPanel("Traditional power",
                            mod_ssp_power_traditional_ui("traditional")),
                   tabPanel("Power curve",
                            mod_ssp_power_curve_ui("curve")),
                   tabPanel("ROPE",
                            mod_ssp_rope_ui("rope")),
                   tabPanel("BF threshold",
                            mod_ssp_bf_thresh_ui("bf_thresh")),
                   tabPanel("APP",
                            mod_ssp_app_ui("app")),
                   tabPanel("Interval Equiv BF",
                            mod_ssp_eq_bf_ui("eq_bf")),
                   tabPanel("AIPE",
                            mod_ssp_aipe_ui("aipe")),
                   tabPanel("BFDA",
                            mod_ssp_bfda_ui("bfda"))
        ),
        tabPanel("Tutorial",
                 mod_article_ui("article")),
        tabPanel("About",
                 mod_about_ui("about_ui_1"))
        ),

    # Enabling waiter JS functions
    waiter::use_waiter(include_js = FALSE),
    waiter::use_waitress()
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'SampleSizePlanner')
  )
 
  tags$head(
    golem::activate_js(),
    rclipboard::rclipboardSetup(),
    # golem::favicon(),
    shinyjs::useShinyjs(),
    tags$script(src = "www/disable_btn.js")
    # Add custom css stylesheet
    # tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css")
  )
}
