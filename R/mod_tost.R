# Module UI
  
#' @title   mod_tost_ui and mod_tost_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_tost
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_tost_ui <- function(id){
  tagList(
    sidebarPanel(
      h1("TOST"),
      sliderInput(NS(id, "opt"), "Power", min = 0, max = 1, value = 0.8, step = 0.1),
      sliderInput(NS(id, "band"), "Band", min = 0, max = 1, value = 0.2, step = 0.1),
      sliderInput(NS(id, "delta"), "Delta", min = 0, max = 1, value = 0, step = 0.1),
      actionButton(NS(id, "calculate"), "Calculate sample size!")
      ),
    mainPanel(
      htmlOutput(NS(id, "tost_output")),
      downloadButton(NS(id, "report"))
      )
  )
}
    
# Module Server
    
#' @rdname mod_tost
#' @export
#' @keywords internal
    
mod_tost_server <- function(id){

  moduleServer(id, function(input, output, session) {
    
    waitress <- waiter::Waitress$new("#tost_ui_1-tost_output", theme = "overlay", infinite = TRUE, hide_on_render = TRUE)
    
    tost_result <- eventReactive(input$calculate, {
      waitress$start()
      TOSTss(Opt = input$opt,
             Band = input$band,
             delta = input$delta)
      })
    
    params <- eventReactive(input$calculate, {
      list(
        n1 = tost_result()$n1,
        n2 = tost_result()$n2,
        output_power = tost_result()$npower,
        input_power = input$opt,
        band = input$band,
        delta = input$delta
        )
      })
    
    output$tost_output <- renderUI({
      if (input$calculate) {
        report_path <- file.path("inst/app/www/", "tost_output.Rmd")
        file <- paste0("tost_preview", ".html")
        callr::r(
          render_report,
          list(input = report_path, output = file, format = "html_document", params = params())
          )
        return(tags$iframe(style="height:400px; width:100%", src = "www/tost_preview.html", seamless = "seamless"))
        } else {
          return(tags$iframe(style="height:400px; width:100%", src = "www/on_load_placeholder.html", seamless = "seamless"))
          }
      })
    
    output$report <- downloadHandler(
      filename = function() {
        paste0("tost_output", Sys.Date(), ".doc")
        },
      content = function(file) {
        report_path <- file.path("inst/app/www/", "tost_output.Rmd")
        file.copy("tost_output.Rmd", report_path, overwrite = TRUE)
        callr::r(
          render_report,
          list(input = report_path, output = file, format = "word_document", params = params())
        )
        }
      )
    })
}
    
## To be copied in the UI
# mod_tost_ui("tost_ui_1")
    
## To be copied in the server
# mod_tost_server("tost_ui_1")
 
