#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_home_ui <- function(id){
  tagList(
    div(
      id = "home-page",
      style="text-align: justify;",
      h2("How to use the app", style = "margin-bottom: 20px; margin-top: 0; padding-top: 0;"),
      HTML(
      "<p>This application provides 9 different methods to determine the sample size
      for a two group design. Moreover, the app provides help to create an accompanying
      report text for your sample size determination. The figure below shows the 9 sample size
      estimation methods in the app and their relationships. To get started with the sample size estimation
      choose a method under the <b>Methods tab</b>. Please visit the <b>About tab</b> if you would like to know more
      about the project or if you have any suggestions or issues with the app.</p>"),
      img(src = "www/flowchart_web.jpg"
          , width = "100%", style = "margin-bottom: 20px; margin-top: 20px;"
          ),
      HTML("<p style='text-align: right;'>*This version of the application is presented in a tutorial paper that is accepted for publication.</br>
           Kovacs, van Ravenzwaaij, Hoekstra & Aczel (2022) <a href='https://doi.org/10.1177/25152459211054059'>link to publication</a></p>")
    )
  )
}
    
## To be copied in the UI
# mod_home_ui("home")
 
