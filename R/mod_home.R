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
      "<p>This application provides multiple Bayesian and frequentist methods to determine the sample size
      for two group and four group independent study designs. Moreover, the app provides help to create an accompanying
      text report for your sample size determination. The figure below shows the different sample size
      estimation methods available in the app and their relationships.</br>
      </br>
      To get started with the sample size estimation:
      <ul>
        <li>Find the method to use based on your research question, study design, and preferences based on the flowchart below.</li>
        <li>Open the page of the choosen method under either the <b>T-test</b> or the <b>ANOVA</b> tabs in the right corner</li>
        <li>Provide your study parameters in the <b>Determine your sample size</b> box to calculate the sample size</li>
        <li>Add your justification to create the report text including the determined sample size</li>
        <li>Download or copy the justification report to include it in your manuscript</li>
      </ul>
      </br>
      Please visit the <b>About</b> tab if you would like to know more about the project or if you have any suggestions or issues with the app.</p>"),
      img(src = "www/flowchart_web.jpg"
          , width = "100%", style = "margin-bottom: 20px; margin-top: 20px;"
          ),
      HTML("<p style='text-align: right;'>*The sample size calculations for the t-tests are presented in a tutorial paper:</br>
           Kovacs, van Ravenzwaaij, Hoekstra & Aczel (2022) <a href='https://doi.org/10.1177/25152459211054059'>link to publication</a></p>")
    )
  )
}
    
## To be copied in the UI
# mod_home_ui("home")
 
