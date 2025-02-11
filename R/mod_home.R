#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_home_ui <- function(id) {
  tagList(
    div(
      id = "home-page",
      style = "text-align: justify;",
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
      Please visit the <b>About</b> tab if you would like to know more about the project or if you have any suggestions or issues with the app.</p>"
      ),
      plotly::plotlyOutput(NS(id, "flowchart_plot"), height = "600px")
    )
  )
}

mod_home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$flowchart_plot <- plotly::renderPlotly({
      # Read the image
      img_path <- app_sys("app/www/flowchart_web.jpg")
      img <- magick::image_read(img_path)
      img_data <- magick::image_data(img, channels = "rgb")
      
      # Get image dimensions
      img_info <-  magick::image_info(img)
      img_width <- img_info$width
      img_height <- img_info$height
      aspect_ratio <- img_height / img_width
      
      # Convert image to base64
      img_base64 <- base64enc::base64encode(img_path)
      img_uri <- paste0("data:image/jpeg;base64,", img_base64)
      
      # Display the image using plotly
      plotly::plot_ly(
        x = c(0, img_width),  # Set the correct width
        y = c(0, img_height), # Set the correct height
        type = 'scatter',
        mode = 'none'
      ) %>%
        plotly::layout(
          images = list(
            list(
              source = img_uri,
              xref = "x",
              yref = "y",
              x = 0,
              y = img_height,
              sizex = img_width,
              sizey = img_height,
              sizing = "stretch",
              opacity = 1,
              layer = "below"
            )
          ),
          xaxis = list(visible = FALSE, range = c(0, img_width)),
          yaxis = list(visible = FALSE, range = c(0, img_height), scaleanchor = "x"),  # Maintain aspect ratio
          dragmode = "pan",
          margin = list(l = 0, r = 0, t = 0, b = 0)
        )
    })
  })
}


## To be copied in the UI
# mod_home_ui("home")
 
