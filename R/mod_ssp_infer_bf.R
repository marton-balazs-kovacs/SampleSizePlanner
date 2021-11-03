#' ssp_infer_bf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ssp_infer_bf_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' ssp_infer_bf Server Functions
#'
#' @noRd 
mod_ssp_infer_bf_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_ssp_infer_bf_ui("ssp_infer_bf_ui_1")
    
## To be copied in the server
# mod_ssp_infer_bf_server("ssp_infer_bf_ui_1")
