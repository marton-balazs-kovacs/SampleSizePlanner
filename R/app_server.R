#' @import shiny
app_server <- function(input, output,session) {
  
  # Methods modules
  mod_ssp_tost_server("tost")
  mod_ssp_power_traditional_server("traditional")
  mod_ssp_power_curve_server("curve")
  mod_ssp_rope_server("rope")
  mod_ssp_bf_predetermined_server("bf_predetermined")
  mod_ssp_app_server("app")
  mod_ssp_eq_bf_server("eq_bf")
  mod_ssp_aipe_server("aipe")
  mod_ssp_bfda_server("bfda")
  
  # Show the paper module
  mod_article_server("article")
}
