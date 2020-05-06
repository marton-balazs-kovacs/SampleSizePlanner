#' @import shiny
app_server <- function(input, output,session) {
  mod_article_module_server("article_module_ui_1")
  mod_about_server("about_ui_1")
  menu_out <- mod_menu_module_server("menu_module_ui_1")
  question_out <- mod_question_module_server("question_module_ui_1")
  mod_tost_module_server("tost_module_ui_1",
                         activate_menu = menu_out$activate,
                         method_menu = menu_out$method,
                         activate_question = question_out$activate,
                         method_question = question_out$method)
  mod_traditional_power_server("traditional_power_ui_1",
                               activate_menu = menu_out$activate,
                               method_menu = menu_out$method,
                               activate_question = question_out$activate,
                               method_question = question_out$method)
  mod_power_curve_server("power_curve_ui_1",
                         activate_menu = menu_out$activate,
                         method_menu = menu_out$method,
                         activate_question = question_out$activate,
                         method_question = question_out$method)
  mod_apriori_precision_server("apriori_precision_ui_1",
                               activate_menu = menu_out$activate,
                               method_menu = menu_out$method,
                               activate_question = question_out$activate,
                               method_question = question_out$method)
  mod_rope_server("rope_ui_1",
                  activate_menu = menu_out$activate,
                  method_menu = menu_out$method,
                  activate_question = question_out$activate,
                  method_question = question_out$method)
  mod_interval_equiv_bf_server("interval_equiv_bf_ui_1",
                               activate_menu = menu_out$activate,
                               method_menu = menu_out$method,
                               activate_question = question_out$activate,
                               method_question = question_out$method)
  mod_bf_threshold_server("bf_threshold_ui_1",
                          activate_menu = menu_out$activate,
                          method_menu = menu_out$method,
                          activate_question = question_out$activate,
                          method_question = question_out$method)
}
