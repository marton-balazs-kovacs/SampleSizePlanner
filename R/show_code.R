show_code <- function(method, code_parameters) {
  # Setup code
  code_text <- 
    glue::glue(
      "# Download package
      install.packages(\"SampleSizePlanner\")
      # Load package
      library(SampleSizePlanner)
      # Run calculation")
  
  # Interactive function call by method
  if (method == "aipe") {
    function_call <-
      glue::glue(
        "ssp_aipe(delta = {delta}, confidence_level = {confidence_level}, width = {width}, which_width = \"Full\", certainty = NULL)",
        width = code_parameters$width,
        delta = code_parameters$delta,
        confidence_level = code_parameters$confidence_level)
    
  } else if (method == "app") {
    function_call <-
      glue::glue(
        "ssp_app(confidence = {confidence}, closeness = {closeness})",
        confidence = code_parameters$confidence,
        closeness = code_parameters$closeness)
    
  } else if (method == "tost") {
    function_call <-
      glue::glue(
        "ssp_tost(trp = {tpr}, eq_band = {eq_band}, delta = {delta}, alpha = .05)",
        eq_band = code_parameters$eq_band,
        delta = code_parameters$delta,
        tpr = code_parameters$tpr)
    
  } else if (method == "eq_bf") {
    function_call <-
      glue::glue("ssp_eq_bf(trp = {tpr}, eq_band = {eq_band}, delta = {delta}, thresh = {thresh}, prior_scale = {prior_scale})",
                 eq_band = code_parameters$eq_band,
                 thresh = code_parameters$thresh, 
                 delta = code_parameters$delta,
                 tpr = code_parameters$tpr,
                 prior_scale = code_parameters$prior_scale)
    
  } else if (method == "traditional") {
    function_call <-
      glue::glue(
        "ssp_power_traditional(max_n = {max_n}, delta = {delta}, tpr = {tpr}, alpha = 0.05)",
        delta = code_parameters$delta,
        tpr = code_parameters$tpr,
        max_n = code_parameters$max_n)
    
  } else if (method == "traditional-anova") {
    function_call <-
      glue::glue(
        "ssp_power_traditional_anova(effect = {effect}, iter = {iter}, max_n = {max_n}, mu = {mu}, sigma = {sigma}, tpr = {tpr}, alpha = 0.05)",
        effect = code_parameters$effect,
        iter = code_parameters$iter,
        mu = code_parameters$mu,
        sigma = code_parameters$sigma,
        tpr = code_parameters$tpr,
        max_n = code_parameters$max_n)
  
  } else if (method == "bf_predetermined") {
    function_call <-
      glue::glue(
        "ssp_bf_predetermined(trp = {tpr}, delta = {delta}, thresh = {thresh}, max_n = {max_n}, prior_scale = {prior_scale})",
        thresh = code_parameters$thresh, 
        delta = code_parameters$delta,
        tpr = code_parameters$tpr,
        max_n = code_parameters$max_n,
        prior_scale = code_parameters$prior_scale)
    
  } else if (method == "rope") {
    function_call <-
      glue::glue(
        "ssp_rope(trp = {tpr}, eq_band = {eq_band}, delta = {delta}, alpha = .05, tol = 1e-4, granularity = 300, prior_scale = {prior_scale})",
        eq_band = code_parameters$eq_band,
        delta = code_parameters$delta,
        tpr = code_parameters$tpr,
        prior_scale = code_parameters$prior_scale)
    
  } else if (method == "power_curve") {
    function_call <-
      glue::glue(
        "# Determine the sample sizes for each delta //
        curve_data <- ssp_power_curve(tpr = {tpr}, delta = seq({delta_min}, {delta_max}, 0.01), max_n = {max_n}) //
        # Plot the power curve //
        SampleSizePlanner::plot_power_curve(delta = curve_data$delta, n1 = curve_data$n1, animated = FALSE)",
        delta_min = code_parameters$delta_min,
        delta_max = code_parameters$delta_max,
        tpr = code_parameters$tpr,
        max_n = code_parameters$max_n)
    
  } else if (method == "bfda") {
    function_call <-
      glue::glue(
        "SampleSizePlanner::ssp_bfda(tpr = {tpr}, delta = {delta}, thresh = {thresh}, n_rep = 10000, prior_scale = {prior_scale})",
        thresh = code_parameters$thresh, 
        delta = code_parameters$delta,
        tpr = code_parameters$tpr,
        prior_scale = code_parameters$prior_scale)
  }
  
  # Return output
  glue::glue(
    code_text,
    function_call,
    .sep = "\n")
}