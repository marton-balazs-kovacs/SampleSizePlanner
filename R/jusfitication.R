justification <- function(method, output_parameters) {
  
  if (method == "aipe") {
    text <- 
      glue::glue(
      "In order to estimate the sample size, we used the accuracy in parameter estimation \\
      (AIPE; Kelley and Rausch, 2006) method. We aimed for a {confidence_level}% confidence level, \\
      because {confidence_level_justification}. The desired width was {width} because \\
      {width_justification}. We expected an underlying population effect size of {delta}, because {delta_justification}. \\
      Based on these parameters, a minimal sample size of {n1} per group was estimated for our design.",
      width = output_parameters$width,
      delta = output_parameters$delta,
      n1 = output_parameters$n1,
      confidence_level = output_parameters$confidence_level,
      confidence_level_justification = output_parameters$confidence_level_justification,
      width_justification = output_parameters$width_justification,
      delta_justification = output_parameters$delta_justification
      )
  } else if (method == "app") {
    text <- 
      glue::glue(
      "In order to estimate the sample size, we used the a-priori precision \\
      (APP; Trafimow and MacDonald, 2017) method. A-priori to data collection, \\
      we wanted to be {confidence}% confident that both sample means lie within {closeness} SD of \\
      the true population means. Based on these parameters, the resulting minimum \\
      sample size was {n1} per group for our design.",
      confidence = output_parameters$confidence,
      closeness = output_parameters$closeness,
      n1 = output_parameters$n1
    )
  } else if (method == "tost") {
    text <-
      glue::glue(
        "In order to calculate an appropriate sample size for testing whether the \\
        two groups are practically equivalent, we used the Two One-Sided Tests of \\
        Equivalence (TOST; Schuirmann, 1981) method. We used an alpha of 0.05. We set the aimed TPR to be {tpr}, \\
        because {tpr_justification}. We consider all effect sizes below {eq_band} \\
        equivalent to zero, because {eq_band_justification}. The expected delta was {delta} \\
        because {delta_justification}. Based on these parameters, a sample size of {n1} \\
        per group was estimated in order to reach a TPR of {round(tpr_out, 1)} with our design.",
        eq_band = output_parameters$eq_band,
        delta = output_parameters$delta,
        n1 = output_parameters$n1,
        tpr_out = output_parameters$tpr_out,
        tpr = output_parameters$tpr,
        tpr_justification = output_parameters$tpr_justification,
        eq_band_justification = output_parameters$eq_band_justification,
        delta_justification = output_parameters$delta_justification
      )
  } else if (method == "eq_bf") {
    text <-
      glue::glue(
        "In order to estimate the sample size, we used the interval equivalent Bayes factor \\
        (Morey & Rouder, 2011; van Ravenzwaaij et al., 2019) method. We used a Cauchy prior \\
        distribution centered on 0 with a scale parameter of {prior_scale}. We set the aimed TPR at \\
        {tpr}, because {tpr_justification}. We consider all effect sizes below {eq_band} equivalent to zero, \\
        because {eq_band_justification}. The expected delta was {delta} because {delta_justification}. \\
        Our Bayes factor threshold for concluding equivalence was {thresh}. Based on these parameters, a minimal \\
        sample size of {n1} was estimated in order to reach {round(tpr_out, 1)} TPR for our design.",
        eq_band = output_parameters$eq_band,
        thresh = output_parameters$thresh, 
        delta = output_parameters$delta,
        n1 = output_parameters$n1,
        tpr_out = output_parameters$tpr_out,
        tpr = output_parameters$tpr,
        prior_scale = output_parameters$prior_scale,
        tpr_justification = output_parameters$tpr_justification,
        eq_band_justification = output_parameters$eq_band_justification,
        delta_justification = output_parameters$delta_justification
      )
  } else if (method == "infer_bf") {
    text <-
      glue::glue(
        "In order to estimate the sample size, we used the non-inferiority Bayes factor \\
        (van Ravenzwaaij et al., 2019) method. We used a Cauchy prior \\
        distribution centered on 0 with a scale parameter of {prior_scale}. We set the aimed TPR at \\
        {tpr}, because {tpr_justification}. We chose a non-inferiority margin of {ni_margin} because {ni_margin_justification}. \\
        The expected delta was {delta} because {delta_justification}. \\
        Our Bayes factor threshold for concluding non-inferiority was {thresh}. Based on these parameters, a minimal \\
        sample size of {n1} was estimated in order to reach {round(tpr_out, 1)} TPR for our design.",
        ni_margin = output_parameters$ni_margin,
        thresh = output_parameters$thresh, 
        delta = output_parameters$delta,
        n1 = output_parameters$n1,
        tpr_out = output_parameters$tpr_out,
        tpr = output_parameters$tpr,
        prior_scale = output_parameters$prior_scale,
        tpr_justification = output_parameters$tpr_justification,
        ni_margin_justification = output_parameters$ni_margin_justification,
        delta_justification = output_parameters$delta_justification
      )
  } else if (method == "traditional") {
    text <- 
      glue::glue(
        "We used a power analysis to estimate the sample size. We used an alpha of 0.05. We set the aimed power at {tpr}, because \\
        {tpr_justification}. The expected delta was {delta} {delta_justification}. Based on these parameters, \\
        a minimal sample size of {n1} was estimated in order to reach {round(tpr_out, 1)} power for our design.",
        n1 = output_parameters$n1,
        tpr_out = output_parameters$tpr_out,
        tpr = output_parameters$tpr,
        tpr_justification = output_parameters$tpr_justification,
        delta = output_parameters$delta,
        delta_justification = output_parameters$delta_justification
      )
  } else if (method == "traditional-twoway-anova") {
    text <- 
      glue::glue(
        "We conducted a power analysis with an alpha of {alpha} to estimate the required sample size. We set the target power at {tpr}, because {tpr_justification}. \\
        The expected group means were {mu} for subgroups 1|1, 1|2, 2|1, and 2|2, of factors A|B respectively, and we assumed a common standard deviation of {sigma}. \\
        Based on these parameters, a minimum per-group sample size of {n1} was required to achieve the target power {tpr}. The effective power was {round(tpr_out, 2)} \\
        for the {effect}."
        ,
        n1 = output_parameters$n1,
        tpr_out = output_parameters$tpr_out,
        tpr = output_parameters$tpr,
        tpr_justification = output_parameters$tpr_justification,
        mu = paste(output_parameters$mu, collapse = ", "),
        sigma = output_parameters$sigma,
        effect = output_parameters$effect,
        alpha = output_parameters$alpha
      )
    
  } else if (method == "bayesian-twoway-anova") {
    text <- 
      glue::glue(
        "We conducted a Bayesian Analysis of Variance (Rouder et al., 2012) to estimate the required sample size, using a Bayes factor \\
        threshold of {thresh} and a Cauchy prior distribution centered at zero with scale parameter {prior_scale}. We set the target true \\
        positive rate at {tpr}, because {tpr_justification}. Expected group means were {mu} for subgroups 1|1, 1|2, 2|1, and 2|2, of factors A|B respectively, \\
        and we assumed a common standard deviation of {sigma}. Based on these parameters, a minimum per-group sample size of {n1} was required \\
        to achieve the target true positive rate {tpr}. The effective true positive rate was {round(tpr_out,2)} for the {effect}.",
        thresh = output_parameters$thresh,
        n1 = output_parameters$n1,
        tpr_out = output_parameters$tpr_out,
        tpr = output_parameters$tpr,
        tpr_justification = output_parameters$tpr_justification,
        prior_scale = output_parameters$prior_scale,
        mu = paste(output_parameters$mu, collapse = ", "),
        sigma = output_parameters$sigma,
        effect = output_parameters$effect
      )

  } else if (method == "eq-twoway-anova") {
    text <- 
      glue::glue(
        "We conducted an Interval Equivalent Bayes factor analysis (Morey & Rouder, 2011; Rouder et al., 2012; van Ravenzwaaij et al., 2019) to \\
        estimate the required sample size, using a Bayes factor threshold of {thresh}. We considered all effect sizes within {eq_band} equivalent to zero, \\
        because {eq_band_justification}. A Cauchy prior distribution centered at zero with scale parameter {prior_scale} was specified, and we set the \\
        target true positive rate at {tpr}, because {tpr_justification}. Expected group means were {mu} which for subgroups 1|1, 1|2, 2|1, and 2|2 of \\
        factors A|B respectively, with a common standard deviation of {sigma}. Based on these parameters, a minimum per-group sample size of {n1} was required \\
        to achieve the target true positive rate {tpr}. The effective true positive rate was {round(tpr_out, 2)} for the {effect}."
        ,
        thresh = output_parameters$thresh,
        eq_band = paste0(-output_parameters$eq_band," to ", output_parameters$eq_band),
        n1 = output_parameters$n1,
        tpr_out = output_parameters$tpr_out,
        tpr = output_parameters$tpr,
        tpr_justification = output_parameters$tpr_justification,
        eq_band_justification = output_parameters$eq_band_justification,
        prior_scale = output_parameters$prior_scale,
        mu = paste(output_parameters$mu, collapse = ", "),
        sigma = output_parameters$sigma,
        effect = output_parameters$effect
      )
    
  } else if (method == "rope-twoway-anova") {
    text <- 
      glue::glue(
        "We conducted a Region of Practical Equivalence (Kruschke, 2018; Kruschke & Liddell, 2018) analysis to estimate the required sample size. \\
        We considered all effect sizes within {eq_band} equivalent to zero, because {eq_band_justification}. We used a highest density interval of {ci} and \\
        specified a Cauchy prior distribution centered at zero  with scale parameter {prior_scale}. The target true positive rate was set at {tpr}, \\
        because {tpr_justification}. Expected group means were {mu} for subgroups 1|1, 1|2, 2|1, and 2|2 of factors A|B, respectively, with a common \\
        standard deviation of {sigma}. Based on these parameters, a minimum per-group sample size of {n1} was required to achieve the target true positive \\
        rate {tpr}. The effective true positive rate was {round(tpr_out, 2)} for the {effect}.",
        eq_band = paste0(-output_parameters$eq_band," to ", output_parameters$eq_band),
        ci = output_parameters$ci,
        n1 = output_parameters$n1,
        tpr_out = output_parameters$tpr_out,
        tpr = output_parameters$tpr,
        tpr_justification = output_parameters$tpr_justification,
        eq_band_justification = output_parameters$eq_band_justification,
        prior_scale = output_parameters$prior_scale,
        mu = paste(output_parameters$mu, collapse = ", "),
        sigma = output_parameters$sigma,
        effect = output_parameters$effect
      )
  } else if (method == "bf_predetermined") {
    text <- 
      glue::glue(
        "We used the Jeffrey-Zellner-Siow Bayes factor method to estimate the sample size. We used a Cauchy prior\\
        distribution centered on 0 with a scale parameter of {prior_scale}.We set the aimed \\
        TPR at {tpr}, because {tpr_justification}. The expected delta was {delta} because {delta_justification}. Our \\
        evidence threshold was {thresh}. Based on these parameters, a minimal sample size of {n1} per group was estimated \\
        in order to reach a {round(tpr_out, 1)} TPR for our design.",
        n1 = output_parameters$n1,
        tpr_out = output_parameters$tpr_out,
        prior_scale = output_parameters$prior_scale,
        tpr = output_parameters$tpr,
        tpr_justification = output_parameters$tpr_justification,
        delta = output_parameters$delta,
        delta_justification = output_parameters$delta_justification,
        thresh = output_parameters$thresh
      )
  } else if (method == "rope") {
    if (!is.na(output_parameters$n1)) {
      text <- 
        glue::glue(
          "In order to estimate the sample size, we used the Region of Practical Equivalence (Kruschke and Liddell, 2018) \\
          method. We used a Cauchy prior distribution centered on 0 with a scale parameter of {prior_scale}. We set the aimed TPR at {tpr}, because {tpr_justification}. We consider all effect sizes below \\
          {eq_band} equivalent to zero, because {eq_band_justification}. The expected delta was {delta} because {delta_justification}. \\
          Based on these parameters, a minimal sample size of {n1} per group was estimated in order to reach a {round(tpr_out,1)} TPR for our design.",
          n1 = output_parameters$n1,
          tpr_out = output_parameters$tpr_out,
          tpr = output_parameters$tpr,
          tpr_justification = output_parameters$tpr_justification,
          eq_band = output_parameters$eq_band,
          eq_band_justification = output_parameters$eq_band_justification,
          delta = output_parameters$delta,
          delta_justification = output_parameters$delta_justification,
          prior_scale = output_parameters$prior_scale
        )
    } else {
      text <- glue::glue("Error: {error_message}",
                         error_message = output_parameters$error_message)
    }
  } else if (method == "power_curve") {
    text <- 
      glue::glue(
        "We used a power analysis to estimate the sample size. We used an alpha of 0.05. We set the aimed TPR at {tpr}, \\
        because {tpr_justification}. Because we {delta_justification}, \\
        we include power calculations for delta ranging from {min(delta)} to {max(delta)}. Based on these parameters, \\
        minimal sample sizes for different hypothetical effect sizes to reach {tpr} TPR can be found in Figure X.",
        tpr = output_parameters$tpr,
        tpr_justification = output_parameters$tpr_justification,
        delta = output_parameters$delta,
        delta_justification = output_parameters$delta_justification
      )
  } else if (method == "bfda") {
    if (!is.na(output_parameters$n1)) {
      text <- 
        glue::glue(
          "We used the BFDA method to estimate the sample size. We used a Cauchy prior \\
          distribution centered on 0 with a scale parameter of {prior_scale}. We set the aimed TPR at {tpr}, \\
          because {tpr_justification}. The expected delta was {delta} because {delta_justification}. \\
          Our evidence threshold was {thresh}. Based on these parameters, a minimal sample size of {n1} \\
          per group was estimated in order to reach a {tpr} TPR for our design.",
          tpr = output_parameters$tpr,
          tpr_justification = output_parameters$tpr_justification,
          delta = output_parameters$delta,
          delta_justification = output_parameters$delta_justification,
          thresh = output_parameters$thresh,
          n1 = output_parameters$n1,
          prior_scale = output_parameters$prior_scale
          )
    } else {
      text <- glue::glue("Error: {error_message}",
                         error_message = output_parameters$error_message)
    }
    
  }
  text
}
