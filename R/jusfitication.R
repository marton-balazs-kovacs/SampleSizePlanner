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
