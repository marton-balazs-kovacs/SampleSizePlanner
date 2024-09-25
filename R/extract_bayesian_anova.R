#############################################################################################
# Bayesian ANOVA.
# Function to retrieve the minimal sample size given input parameters from the pre-calculations
# together with the re-scaled and re-ordered mu and tpr.

extract_bayesian_anova <- function(pre_data, mu_ui, sigma_ui, tpr_ui, thresh_ui, prior_scale_ui, effect_ui) {
  bayes_anova_data <- pre_data
  
  # rescale first mu to zero
  scale_mu <- mu_ui[1]                   # save mu1 for re-scaling
  mu_scaled <- mu_ui - mu_ui[1]          # scale the mean of first group to 0
  
  # Re-scale the input mu and sigma
  mu_scaled <- mu_scaled / sigma_ui             # scale mu by sigma_ui
  
  # sort the input means and track effect swap
  sort_mu <- function (mu) {
    effect_swap <- FALSE
    if (mu[2]==base::min(mu)) {mu <- mu[c(2,1,4,3)]}      # sort mu[1]=min(mu)
    if (mu[3]==base::min(mu)) {mu <- mu[c(3,4,1,2)]}
    if (mu[4]==base::min(mu)) {mu <- mu[c(4,3,2,1)]}
    if (mu[1]==mu[2] & mu[3]>mu[4]) {mu <- mu[c(2,1,4,3)]}# if mu[1]=mu[2] or mu[1]=mu[3] we can apply operations again
    if (mu[1]==mu[3] & mu[2]>mu[4]) {mu <- mu[c(3,4,1,2)]}
    
    if (mu[2]>mu[3]) 
    {mu <- mu[c(1,3,2,4)]         # sort mu[2]<mu[3], main effects change!
    effect_swap <- TRUE}          # add index if main effects changed
    
    return(list(mu,effect_swap))
  }
  sorted_mu <- sort_mu(mu_scaled)[[1]]
  effect_swap <- sort_mu(mu_scaled)[[2]]
  
  
  # account for effect swap in sorting
  if (effect_ui == "Main Effect 1" & effect_swap) {
    effect_extract <- "Main Effect 2"} else if 
  (effect_ui == "Main Effect 2" & effect_swap) {
    effect_extract <- "Main Effect 1"} else {
      effect_extract <- effect_ui}

    # Filter the results
  result_filter <- bayes_anova_data %>%
    dplyr::filter(
      dplyr::near(tpr_in, tpr_ui),
      dplyr::near(thresh, as.numeric(thresh_ui)),
      dplyr::near(prior_scale, 
        as.numeric(
          eval(
            parse(
              text=prior_scale_ui)
            )
          )
        )) %>%
    dplyr::filter(effect == effect_extract)
  
  # calculate deviance
  dev = c()
  for (i in 1:nrow(result_filter)) {
    dev[i] <- sum((sorted_mu - unlist(result_filter[i,"mu"]))^2)
  }
  
  # select result with minimum deviance
  result <- result_filter[which.min(dev),]
  
  # re-scale and un-standardize mu
  rescaled_mu <- (unlist(result$mu) * sigma_ui) + scale_mu
  
  # order the re-scaled mu according the the order of mu
  # open vector for the ordered mu
  ordered_mu <- c(1:4)
  
  # extract ordering from mu
  ordering <- order(mu_ui)
  
  # add min and max mu
  ordered_mu[ordering[1]] <- min(rescaled_mu)
  ordered_mu[ordering[4]] <- max(rescaled_mu)
  
  # remove the min and max mu from the rescaled mu
  rescaled_mu <- rescaled_mu[c(-which(rescaled_mu == min(rescaled_mu))[1]
                               ,-which(rescaled_mu == max(rescaled_mu))[1])]
  
  # check which remaining mu is smaller and add it
  if (rescaled_mu[1] <= rescaled_mu[2]) {
    ordered_mu[ordering[2]] <- rescaled_mu[1]
    ordered_mu[ordering[3]] <- rescaled_mu[2]
  } else {
    ordered_mu[ordering[2]] <- rescaled_mu[2]
    ordered_mu[ordering[3]] <- rescaled_mu[1]
  }
  
  if(is.na(result$n1[[1]])) {
    return(list(pre_mu = ordered_mu, message = result$error_message))}
  else {
    return(list(pre_mu = ordered_mu, pre_tpr_out = result$tpr_out, pre_n1 = result$n1, message = result$error_message))}
}

# don't run
# extract_bayesian_anova(pre_data = bayes_anova_data, mu_ui = c(1,1.2,1.5,1.3), sigma_ui = 1.2, tpr_ui = 0.7, thresh_ui = 10, prior_scale_ui = c("1 / sqrt(2)"), effect_ui = "Main Effect 1")

