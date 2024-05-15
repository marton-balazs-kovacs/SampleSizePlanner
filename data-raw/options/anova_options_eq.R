############################Bayesian Equivalence interval##########################################
# Create a csv file containing all configurations used for the pre calculations
# Create a data frame storing all possible configurations for the method
bayes_anova_options <-
  as.data.frame(
    tidyr::expand_grid(
      m11 = 0,
      m12 = base::seq(0, 1.25, by = 0.25),
      m21 = base::seq(0, 1.25, by = 0.25),
      m22 = base::seq(0, 1.25, by = 0.25),
      effect = c("Main Effect 1", "Main Effect 2"),
      eq_band = base::seq(0.1, 0.3, by = 0.05),
      tpr = seq(0.7, 0.9, by = 0.05),
      thresh = c(10),
      prior_scale = c(1 / sqrt(2))
    ))

# sort the mean vectors
# create sorting function
sort_mu <- function (mu) {
  if (mu[2]==base::min(mu)) {mu <- mu[c(2,1,4,3)]}      # sort mu[1]=min(mu)
  if (mu[3]==base::min(mu)) {mu <- mu[c(3,4,1,2)]}
  if (mu[4]==base::min(mu)) {mu <- mu[c(4,3,2,1)]}
  if (mu[1]==mu[2] & mu[3]>mu[4]) {mu <- mu[c(2,1,4,3)]}# if mu[1]=mu[2] or mu[1]=mu[3] we can apply operations again
  if (mu[1]==mu[3] & mu[2]>mu[4]) {mu <- mu[c(3,4,1,2)]}
  if (mu[2]>mu[3]) {mu <- mu[c(1,3,2,4)]}               # sort mu[2]<mu[3], main effects change 
  return (mu)
}

# Sort the mean vector with the sort function and store the sorted mean vectors in matrix
for (i in 1:base::nrow(bayes_anova_options)) {
  mu <- bayes_anova_options[i,1:4]
  bayes_anova_options[i,1:4] <- sort_mu(mu)
} 

# remove equivalent rows
bayes_anova_options <- unique(bayes_anova_options)

# store in .csv file
write.csv(bayes_anova_options, "X:\\My Documents\\R\\data\\SSP\\ssp_anova_options_eq.csv", row.names=FALSE)
