# Validation check for precalculations
##### Checking results of Recalculations #####

# rope method
rope_res <- cbind(c(1:50),c(1:50))
load("/Users/lortz/Desktop/Research Master/Projects/Sample Size planner/Recalc/rope_anova_data.rda")

valid_rows <- which(!is.na(rope_anova_data$n1))
idx <- sample(valid_rows, 50)
for (i in 1:50) {
  row <- rope_anova_data[idx[i],]
  res <- twoway_ANOVA_rope_pwr(
    n      = row$n1,
    effect = row$effect,
    eq_band = row$eq_band,
    iter   = row$iter,
    post_iter = row$post_iter,
    mu     = unlist(row$mu),
    sigma  = row$sigma,
    ci = row$ci,
    prior_scale = row$prior_scale,
    prior_location = row$prior_location,
    seed   = NULL
  )
  rope_res[i,] <- c(res, row$tpr_out)
  cat("power is ", res, "tpr_out was", row$tpr_out, "\n")
}


# eq method
eq_res <- cbind(c(1:50),c(1:50))
load("/Users/lortz/Desktop/Research Master/Projects/Sample Size planner/Recalc/eq_anova_data.rda")

valid_rows <- which(!is.na(eq_anova_data$n1))
idx <- sample(valid_rows, 50)
for (i in 1:50) {
  row <- eq_anova_data[idx[i],]
  res <- twoway_ANOVA_eq_pwr(
    n      = row$n1,
    effect = row$effect,
    eq_band = row$eq_band,
    iter   = row$iter,
    post_iter = row$post_iter,
    mu     = unlist(row$mu),
    sigma  = row$sigma,
    thresh = row$thresh,
    prior_scale = row$prior_scale,
    prior_location = row$prior_location,
    seed   = NULL
  )
  eq_res[i,] <- c(res, row$tpr_out)
  cat("power is ", res, "tpr_out was", row$tpr_out)
}

# bayesian anova
bayes_res <- cbind(c(1:50),c(1:50))
load("/Users/lortz/Desktop/Research Master/Projects/Sample Size planner/Recalc/bayes_anova_data.rda")

valid_rows <- which(!is.na(bayes_anova_data$n1))
idx <- sample(valid_rows, 50)
for (i in 1:50) {
  row <- bayes_anova_data[idx[i],]
  res <- twoway_ANOVA_bf_pwr(
    n      = row$n1,
    effect = row$effect,
    iter   = 5000,
    mu     = unlist(row$mu),
    sigma  = row$sigma,
    thresh = row$thresh,
    prior_scale = row$prior_scale,
    max_bf = 1e8,
    seed   = NULL
  )
  bayes_res[i,] <- c(res, row$tpr_out)
  cat("power is ", res, "tpr_out was", row$tpr_out)
}

hist(bayes_res[,2]-bayes_res[,1])
hist(eq_res[,2]-eq_res[,1])
hist(rope_res[,2]-rope_res[,1])

mean(bayes_res[,2]-bayes_res[,1])
mean(eq_res[,2]-eq_res[,1])
mean(rope_res[,2]-rope_res[,1])

sd(bayes_res[,2]-bayes_res[,1])
sd(eq_res[,2]-eq_res[,1])
sd(rope_res[,2]-rope_res[,1])
