# Validation check for precalculations

# load results
bayes_anova_res <- readRDS("/Users/lortz/Desktop/Research Master/Projects/Sample Size planner/Recalc/ssp_bayes_anova_res.rds")
eq_anova_res <- readRDS("/Users/lortz/Desktop/Research Master/Projects/Sample Size planner/Recalc/ssp_eq_anova_res.rds")
rope_anova_res <- readRDS("/Users/lortz/Desktop/Research Master/Projects/Sample Size planner/Recalc/ssp_rope_anova_res.rds")

library(purrr)
library(dplyr)

bayes_anova_res_recalc <- bayes_anova_res %>%
  imap_dfr(     # iterate with index, row-bind into a tibble
    ~ {
      params <- .x$parameters
      out     <- .x$output$result
      
      tibble(
        id         = as.integer(.y),
        tpr        = params$tpr,
        effect     = params$effect,
        thresh     = params$thresh,
        prior_scale= params$prior_scale,
        mu1        = params$mu[1],
        mu2        = params$mu[2],
        mu3        = params$mu[3],
        mu4        = params$mu[4],
        sigma      = params$sigma,
        n1         = out$n1,
        tpr_out    = out$tpr_out
      )
    }
  )

eq_anova_res_recalc <- eq_anova_res %>%
  imap_dfr(     # iterate with index, row-bind into a tibble
    ~ {
      params <- .x$parameters
      out     <- .x$output$result
      
      tibble(
        id         = as.integer(.y),
        tpr        = params$tpr,
        effect     = params$effect,
        thresh     = params$thresh,
        prior_scale= params$prior_scale,
        eq_band    = params$eq_band,
        mu1        = params$mu[1],
        mu2        = params$mu[2],
        mu3        = params$mu[3],
        mu4        = params$mu[4],
        sigma      = params$sigma,
        n1         = out$n1,
        tpr_out    = out$tpr_out
      )
    }
  )

rope_anova_res_recalc <- rope_anova_res %>%
  imap_dfr(     # iterate with index, row-bind into a tibble
    ~ {
      params <- .x$parameters
      out     <- .x$output$result
      
      tibble(
        id         = as.integer(.y),
        tpr        = params$tpr,
        effect     = params$effect,
        thresh     = params$thresh,
        prior_scale= params$prior_scale,
        eq_band    = params$eq_band,
        ci         = params$ci,
        mu1        = params$mu[1],
        mu2        = params$mu[2],
        mu3        = params$mu[3],
        mu4        = params$mu[4],
        sigma      = params$sigma,
        n1         = out$n1,
        tpr_out    = out$tpr_out
      )
    }
  )

print(bayes_anova_res_recalc, n = 100)
print(eq_anova_res_recalc, n = 100)
print(rope_anova_res_recalc, n = 100)


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
