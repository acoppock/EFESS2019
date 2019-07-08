# European Field Experiments Summer School 2018
# Lab 2

# Be sure to set your working directory!

rm(list = ls())

library(tidyverse)
library(randomizr)
library(estimatr)
library(ri2)

dat <- read_csv("data/lab_2.csv")

# Look at the data

head(dat)

# Obtaining Standard Errors -----------------------------------------------

# Difference-in-means:

ate_hat_dim <- difference_in_means(Y ~ Z, data = dat)
tidy(ate_hat_dim)

# same as unadjusted OLS:
fit <- lm_robust(Y ~ Z, data = dat)
tidy(fit)

# estimated standard error (equation 3.6)
with(dat, sqrt(var(Y[Z == 1]) / sum(Z == 1) +
                 var(Y[Z == 0]) / sum(Z == 0)))

# exactly the same as HC2 robust standard errors:
tidy(fit)

# slightly different from the classical standard errors:
fit_classical <- lm_robust(Y ~ Z, se_type = "classical", data = dat)
tidy(fit_classical)

# same as built in lm()
fit_lm <- lm(Y ~ Z, data = dat)
tidy(fit_lm)

# if you're doing an individually randomized experiment, HC2 standard errors are almost always the way to go.

# Randomization Inference -------------------------------------------------

# Step 1: Make Hypothesized Outcomes under sharp null hypothesis

dat <-
  dat %>%
  mutate(
    Y0_star = Y,
    Y1_star = Y
  )


# exactly zero treatment effect for every unit
with(dat, Y1_star - Y0_star)

# Step 2: set up loop

sims <- 1000
simulated_ates <- rep(NA, sims)

for (i in 1:sims) {
  # Do a random assignment
  dat <-
    dat %>%
    mutate(
      Z_sim = complete_ra(N = 500, m = 200),
      Y_sim = Y1_star * Z_sim + Y0_star * (1 - Z_sim)
    )
  
  fit_sim <- difference_in_means(Y_sim ~ Z_sim, data = dat)
  simulated_ates[i] <- fit_sim$coefficients
  
}

# Step 3: take a look at the null distribution and compare it to the observed value


ate_hat_obs <- ate_hat_dim$coefficients

qplot(simulated_ates) + geom_vline(xintercept = ate_hat_obs, color = "red")

p_value_ri <- mean(abs(simulated_ates) >= ate_hat_dim$coefficients)
p_value_ri

# compare to
tidy(fit)

# Using ri2 ---------------------------------------------------------------

# declare randomization procedure:
ra_declaration <- declare_ra(N = 500, m = 200)
fit_ri <- conduct_ri(Y ~ Z, declaration = ra_declaration, data = dat)
fit_ri

# Comparing Blocking to Covariate Adjustment ------------------------------

# plot the outcome against the pre score, coloring by treatment assignment

ggplot(dat, aes(x = pre_score, y = Y, color = Z)) +
  geom_point() +
  theme_bw()

# Goal: simulate the TRUE sampling distribution when
## A: the treatment is blocked by pre_score quantile and we use DIM
## B: we use complete random assignment and just control for pre_score quantile

dat <-
  dat %>%
  mutate(pre_score_quantile =
           cut(
             pre_score,
             breaks = quantile(pre_score, probs = c(0, .2, .4, .6, .8, 1)),
             include.lowest = TRUE
           ))

# check the variable we made
table(dat$pre_score_quantile, useNA = "always")

# check out the block random assignment procedure.

dat <-
  dat %>%
  mutate(Z_blocked = block_ra(pre_score_quantile))

with(dat, table(pre_score_quantile, Z_blocked))

# SIMULATION A: block the treatment, use difference-in-means
sims <- 1000
estimates_A <- rep(NA, sims)

for (i in 1:sims) {
  dat <-
    dat %>%
    mutate(
      # Do a blocked random assignment
      Z_sim = block_ra(pre_score_quantile),
      # Do the switching equation
      Y_sim = Y_Z_0 * (1 - Z_sim) + Y_Z_1 * (Z_sim)
    )
  
  fit <- difference_in_means(formula = Y_sim ~ Z_sim, blocks = pre_score_quantile, data = dat)
  estimates_A[i] <- fit$coefficients
}

# SIMULATION B: use complete_ra, use difference-in-means and OLS
sims <- 1000
estimates_B_dim <- rep(NA, sims)
estimates_B_ols <- rep(NA, sims)

for (i in 1:sims) {
  dat <-
    dat %>%
    mutate(
      # Do a blocked random assignment
      Z_sim = complete_ra(N = 500, m = 250),
      # Do the switching equation
      Y_sim = Y_Z_0 * (1 - Z_sim) + Y_Z_1 * (Z_sim)
    )
  
  # difference-in-means
  fit_dim <- difference_in_means(Y_sim ~ Z_sim, data = dat)
  fit_ols <- lm_robust(Y_sim ~ Z_sim + pre_score_quantile, data = dat)
  estimates_B_dim[i] <- fit_dim$coefficients
  estimates_B_ols[i] <- fit_ols$coefficients[2]
}


gg_df <-
  data.frame(
    facet = rep(c(
      "blocked + DIM", "Complete + DIM", "Compete + OLS"
    ), each = sims),
    estimates = c(estimates_A, estimates_B_dim, estimates_B_ols)
  )


ggplot(gg_df, aes(estimates)) +
  geom_histogram(bins = 30) +
  facet_wrap(~facet) +
  theme_bw()



sd(estimates_A)
sd(estimates_B_dim)
sd(estimates_B_ols)

# Bottom line: Blocking is great! But you can do approximately as well with OLS.

