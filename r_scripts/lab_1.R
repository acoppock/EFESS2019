# European Field Experiments Summer School 2018
# Lab 1

# Be sure to set your working directory!

rm(list = ls())

library(tidyverse)
library(randomizr)
library(estimatr)

# Load the csv
dat <- read_csv("data/lab_1.csv")

# Step 1: Explore the data ------------------------------------------------

# Take a look at the data
head(dat)
View(dat)

table(dat$Y0)
table(dat$Y1)

# this will give an error -- why?
mean(Y1 - Y0)

# Find the true (not estimated) ATE
true_ate <- with(dat, mean(Y1 - Y0))

# Graph the potential outcomes against one another
ggplot(dat, aes(x = Y0, y = Y1)) + 
  geom_point(position = position_jitter(width = .1, height = .1)) +
  geom_abline(slope = 1, intercept = 0)
# units above the 45 degree line have *positive* treatment effects

# Graph the true treatment effects for each unit
ggplot(dat, aes(x = Y1 - Y0)) + geom_histogram(bins = 15)


# Randomize ---------------------------------------------------------------

dat <-
  dat %>%
  mutate(
    Z = complete_ra(N = 100, m = 50),
    Y = Y1 * Z + Y0 * (1 - Z)
  )

head(dat)

# Estimate ----------------------------------------------------------------

fit <- difference_in_means(Y ~ Z, data = dat)
tidy(fit)

# exactly identical!
fit <- lm_robust(Y ~ Z, data = dat)
tidy(fit)

# Simulate a sampling distribution ----------------------------------------

# How many ways are there to allocate 50 treatments among 100 units?
choose(100, 50) # lots

# simulation
sims <- 1000
ate_hats <- rep(NA, sims)

for (i in 1:sims) {
  
  dat <-
    dat %>%
    mutate(
      Z_sim = complete_ra(N = 100, m = 50),
      Y_sim = Y1 * Z_sim + Y0 * (1 - Z_sim)
    )
  
  fit_sim <- difference_in_means(Y_sim ~ Z_sim, data = dat)
  ate_hats[i] <- fit_sim$coefficients
  
}

# look at these all!
ate_hats


# Visualize the sampling distribution
qplot(ate_hats, bins = 50) + geom_vline(xintercept = true_ate, color = "red")

# Unbiasedness
mean(ate_hats)
true_ate

# This is the SD of the sampling distribution
# It's also the *true* (not estimated) standard error
sd(ate_hats)

# check against equation 3.4

pop_var <- function(x){
  mean((x - mean(x)) ^ 2)
}

pop_cov <- function(x, y){
  mean((x - mean(x)) * (y - mean(y)))
}

eq_3.4 <- function(Y0, Y1, m){
  N <- length(Y0)
  sqrt(1 / (N - 1) *
         (pop_var(Y0) * (m / (N - m)) +
            pop_var(Y1) * ((N - m) / m) +
            2 * pop_cov(Y1, Y0))
  )
}

# Matches!
with(dat, eq_3.4(Y0, Y1, m = 50))
sd(ate_hats)

