# European Field Experiments Summer School 2018
# Lab 4

# Be sure to set your working directory!

rm(list = ls())

library(tidyverse)
library(estimatr)

three_group <- read_csv("data/Gerber_et_al_PA_2010.csv")

three_group <-
  three_group %>%
  # drop two-person households and those with missing outcome data
  filter(hhcount == 1,!is.na(p2008_fi)) %>%
  mutate(Z = factor(
    treatmen,
    levels = c(0, 2, 1),
    labels = c("control", "placebo", "treatment")
  ),
  Y = p2008_fi)

with(three_group, table(Z))
with(three_group, table(Y, Z))
with(three_group, table(contact, Z))

# Imagine this is a "normal" phone call experiment ------------------------

two_group <- 
  three_group %>% 
  filter(Z != "placebo") %>%
  mutate(D = as.numeric(contact == "yes"))

fit_itt <- lm_robust(Y ~ Z, data = two_group)
fit_ittd <- lm_robust(D ~ Z, data = two_group)

tidy(fit_itt)
tidy(fit_ittd)

cace_est_1 <- coef(fit_itt)[2] / coef(fit_ittd)[2]
cace_est_1

# equivalent to two-stage least squares:
fit_2sls <- iv_robust(Y ~ D | Z, data = two_group)
tidy(fit_2sls)

# Check against the AER package
fit_aer <- AER::ivreg(Y ~ D | Z, data = two_group)
summary(fit_aer) # notice SEs are not "robust"

# Imagine this is only placebo controlled ---------------------------------

placebo_controlled <-
  three_group %>%
  filter(Z != "control", contact == "yes")

fit_placebo <- lm_robust(Y ~ Z, data = placebo_controlled)
tidy(fit_placebo)

cace_est_2 <- coef(fit_placebo)[2]
cace_est_2

# These authors give a specialized estimator to combine these estimates.

cace_est_1
cace_est_2
