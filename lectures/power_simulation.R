rm(list = ls())
library(randomizr)
library(estimatr)

N <- 3000

# Set up loop
sims <- 1000

ps_1 <- ps_2 <- ps_3 <- rep(NA, sims)

for(i in 1:sims){
  # Potential outcomes (M)
  Y_0_0 <- rbinom(n = N, size = 1, prob = 0.30)
  Y_1_0 <- rbinom(n = N, size = 1, prob = 0.35)
  Y_0_1 <- rbinom(n = N, size = 1, prob = 0.40)
  Y_1_1 <- rbinom(n = N, size = 1, prob = 0.50)
  
  # Randomization (D)
  
  # one way to get 1/4 per cell
  Z <- complete_ra(N = N, num_arms = 4)
  
  # let's proceed with this way
  Z1 <- complete_ra(N = N)
  Z2 <- block_ra(blocks = Z1)
  
  Y <- rep(NA, N)
  Y[Z1 == 0 & Z2 == 0] <- Y_0_0[Z1 == 0 & Z2 == 0]
  Y[Z1 == 1 & Z2 == 0] <- Y_1_0[Z1 == 1 & Z2 == 0]
  Y[Z1 == 0 & Z2 == 1] <- Y_0_1[Z1 == 0 & Z2 == 1]
  Y[Z1 == 1 & Z2 == 1] <- Y_1_1[Z1 == 1 & Z2 == 1]
  
  # Estimation (A)
  dat <- data.frame(Z1, Z2, Y)
  
  # ATE-hat of Z1
  fit_1 <- lm_robust(Y ~ Z1, data = dat)
  # ATE-hat of Z2
  fit_2 <- lm_robust(Y ~ Z2, data = dat)
  # difference in effects
  fit_3 <- lm_robust(Y ~ Z1 + Z2 + Z1*Z2, data = dat)
  
  ps_1[i] <- tidy(fit_1)$p.value[2]
  ps_2[i] <- tidy(fit_2)$p.value[2]
  ps_3[i] <- tidy(fit_3)$p.value[4]
  
}

mean(ps_1 <= 0.05)
mean(ps_2 <= 0.05)
mean(ps_3 <= 0.05)





