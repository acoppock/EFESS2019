rm(list = ls())

library(tidyverse)
library(randomizr)
library(estimatr)

rind <- read_csv("data/Rind_Bordia_JASP_1996.csv")

# check out the dataset
head(rind)

# treat 45 of 89
table(rind$happyface)

# Question 1: difference-in-variances -------------------------------------

# obtain observed diff-in-variances
div_obs <- with(rind, var(tip[happyface == 1]) - var(tip[happyface == 0]))
div_obs

# generate a full schedule of potential outcomes
ate_obs <- with(rind, mean(tip[happyface == 1]) - mean(tip[happyface == 0]))
ate_obs

rind <-
  rind %>%
  mutate(
    Y0_star = (tip - ate_obs) * happyface + tip * (1 - happyface),
    Y1_star = tip * happyface + (tip + ate_obs) * (1 - happyface)
  )

rind %>% select(Y0_star, Y1_star) %>% head

# Set up a loop to get null distribution
sims <- 1000
div_sims <- rep(NA, sims)

# generate the sampling distribution of the difference in variances
for (i in 1:sims){
  
  rind <-
    rind %>%
    mutate(Z_sim = complete_ra(N = 89, m = 45),
           Y_sim = Y1_star * Z_sim + Y0_star * (1 - Z_sim))
  
  div_sims[i] <- with(rind, var(Y_sim[Z_sim == 1]) - var(Y_sim[Z_sim == 0]))
  
}

qplot(div_sims) + geom_vline(xintercept = div_obs, color = "red")
      
# p-value
mean(abs(div_sims) >= abs(div_obs)) 
  
# Question 2: Write down a regression model -------------------------------

# Y_i = \beta_0 + \beta_1 happyface + \beta_2 female + \beta_3 (happyface * female) + e_i

# Question 3: Estimate! ---------------------------------------------------

fit <- lm_robust(tip ~ happyface + female + happyface*female, se_type = "classical", data = rind)
tidy(fit)




summary_table <-
  rind %>%
  mutate(gender = factor(female, levels = c(0, 1), labels = c("Male", "Female"))) %>% 
  group_by(gender, happyface) %>%
  summarise(group_mean = mean(tip))


ggplot(summary_table, 
       aes(x = happyface, y = group_mean, color = gender, group = gender)) +
  geom_point() +
  geom_line(aes(linetype = gender)) +
  facet_wrap(~gender) +
  theme_bw() +
  xlab("Happy Face Treatment Assignment") +
  ylab("Group Average Tip Percentage") +
  theme(legend.position = "none")


ggplot(summary_table, aes(x = happyface, y = group_mean)) +
  geom_bar(stat = "identity") +
  facet_wrap(~female)







