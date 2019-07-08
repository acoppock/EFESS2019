# European Field Experiments Summer School 2018
# Lab 3

# Be sure to set your working directory!

rm(list = ls())

library(tidyverse)
library(randomizr)
library(estimatr)
library(ri2)

dat <- read_csv("data/lab_3.csv")
head(dat)

# some useful recodes

dat <-
  dat %>%
  mutate(
    city = factor(chicago, 0:1, c("Boston", "Chicago")),
    quality = factor(lowquality, 0:1, c("High quality", "Low quality")),
    race = factor(black, 0:1, c("White name", "Black name"))
  )

head(dat)


# Tables before regression ------------------------------------------------

summary_table <-
  dat %>%
  group_by(city, quality, race) %>%
  summarize(group_mean = mean(Y))

summary_table

# slightly more advanced....

summary_table <-
  dat %>%
  group_by(city, quality, race) %>%
  do(tidy(lm_robust(Y ~ 1, data = .)))

summary_table

# The Regression ----------------------------------------------------------

# The regression is hard to read!
fit <- lm_robust(Y ~ black*chicago*lowquality, data = dat)
tidy(fit)

# you can go back and forth between the tables
tidy(fit)$estimate[1]
summary_table[1,]

tidy(fit)$estimate[1] + tidy(fit)$estimate[2]
summary_table[2,]




# Plots -------------------------------------------------------------------


ggplot(summary_table, aes(x = race, y = estimate, group = quality, color = quality)) +
  geom_point() +
  geom_line() +
  facet_wrap( ~ city) +
  theme_bw()

# slightly more advanced... How to improve this graph?

label_df <- summary_table %>% filter(race == "White name", city == "Boston")
library(ggrepel)

ggplot(summary_table, aes(x = race, y = estimate, group = quality, color = quality, shape = quality)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5), width = 0) +
  geom_text_repel(data = label_df, aes(label = quality), position = position_dodge(width = 0.5) ) +
  geom_point(data = dat, aes(y = Y), 
             position = position_jitterdodge(jitter.width = 0.3, jitter.height = 0.02, dodge.width = 0.5),
             alpha = 0.05) +
  scale_color_manual(values = c("blue", "red")) +
  ylab("Call Back Rate") +
  facet_wrap( ~ city) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        strip.background = element_blank())


# There are 4 slopes with respect to "Black"... let's calculate each slope with regression.
slopes <-
  dat %>%
  group_by(city, quality) %>%
  do(tidy(lm_robust(Y ~ black, data = .))) %>%
  filter(term == "black")

# we could also plot the slopes!
ggplot(slopes, aes(x = quality, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
  facet_wrap( ~ city) 

# Questions for discussion

# 1. What is the effect of low quality in Chicago? is it significant?
# 2. What is the effect of low quality in Boston? is it significant?
# 3. What is the *difference* in the effects of low quality across contexts?
# 4. The regression lm_robust(Y ~ black*chicago*lowquality, data = dat) reports 8 numbers.  Which are causal? Which are descriptive?





