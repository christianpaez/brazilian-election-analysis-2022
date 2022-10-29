library(rvest)
library(httr)
library(stringr)
library(tidyverse)
options(digits = 3)

dataframe_name = "brazilian-presidential-election-polling-data.csv"
second_round_dataframe = as.data.frame(read_csv(dataframe_name))


# We will provide an estimate for the proportion of votes for Lula
# let's make an estimate of the spread mean, se and a combined ci

combined_estimates = second_round_dataframe %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - (qnorm(0.975) * se), end = avg + (qnorm(0.975) * se))

# taken from 1989-2018 election results
historical_spread_mean = 0.12

# Bayes Theorem
mu = 0
tau = historical_spread_mean
general_bias = 0.025 # general bias seen in opinion polls in USA, may be estimated later
sigma = sqrt((combined_estimates$se) ^ 2 + (general_bias ^ 2))
Y = combined_estimates$avg
B = sigma^2 / (sigma^2 + tau^2)
posterior_mean = B * mu + (1 - B) * Y
posterior_se = sqrt(1 / ((1 / sigma^2) + (1 / tau^2)))
d_hat_ci = c(posterior_mean - (posterior_se * qnorm(0.975)), 
             posterior_mean + (posterior_se * qnorm(0.975)))
d_hat = 1 - pnorm(0, posterior_mean, posterior_se)

# Monte Carlo Simulation
results = as.data.frame(rnorm(1000, posterior_mean, posterior_se))
colnames(results) = "spread"
results = results %>%
  mutate(lula_wins = spread > 0)


winner_plot = results %>% 
  ggplot(aes(spread, fill = lula_wins)) + 
  geom_histogram(binwidth = 0.01, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", col = "red") +
  labs(x = NULL,
       y = NULL,
       fill = "Winner",
       title ="2022 Brazilian Election Second Round Winner",
       caption = paste("Last Updated: ", Sys.Date(), " - Source: Art of Code"),
       alt = "2022 Brazilian Election Second Round Winner") +
  scale_fill_discrete(breaks=c("FALSE", "TRUE"),
                      labels=c("Bolsonaro", "Lula")) +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

