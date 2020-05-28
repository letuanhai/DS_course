library(tidyverse)
library(dslabs)

set.seed(3, sample.kind = "Rounding")

polls <- polls_us_election_2016 %>% 
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% group_by(pollster) %>% 
  filter(enddate == max(enddate)) %>% 
  ungroup()

results <- one_poll_per_pollster %>% 
  summarise(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>% 
  mutate(start = avg - 1.96 * se, end = avg + 1.96 * se)

#### Predicting popular vote ####
# general bias for 2016 has mean of 0 and standard error of .025
mu <- 0
sigma_b <- .025
tau <- .035
sigma <- sqrt(results$se^2 + sigma_b^2)
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- B*mu + (1 - B)*Y
posterior_se <- sqrt(1/(1/sigma^2 + 1/tau^2))

# calculate probability of Cling winning (spread > 0)
1 - pnorm(0, posterior_mean, posterior_se)

#### Predicting the electoral college ####

results <- polls_us_election_2016 %>%
  filter(state!="U.S." & 
           !str_detect(state, "CD") & 
           enddate >="2016-10-31" & 
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))
results <- left_join(results, results_us_election_2016, by = "state")
results <- results %>% 
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = T), sd))

# assume we know nothing about prior mean of each state, prior standard error is
# .02 based on hitorical data
mu <- 0
tau <- .02

# assume general bias at state level with mean 0 and se = .03
bias_sd <- .03

# Use Monte Carlo simulation to generate random samples
B <- 10000
clinton_votes <- replicate(B, {
  results %>% mutate(sigma = sqrt(sd^2/n + bias_sd^2),
                     B = sigma^2 / (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1 - B)*avg,
                     posterior_se = sqrt(1/(1/sigma^2 + 1/tau^2)),
                     result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(result > 0, electoral_votes, 0)) %>% 
    summarise(clinton = sum(clinton) + 7) %>% 
    pull(clinton)
})

mean(clinton_votes > 269)

#### Practice ####
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Create an object called `cis` that has the columns indicated in the instructions
cis <- polls %>% mutate(X_hat = (spread + 1) / 2,
                        se = 2 * sqrt(X_hat * (1 - X_hat) / samplesize),
                        lower = qnorm(.025, spread, se),
                        upper = qnorm(.975, spread, se)) %>%
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of hits for each pollster that has at least 5 polls.
p_hits <- ci_data %>% mutate(hit = actual_spread >= lower & actual_spread <= upper) %>%
  group_by(pollster) %>%
  summarize(propotion_hits = mean(hit),
            n = n(),
            grade = grade[1]) %>%
  filter(n >= 5) %>%
  arrange(desc(propotion_hits))

