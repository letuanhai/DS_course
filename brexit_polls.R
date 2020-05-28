# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread

N <- 1500
total_exp <- N*p
se_exp <- sqrt(p * (1 - p) * N)

se_hat <- sqrt(p * (1 - p) / N)

se_d <- 2 * se_hat

brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1) / 2)

summarise(brexit_polls, mean_spread = mean(spread),
          sd_spread = sd(spread), mean_x = mean(x_hat), sd_x = sd(x_hat))

june_polls <- brexit_polls %>% filter(enddate >= "2016-06-01") %>% 
  mutate(se_x_hat = sqrt(x_hat * (1 - x_hat) / samplesize),
         se_spread = 2 * se_x_hat,
         lower = spread - 1.96 * se_spread,
         upper = spread + 1.96 * se_spread,
         hit = d >= lower & d <= upper)

with(june_polls, mean(lower <= 0 & upper >= 0))
with(june_polls, mean(lower > 0))
mean(june_polls$hit)

june_polls %>% group_by(pollster) %>%
  summarise(hit_rate = mean(hit),
            n = n()) %>% 
  arrange(hit_rate)

june_polls %>% group_by(poll_type) %>% 
  ggplot(aes(poll_type, spread)) +
  geom_boxplot()

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2) %>% 
  mutate(se_spread = 2 * sqrt(p_hat * (1 - p_hat) / N),
         lower = spread - 1.96 * se_spread,
         upper = spread + 1.96 * se_spread)

brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

# chi-square test ####
twoxtwo <- brexit_hit %>% group_by(poll_type, hit) %>% summarise(n = n()) %>% 
  spread(poll_type, n)

twoxtwo %>% 
  select(-hit) %>% chisq.test()

with(twoxtwo, (Online[2]/sum(Online)) / (Online[1]/sum(Online)))
with(twoxtwo, (Telephone[2]/sum(Telephone)) / (Telephone[1]/sum(Telephone)))

# plotting spread over time ####
brexit_polls %>% ggplot(aes(enddate, spread, col = poll_type)) +
  geom_smooth(method = "loess", span = 0.4) +
  geom_point() +
  geom_hline(yintercept = -.038)

brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

ggplot(brexit_long, aes(enddate, proportion, col = vote)) +
  geom_smooth(method = "loess", span = .3)
