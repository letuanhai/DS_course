options(digits = 3)
library(tidyverse)
library(HistData)
data("GaltonFamilies")

set.seed(1983, sample.kind = "Rounding")
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# mean and standard deviation
galton_heights %>% 
  summarise(mean(father), sd(father), mean(son), sd(son))

# scatterplot of father and son heights
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)

# stratify the son heights by the standardized father heights
galton_heights %>%
  mutate(z_father = round(scale(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +  
  stat_qq(aes(sample = son)) +
  facet_wrap( ~ z_father) 

# practice ####
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

female_heights %>% summarise(mean(mother), sd(mother), mean(daughter), sd(daughter))
r_2 <- cor(female_heights$mother, female_heights$daughter)
mu_m <- mean(female_heights$mother)
s_m <- sd(female_heights$mother)
mu_d <- mean(female_heights$daughter)
s_d <- sd(female_heights$daughter)

m_1 <- r_2 * s_d/s_m
b_1 <- mu_d - m_1 * mu_m

# Least squares estimates ####
rss <- function(beta0, beta1){
  resid <- galton_heights$son - (beta0 + beta1 * galton_heights$father)
  sum(resid^2)
}

# plot RSS with  values of beta1 when beta0 is fixed at 25
beta1 <- seq(0, 1, len = nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line()

# calculate using lm() function
fit <- lm(son ~ father, data = galton_heights)
fit$coefficients

# run a Monte Carlo simulation with samples from the galton_heights data as population
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = T) %>% 
    lm(son ~ father, data = .) %>% 
    .$coef
})
lse <- data.frame(beta0=lse[1,], beta1 = lse[2,])
hist(lse$beta0)
hist(lse$beta1)

set.seed(1989, sample.kind = "Rounding")
female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

lm(mother ~ daughter, data = female_heights)

# broom and do ####
library(broom)

set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton %>% group_by(pair) %>% 
  summarise(n())

galton %>% group_by(pair) %>% 
  summarise(cor = cor(parentHeight, childHeight)) %>% 
  arrange(cor)

galton %>% group_by(pair) %>% 
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = T)) %>% 
  filter(term == "parentHeight") %>% 
  ggplot(aes(pair, estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()
