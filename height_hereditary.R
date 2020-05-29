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
