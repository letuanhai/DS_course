options(digits = 3)

library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

# plot home run vs run
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# plot stolen base vs run
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# plot base on ball vs run
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# plot run vs at bat
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB / G, R_per_game = R / G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB / G, R_per_game = R / G) %>%
  summarize(r = mean(scale(AB_per_game) * scale(R_per_game)))
  
# plot win vs fielding errors
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(W_per_game = W / G, E_per_game = E / G) %>%
  ggplot(aes(E_per_game, W_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(W_per_game = W / G, E_per_game = E / G) %>%
  summarize(r = mean(scale(W_per_game) * scale(E_per_game)))
  
# plot triples vs doubles
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X2B_per_game = X2B / G, X3B_per_game = X3B / G) %>%
  ggplot(aes(X2B_per_game, X3B_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X2B_per_game = X2B / G, X3B_per_game = X3B / G) %>%
  summarise(r = mean(scale(X2B_per_game) * scale(X3B_per_game)))

#### Confounding ####

# function to get slope
get_slope <- function(x, y) cor(x, y) * sd(y) / sd(x)

# slope of BBs vs runs
Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  summarise(slope = get_slope(BB_per_game, R_per_game))

# slope of singles vs runs
Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(Singles_per_game = (H - HR - X2B - X3B)/G, R_per_game = R/G) %>% 
  summarise(slope = get_slope(Singles_per_game, R_per_game))

# correlation btw HR, BB and singles
Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(singles = (H - HR - X2B - X3B)/G, BB = BB/G, HR = HR/G) %>% 
  summarise(cor(singles, BB), cor(BB, HR), cor(singles, HR))

# stratify BB by HR
dat <- Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(HR_strata = round(HR/G, 1),
         BB_per_game = BB/G,
         R_per_game = R/G) %>% 
  filter(HR_strata >= .4 & HR_strata <= 1.2)

# plot each strata
dat %>% ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm") +
  facet_wrap(~ HR_strata)

# calculate slope for each HR strata
dat %>% group_by(HR_strata) %>% 
  summarise(slope = get_slope(BB_per_game, R_per_game))

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR/G, R_per_game = R/G, BB_per_game = BB/G) %>%
  lm(R_per_game ~ BB_per_game + HR_per_game, data = .)

# Practice ####
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>% 
  summarise(mean_singles = mean(singles), mean_bb = mean(bb))

bat99_02 <- bat99_01 %>% inner_join(bat_02)
cor(bat99_02$singles, bat99_02$mean_singles)
cor(bat99_02$bb, bat99_02$mean_bb)

bat99_02 %>% ggplot(aes(singles, mean_singles)) + geom_point()
bat99_02 %>% ggplot(aes(bb, mean_bb)) + geom_point()

bat99_02 %>% lm(singles ~ mean_singles, data = .)
bat99_02 %>% lm(bb ~ mean_bb, data = .)

# Linear regression & the tidyverse ####
library(broom)

dat %>% group_by(HR_strata) %>% 
  do(fit = lm(R ~ BB, data = .))

# using data from 1961:2001 to fit linear model
fit <- Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G,
         singles = (H - X2B - X3B - HR)/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR = HR/G,
         R = R/G) %>% 
  lm(R ~ BB + singles + doubles + triples + HR, data = .)

# plot predicted vs actual for 2002
Teams %>% filter(yearID == 2002) %>% 
  mutate(BB = BB/G,
         singles = (H - X2B - X3B - HR)/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR = HR/G,
         R = R/G) %>% 
  mutate(R_hat = predict(fit, newdata = .)) %>% 
  ggplot(aes(R_hat, R, label = teamID)) +
  geom_point() +
  geom_text(nudge_x = .1, cex =2) +
  geom_abline()

pa_per_game <- Batting %>% filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarise(pa_per_game = sum(AB + BB)/max(G)) %>% 
  pull(pa_per_game) %>% 
  mean

# Practice ####
library(Lahman)
library(broom)

Teams %>% filter(yearID == 1971) %>% 
  mutate(BB = BB/G, HR = HR/G, R = R/G) %>% 
  lm(R ~ BB + HR, data = .) %>% tidy()

Teams %>% filter(yearID %in% 1961:2018) %>% 
  group_by(yearID) %>% 
  do(tidy(lm(R ~ BB + HR, data = .), conf.int = T)) %>% 
  filter(term == "BB") %>% 
  lm(estimate ~ yearID, data = .) %>% tidy()

# game attendance ####
library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

Teams_small %>% mutate(R = R/G) %>% 
  lm(avg_attendance ~ R, data = .) %>% tidy()

Teams_small %>% mutate(HR = HR/G) %>% 
  lm(avg_attendance ~ HR, data = .) %>% tidy()

Teams_small %>% lm(avg_attendance ~ W, data = .) %>% tidy()
Teams_small %>% lm(avg_attendance ~ yearID, data = .) %>% tidy()

Teams_small %>% summarise(cor(R/G, W), cor(W, HR/G))

w_stra <- Teams_small %>% mutate(W_strata = round(W/10)) %>% 
  filter(between(W_strata, 5, 10)) %>% 
  group_by(W_strata) %>% filter(n() >= 20) %>% 
  ungroup()

w_stra %>% filter(W_strata == 8) %>% nrow()

w_stra %>% mutate(R = R/G) %>% 
  group_by(W_strata) %>% 
  do(tidy(lm(avg_attendance ~ R, data = .))) %>% 
  filter(term == "R") %>% 
  arrange(estimate)

w_stra %>% mutate(HR = HR/G) %>% 
  group_by(W_strata) %>% 
  do(tidy(lm(avg_attendance ~ HR, data = .))) %>% 
  filter(term == "HR") %>% 
  arrange(estimate)

fit <- Teams_small %>% mutate(R = R/G, HR = HR/G) %>% 
  lm(avg_attendance ~ R + HR + W + yearID, data = .)

sce1 <- tibble(R = 5, HR = 1.2, W = 80, yearID = 2002)
sce2 <- tibble(R = 5, HR = 1.2, W = 80, yearID = 1960)
predict(fit, newdata = sce1)
predict(fit, newdata = sce2)

Teams %>% filter(yearID == 2002) %>% 
  mutate(R = R/G, HR = HR/G, avg_att_actual = attendance/G) %>% 
  mutate(avg_att_est = predict(fit, newdata = .)) %>% 
  summarise(cor(avg_att_est, avg_att_actual))
