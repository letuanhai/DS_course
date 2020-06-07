library(tidyverse)
library(dslabs)

# Spurious correlation ####
N <- 25
g <- 1000000
sim_data <- tibble(group = rep(1:g, each=N), 
                   x = rnorm(N * g), 
                   y = rnorm(N * g))

res <- sim_data %>% group_by(group) %>% 
  summarise(r = cor(x, y)) %>% 
  arrange(desc(r))

sim_data %>% filter(group == res$group[which.max(res$r)]) %>% 
  ggplot(aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm")

res %>% ggplot() + geom_histogram(aes(r), binwidth = .1, col = "black")

library(broom)
sim_data %>% 
  filter(group == res$group[which.max(res$r)]) %>% 
  do(tidy(lm(y ~ x, data = .))) %>% 
  filter(term == "x")
  
# Outliers ####
set.seed(1985, sample.kind = "Rounding")

x <- rnorm(100, 100, 1)
y <- rnorm(100, 84, 1)

x[-23] <- scale(x[-23])
y[-23] <- scale(y[-23])

qplot(x, y)

cor(x, y)

cor(x[-23], y[-23])

# Spearman correlation: compute the correlation on the ranks of the values
qplot(rank(x), rank(y))
cor(rank(x), rank(y))
cor(x, y, method = "spearman")

# Confounders - Simpson's paradox ####
data(admissions)

# using chisq test show that there seems to be gender bias in general
admissions %>% group_by(gender) %>% 
  summarize(total_admitted = round(sum(admitted / 100 * applicants)), 
            not_admitted = sum(applicants) - sum(total_admitted)) %>%
  select(-gender) %>% 
  do(tidy(chisq.test(.))) %>% .$p.value

# inspecting admission rate by major show a different view
admissions %>% select(major, gender, admitted) %>% 
  spread(gender, admitted) %>% 
  mutate(w_minus_m = women - men)
  
# The paradox is that analyzing the totals suggests a dependence between admission and gender, but when the data is grouped by major, this dependence seems to disappear. What’s going on? This actually can happen if an uncounted confounder is driving most of the variability.

# plot the total percent admitted to a major versus the percent of women that made up the applicants
admissions %>% group_by(major) %>% 
  summarize(major_selectivity = sum(admitted * applicants)/sum(applicants),
            percent_women_applicants = sum(applicants * (gender=="women")) /
              sum(applicants) * 100) %>%
  ggplot(aes(major_selectivity, percent_women_applicants, label = major)) +
  geom_text()
  
admissions %>%
  mutate(yes = admitted/100 * applicants,
         no = applicants - yes) %>% 
  gather(admission, number_of_students, c(`yes`,`no`)) %>% 
  ggplot(aes(gender, number_of_students, fill = admission)) +
  geom_col(position = "stack")+
  facet_wrap(. ~ major)
