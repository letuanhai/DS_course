options(digits = 3)
library(tidyverse)
library(titanic)
library(gridExtra)


titanic <- titanic_train %>% 
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>% 
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

titanic %>% filter(!is.na(Age)) %>% 
  ggplot(aes(Age, fill = Sex)) +
  geom_density(aes(y = ..count..), alpha = 0.2)

titanic %>% filter(!is.na(Age)) %>% 
  ggplot(aes(Age, Sex)) +
  geom_point()

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic %>% filter(!is.na(Age)) %>% 
  ggplot(aes(sample = Age)) +
  geom_qq(dparams = params) +
  geom_abline()

titanic %>% filter(!is.na(Survived)) %>% 
  ggplot(aes(Survived, fill = Sex)) +
  geom_bar(position = position_dodge())

titanic %>% filter(!is.na(Age), !is.na(Survived)) %>% 
  ggplot() +
  geom_density(aes(Age, y = ..count.., fill = Survived), alpha = 0.2)

titanic %>% filter(!is.na(Survived), !is.na(Fare), Fare > 0) %>% 
  ggplot(aes(Survived, Fare)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log2") +
  geom_jitter(alpha = 0.2)

b1 <- titanic %>% ggplot(aes(Pclass, fill = Survived))+
  geom_bar()

b2 <- titanic %>% ggplot(aes(Pclass, fill = Survived))+
  geom_bar(position = position_fill())

b3 <- titanic %>% ggplot(aes(Survived,fill = Pclass))+
  geom_bar(position = position_fill())

grid.arrange(b1, b2, b3, ncol = 3)

titanic %>% filter(!is.na(Age)) %>% 
  ggplot()+
  geom_density(aes(Age, y = ..count.., fill = Survived), alpha = 0.2) +
  facet_grid(Sex ~ Pclass)
