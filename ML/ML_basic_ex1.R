# Exercise 1 ####

library(dslabs)
library(dplyr)
library(lubridate)
library(caret)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

# explore proportion of gender by type
dat %>% filter(type == "inclass") %>% with(mean(sex == "Female"))
dat %>% filter(type == "online") %>% with(mean(sex == "Female"))

# simple prediction using type
y_hat <- if_else(x == "inclass", "Female", "Male") %>% factor(levels = c("Female", "Male"))
mean(y_hat == y)

# build confusion matrix
table(predict = y_hat, actual = y)

sensitivity(y_hat, reference = y)
specificity(y_hat, y)

mean(y == "Female")

# Exercise 2 ####
library(caret)
library(tidyverse)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p = .5, list = F)
test <- iris[test_index,]
train <- iris[-test_index,]

# cutoff of 1 feature
# optimize using train set
features <- names(train)[-length(train)]
cutoffs <- map_df(features, function(f){
  cutoff <- seq(min(train[f]), max(train[f]), by = .1)
  accuracy <- sapply(cutoff, function(cutoff){
    y_hat <- if_else(train[f] > cutoff, "virginica", "versicolor") %>% 
      factor(levels = c("virginica", "versicolor", "setosa"))
    mean(y_hat == train$Species)
  })
  list(feature = f,
       value = cutoff[which.max(accuracy)],
       accuracy = max(accuracy))
})
cutoffs %>% arrange(accuracy)

# apply to test set
cutoff <- 4.7
y_hat <- ifelse(test["Petal.Length"] > cutoff, "virginica", "versicolor") %>% 
  factor(levels = c("virginica", "versicolor", "setosa"))
mean(y_hat == test$Species)

# optimize using test set
test_accuracy <- map_df(features, function(f){
  cutoff <- seq(min(test[f]), max(test[f]), by = .1)
  accuracy <- sapply(cutoff, function(cutoff){
    y_hat <- if_else(test[f] > cutoff, "virginica", "versicolor") %>% 
      factor(levels = c("virginica", "versicolor", "setosa"))
    mean(y_hat == test$Species)
  })
  list(feature = f,
       value = cutoff[which.max(accuracy)],
       accuracy = max(accuracy))
})
test_accuracy %>% arrange(desc(accuracy))

# EDA
plot(iris,pch=21,bg=iris$Species)

# cutoffs of 2 features
# optimize using train set
cutoff_l <- seq(min(train["Petal.Length"]), max(train["Petal.Length"]), by = .1)
cutoff_w <- seq(min(train["Petal.Width"]), max(train["Petal.Width"]), by = .1)
cutoff <- expand_grid(length = cutoff_l, width = cutoff_w)

predict_a <- function(l, w){
  y_hat <- ifelse(train$Petal.Length > l | train$Petal.Width > w, "virginica", "versicolor") %>% 
      factor(levels = c("virginica", "versicolor", "setosa"))
  mean(y_hat == train$Species)
}

outcome <- mapply(predict_a, cutoff$length, cutoff$width)
select_cutoff <- cutoff[which.max(outcome),]

# apply to test set
y_hat <- ifelse(test$Petal.Length > select_cutoff$length | test$Petal.Width > select_cutoff$width
                , "virginica", "versicolor") %>% 
    factor(levels = c("virginica", "versicolor", "setosa"))
mean(y_hat == test$Species)
