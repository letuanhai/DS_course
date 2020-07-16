library(tidyverse)
library(caret)

# build 100 linear models and calculate RMSE distribution
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1, sample.kind = "Rounding")
rmse <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = .5, list = F)
  train_set <- dat[-test_index,]
  test_set <- dat[test_index,]
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})
mean(rmse)
sd(rmse)

# repeat with larger datasets
test_func <- function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = .5, list = F)
    train_set <- dat[-test_index,]
    test_set <- dat[test_index,]
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata = test_set)
    sqrt(mean((y_hat - test_set$y)^2))
  })
  list(mean = mean(rmse), sd = sd(rmse))
}

n <- c(100, 500, 1000, 5000, 10000)
set.seed(1, sample.kind = "Rounding")
outcome <- sapply(n, test_func)

# repeat with larger correlation between variables
set.seed(1, sample.kind = "Rounding")
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
set.seed(1, sample.kind = "Rounding")
rmse <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = .5, list = F)
  train_set <- dat[-test_index,]
  test_set <- dat[test_index,]
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})
mean(rmse)
sd(rmse)

# new dataset with 2 dependent variables ####
set.seed(1, sample.kind = "Rounding")
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(dat$y, times = 1, p = .5, list = F)
test_set <- dat %>% slice(test_index)
train_set <- dat %>% slice(-test_index)

# using x_1
fit <- lm(y~x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
rmse_1 <- sqrt(mean((y_hat - test_set$y)^2))

# using x_2
fit <- lm(y~x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
rmse_2 <- sqrt(mean((y_hat - test_set$y)^2))

# using x_1 and x_2 
fit <- lm(y~x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
rmse_12 <- sqrt(mean((y_hat - test_set$y)^2))

# 2 highly correlated dependent variables ####
set.seed(1, sample.kind = "Rounding")
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(dat$y, times = 1, p = .5, list = F)
test_set <- dat %>% slice(test_index)
train_set <- dat %>% slice(-test_index)

# using x_1
fit <- lm(y~x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
rmse_1 <- sqrt(mean((y_hat - test_set$y)^2))

# using x_2
fit <- lm(y~x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
rmse_2 <- sqrt(mean((y_hat - test_set$y)^2))

# using x_1 and x_2 
fit <- lm(y~x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
rmse_12 <- sqrt(mean((y_hat - test_set$y)^2))
