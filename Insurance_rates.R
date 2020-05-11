library(dslabs)
library(tidyverse)

data("death_prob")
loss_per_death <- -150e3
gain_per_contract <- 1150

# 50 yrs old female population
p <- death_prob %>% filter(sex=="Female", age=="50") %>% pull(prob)
n <- 1000

expected <- loss_per_death*p + gain_per_contract*(1-p)
se_1 <- (1150 + 150e3) * sqrt(p * (1 - p))

se <- se_1 * sqrt(n)
avg <- expected * n
pnorm(0, avg, se)

# 50 yrs old male population
p <- death_prob %>% filter(sex=="Male", age=="50") %>% pull(prob)
n <- 1000
premium <- (7e5/n - loss_per_death*p) / (1 - p)

se <- abs(loss_per_death - premium) * sqrt(n * p * (1 - p))
avg <- n * (p * loss_per_death + (1 - p) * premium)

pnorm(0, avg, se)


# pandemic striked
p <- .015
n <- 1000

expected <- loss_per_death*p + gain_per_contract*(1-p)
avg <- n * expected
se <- abs(loss_per_death - gain_per_contract) * sqrt(n * p * (1 - p))

pnorm(-1e6, avg, se)

p <- seq(.01, .03, .0025)
y <- pnorm(-1e6, avg, se)
data.frame(p, y * 100)

# sampling models
set.seed(27, sample.kind = "Rounding")
p_loss <- 0.015
loss_on_claim <- -150e3
profit_no_claim <- 1150
n <- 1000
B <- 10000

S <- replicate(B, {
  X <- sample(c(loss_on_claim, profit_no_claim), n, prob = c(p_loss, 1 - p_loss), replace = TRUE)
  sum(X)
})
mean(S<= -1e6)

# Define premium
p <- .015
n <- 1000
loss_on_claim <- -150e3
z <- qnorm(.05)

x <- -loss_on_claim*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
expected <- loss_on_claim * p + x * (1 - p)
avg <- expected * n

# Monte Carlo simulation
set.seed(28, sample.kind = "Rounding")
B <- 10000

S <- replicate(B, {
  X <- sample(c(loss_on_claim, x), n, prob = c(p, 1 - p), replace = TRUE)
  sum(X)
})
mean(S <= 0)

# Random pandemic death rate
set.seed(29, sample.kind = "Rounding")
B <- 10000
p <- .015
 
S <- replicate(B, {
  p <- p + sample(seq(-.01, .01, len = 100), 1)
  X <- sample(c(loss_on_claim, x), n, prob = c(p, 1 - p), replace = TRUE)
  sum(X)
})
mean(S < -1e6)
