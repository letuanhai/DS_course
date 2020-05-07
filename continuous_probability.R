library(tidyverse)
library(dslabs)
data("heights")

  
x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()

# Monte Carlo simulation of tallest person over 7 feet
x <- heights %>% filter(sex == "Male") %>% pull(height)
n <- length(x)
avg <- mean(x)
s <- sd(x)

B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s)
  max(simulated_data)
})

mean(tallest>7*12)

# ACT scores
set.seed(16, sample.kind = "Rounding")
act_scores <- rnorm(10000, 20.9, 5.7)
mean(act_scores)
sd(act_scores)
sum(act_scores>=36)
mean(act_scores >= 30)
mean(act_scores <= 10)

x <- 1:36
f_x <- dnorm(x, 20.9, 5.7)
plot(x, f_x, type = "l")

z_scores <- (act_scores - 20.9) / 5.7
mean(z_scores >= 2)
mean(z_scores == 2)

F <- function(a){
  mean(act_scores <= a)
}
scores_prop <- sapply(x, F)

p <- seq(0.01, 0.99, 0.01)
sample_quantile <- quantile(act_scores, p)

theoretical_quantile <- qnorm(p, 20.9, 5.7)

qqplot(theoretical_quantile, sample_quantile)
