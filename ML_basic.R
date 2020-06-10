library(tidyverse)
library(caret)

library(dslabs)
data(heights)

y <- heights$sex
x <- heights$height

set.seed(2007, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p = .5, list = F)
test_set <- heights[test_index,]
train_set <- heights[-test_index,]

# simplest algorithm: guess the outcome
y_hat <- sample(c("Male", "Female"), length(test_index), replace = T) %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)  # overall accuracy

# EDA suggest that males are typically taller
heights %>% group_by(sex) %>% summarise(mean(height), sd(height))

# try another simple approach
y_hat <- ifelse(x > 62, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)  # overall accuracy

cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
qplot(cutoff, accuracy)
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)
