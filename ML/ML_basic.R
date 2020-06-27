library(tidyverse)
library(caret)
library(ggrepel)

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

cutoff <- c(50, seq(61, 70), 80)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

# plot ROC curve of guessing and cutoff method
probs <- seq(0, 1, length.out = 10) %>% round(1)
n <- length(test_index)
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), n, replace = T, prob = c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       value = p,
       FPR = 1 - specificity(data = y_hat, reference = test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

cutoff_o <- map_df(cutoff, function(c){
  y_hat <- ifelse(test_set$height > c, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       value = c,
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

guessing %>% union(cutoff_o) %>% 
  ggplot(aes(FPR, TPR, col = method))+
  geom_line()+
  geom_point()+
  geom_text_repel(aes(label = value), nudge_x = 0.03, nudge_y = -0.01)

# plot precision-recall
guessing %>% union(cutoff_o) %>% 
  ggplot(aes(TPR, precision, col = method))+
  geom_line()+
  geom_point()+
  geom_text_repel(aes(label = value), nudge_x = 0.03, nudge_y = -0.01)+
  xlab("recall")

# plot precision-recall in case Y=1 is "Male"
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), n, replace = T, prob = c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guessing",
       value = p,
       FPR = 1 - specificity(y_hat, relevel(test_set$sex, "Male", "Female")),
       TPR = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

cutoff_o <- map_df(cutoff, function(c){
  y_hat <- ifelse(test_set$height > c, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       value = c,
       FPR = 1 - specificity(y_hat, relevel(test_set$sex, "Male", "Female")),
       TPR = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

bind_rows(guessing, cutoff_o) %>%
  ggplot(aes(TPR, precision, col = method, label = value))+
  geom_line()+
  geom_point()+
  geom_text_repel(nudge_x = .01, nudge_y = -.01)+
  xlab('recall')
