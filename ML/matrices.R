library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()

x <- mnist$train$images[1:1000,] 
y <- mnist$train$labels[1:1000]

grid <- matrix(x[3,], 28, 28)
image(1:28, 1:28, grid)
image(grid[,28:1])
sum <- rowSums(x)  # matrix-specific functions
avg <- rowMeans(x)
tibble(labels = as.factor(y), row_avg = avg) %>% 
  qplot(labels, row_avg, data = ., geom = "boxplot")

# we can also use apply() with more flexibility (but less speed)
avgs <- apply(x, 1, mean)
sds <- apply(x, 2, sd)

# Filtering columns based on summaries ####
library(matrixStats)
sds <- colSds(x)

qplot(sds, bin = "30", color = I("black"))
image(1:28, 1:28, matrix(sds, 28, 28)[,28:1])

new_x <- x[, colSds(x) > 60]
dim(new_x)

# if you select one column or one row, the result is no longer a matrix but a vector
class(x[,1])
class(x[1,])
dim(x[1,])
# we can preserve the matrix class by using the argument drop=FALSE
class(x[, 1, drop=F])
dim(x[1,,drop=F])

# Indexing with matrices
qplot(as.vector(x), bins = 30, color = I("black"))

new_x <- x
new_x[new_x < 50] <- 0

# Binarizing the data
bin_x <- x
bin_x[bin_x < 255/2] <- 0 
bin_x[bin_x > 255/2] <- 1

# another way: convert to a matrix of logicals and then coerce to numbers
bin_X <- (x > 255/2)*1
