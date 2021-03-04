# Heatmaps and Dendograms

set.seed(12345)

# Display initial image with image(...)
par(mar = rep(0.2, 4))
data_Matrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(data_Matrix)[, nrow(data_Matrix):1])

# Display with heatmap(...) to get our Dendogram
par(mar = rep(0.2, 4))
heatmap(data_Matrix)

# using rbinom to generate a pattern
set.seed(678910)
for (i in 1:40) {
  coin_Flip <- rbinom(1, size = 1, prob = 0.5)
  if(coin_Flip) {
    data_Matrix[i, ] <- data_Matrix[i, ] + rep(c(0, 3), each = 5)
  }
}
par(mar=rep(0.2, 4))
image(1:10, 1:40, t(data_Matrix)[, nrow(data_Matrix):1])

par(mar=rep(0.2, 4))
heatmap(data_Matrix)

#Analyzing the patterns
hh <- hclust(dist(data_Matrix))
data_Matrix_Ordered <- data_Matrix[hh$order, ]
par(mfrow = c(1, 3))
image(1:10, 1:40, t(data_Matrix)[, nrow(data_Matrix):1])
plot(rowMeans(data_Matrix_Ordered), 40:1, xlab = "The Row Mean", ylab = "Row", pch=19)
plot(colMeans(data_Matrix_Ordered), xlab = "Column", ylab = "Column Mean", pch = 19)