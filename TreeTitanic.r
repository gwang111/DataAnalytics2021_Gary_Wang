data(titanic)

library(tree)
require(rpart)
require(party)
require(randomForest)

titanic_rpart <- rpart(Survived ~ Age + Fare + Pclass, data=titanic)
plot(titanic_rpart)

tree_titanic <- ctree(Survived ~ ., data=titanic)
plot(tree_titanic)

d <- dist(titanic, method="euclidean")
clust_titanic <- hclust(d, method = "complete")
plot(clust_titanic)

fit_titanic <- randomForest(Survived ~ Age + Fare + Pclass, data=titanic)
print(fit_titanic)
importance(fit_titanic)
varImpPlot(fit_titanic)
