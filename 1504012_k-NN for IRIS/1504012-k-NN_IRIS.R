# @authour Nikhil N Pandarge

# calling required packages

library(class)

library(ggplot2)

install.packages("GGally")
library(GGally)

install.packages("ggcorrplot")
library(ggcorrplot)

# data summary

data(iris)

summary(iris)

#------------------------
# Distribution of Variables

iris[,1:4] <- scale(iris[,1:4])

par(mfrow=c(2,2))

plot(density(iris$Sepal.Length), col=iris$Species)

plot(density(iris$Sepal.Width))

plot(density(iris$Petal.Length))

plot(density(iris$Petal.Width))

par(mfrow=c(2,2))

hist(iris$Sepal.Length, col="orange", breaks=20)

hist(iris$Sepal.Width, col="orange", breaks=20)

hist(iris$Petal.Length, col="orange", breaks=20)

hist(iris$Petal.Width, col="orange", breaks=20)

#-----------------------------
# Relationship between Variables:

ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species))+ geom_point() + geom_smooth(method="lm") + facet_grid(.~iris$Species)

ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width, col = Species)) + geom_point() + geom_smooth(method="lm") + facet_grid(.~iris$Species)

# use the correlation matrix to check the strength of correlation.
ggpairs(iris)

# Classification into Different Species using the K-Nearest Neighbor Method

set.seed(12366894)

setosa<- rbind(iris[iris$Species=="setosa",])

versicolor<- rbind(iris[iris$Species=="versicolor",])

virginica<- rbind(iris[iris$Species=="virginica",])

ind <- sample(1:nrow(setosa), nrow(setosa)*0.6)

iris.train<- rbind(setosa[ind,], versicolor[ind,], virginica[ind,])

iris.test<- rbind(setosa[-ind,], versicolor[-ind,], virginica[-ind,])

# Choosing the Value of K
#------------------------
error <- c()
for (i in 1:15)
{
  knn.fit <- knn(train = iris.train[,1:4], test = iris.test[,1:4], cl = iris.train$Species, k = i)
  error[i] = 1- mean(knn.fit == iris.test$Species)
}

ggplot(data = data.frame(error), aes(x = 1:15, y = error)) +
  geom_line(color = "orange")

# Prediction Accuracy using the chosen K value
#------------------------

set.seed(12366894)

iris_pred <- knn(train = iris.train[,1:4], test = iris.test[,1:4], cl = iris.train$Species, k=5)

table(iris.test$Species,iris_pred)
