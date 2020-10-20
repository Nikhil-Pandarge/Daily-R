library(ISwR)
plot(rnorm(100))
rnorm(100)
exp(3.14)
exp(1)
NIKHIL <- 23
nikhil <-23
NIKHIL == nikhil
weight <- c(20,30,40)

height <- c(1.72,1.60,1.50,1.70)
weight <- c(86,60,50,90)
BMI <- weight/height^2
BMI

xbar <- sum(weight)/length(weight)

sqrt(sum((weight - xbar)^2)/(length(weight)-1))
sd(weight)
t.test(BMI,mu=22.5)

plot(height,weight,pch=22)

hh <- c(1.65, 1.70, 1.75, 1.80, 1.85, 1.90)

lines(hh,22.5 * hh^2)

ls()
ls

plot
install.packages("tidyverse")
library(ggplot2)
library(readr)
library(purrr)
library(tibble)  
as_tibble(iris)

install.packages("installr")
library(installr)
updateR()

data("mtcars")
mtcars
data("Bodyfat")
data("bodyfat", package="TH.data")
head(bodyfat)
summary(Carseats)
data(carseats)
data()
Car_Seats <- read.csv("Carseats.csv")
Car_Seats
str(iris)
str(bodyfat)


install.packages("rpart")
install.packages("rpart.plot")
library("rpart")
library("rpart.plot")
data("iris")


str(iris)


indexes = sample(150, 110)
iris_train = iris[indexes,]
iris_test = iris[-indexes,]

target = sum(complete.cases(iris))

# target = Sepal.Length ~.iris

tree = rpart(target, data = iris_train, method = "class")
rpart.plot(tree)

table(iris$Sepal.Length,iris$Petal.Length)
library(installr)
