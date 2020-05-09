
library(ISLR)

Carseats$HighSales <- ifelse(test = Carseats$Sales > 9.32,yes = 'Yes',no = 'No')
Carseats$HighSales <- as.factor(Carseats$HighSales)

Carseats <- Carseats[,-1]
str(Carseats)

## 'data.frame': 400 obs. of 11 variables:

num.vars <- c(1:5,7,8)
apply(X = Carseats[,num.vars], MARGIN = 2, FUN = shapiro.test)

## $CompPrice

library(bnlearn)

to.discretize <- c("Education", "Age", "Population", "Advertising", "Income")
#discretized <- discretize(data = Carseats[,to.discretize],

summary(Carseats$Advertising)
## Min. 1st Qu. Median Mean 3rd Qu. Max.
## 0.000 0.000 5.000 6.635 12.000 29.000

library(ggplot2)
ggplot(data = Carseats, mapping = aes(x = Advertising)) +
  geom_histogram(bins = 30)

discretized <- discretize(data = Carseats[,to.discretize],
                          method = 'quantile',
                          breaks = c(5,5,5,2,5))
summary(discretized)
## Education Age Population Advertising Income

cols.to.add <- setdiff(names(Carseats), names(discretized))
carseats.new <- data.frame(cbind(Carseats[,cols.to.add], discretized))
str(carseats.new)
## 'data.frame': 400 obs. of 11 variables:
## $ CompPrice : num 138 111 113 117 141 124 115

carseats.new <- carseats.new[,names(Carseats)]
str(carseats.new)
## 'data.frame': 400 obs. of 11 variables:
## $ CompPrice : num 138 111 113 117 141 124 115 136 132 132

library(lattice)
library(caret)

set.seed(1010)
train.indices <- createDataPartition(carseats.new$HighSales, p = 0.8, list = FALSE)
train.data <- carseats.new[train.indices,]
test.data <- carseats.new[-train.indices,]

library(e1071)

nb1 <- naiveBayes(HighSales ~ ., data = train.data)
print(nb1)
##
## Naive Bayes Classifier for Discrete Predictors

nb1.pred <- predict(nb1, newdata = test.data, type = 'class')
head(nb1.pred)
## [1] Yes No No Yes Yes No
## Levels: No Yes

nb1.cm <- table(true = test.data$HighSales, predicted = nb1.pred)
nb1.cm
## predicted
## true No Yes
## No 56 4
## Yes 10 9

# function for computing evaluation metrix

compute.eval.metrics <- function(cmatrix) {
  TP <- cmatrix[1,1] # true positive
  TN <- cmatrix[2,2] # true negative
  FP <- cmatrix[2,1] # false positive
  FN <- cmatrix[1,2] # false negative
  acc = sum(diag(cmatrix)) / sum(cmatrix)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1 <- 2*precision*recall / (precision + recall)
  c(accuracy = acc, precision = precision, recall = recall, F1 = F1)
}

# source("EvaluationMeasures.R")
# compute the evaluation metrics
nb1.eval <- compute.eval.metrics(nb1.cm)
nb1.eval
## accuracy precision recall F1
## 0.8227848 0.8484848 0.9333333 0.8888889

nb2 <- naiveBayes(HighSales ~ ShelveLoc + Price + Advertising + Age + CompPrice,
                  data = train.data)
##Make predictions using the new model:
  
nb2.pred <- predict(nb2, newdata = test.data, type = 'class')
##Evaluate the new model by first creating the confusion matrix:
  
nb2.cm <- table(true = test.data$HighSales, predicted = nb2.pred)
nb2.cm
## predicted
## true No Yes
## No 59 1
## Yes 10 9

nb2.eval <- compute.eval.metrics(nb2.cm)
nb2.eval
## accuracy precision recall F1
## 0.8607595 0.8550725 0.9833333 0.9147287

data.frame(rbind(nb1.eval, nb2.eval), row.names = c("NB_mod 1", "NB_mod 2"))
## accuracy precision recall F1
## NB_mod 1 0.8227848 0.8484848 0.9333333 0.8888889
## NB_mod 2 0.8607595 0.8550725 0.9833333 0.9147287

nb2.pred.prob <- predict(nb2, newdata = test.data, type = "raw")
# note that the type parameter is now set to 'raw'
head(nb2.pred.prob)
## No Yes
## [1,] 0.1483420 0.8516580
## [2,] 0.9703448 0.0296552
## [3,] 0.8073813 0.1926187
## [4,] 0.5569032 0.4430968
## [5,] 0.3655349 0.6344651
## [6,] 0.6029657 0.3970343

##To create ROC curves, we'll use the pROC package. For more info about pROC, check:
##https://web.expasy.org/pROC/

library(pROC)

nb2.roc <- roc(response = as.numeric(test.data$HighSales),
               predictor = nb2.pred.prob[,1],
               levels = c(2, 1))

nb2.roc$auc
## Area under the curve: 0.9333

plot.roc(nb2.roc,
         print.thres = TRUE,
         print.thres.best.method = "youden")

nb2.coords <- coords(nb2.roc,ret = c("accuracy", "spec", "sens", "thr"),x = "local maximas")
nb2.coords

prob.threshold <- nb2.coords[4,5]

nb2.pred2 <- ifelse(test = nb2.pred.prob[,1] >= prob.threshold, yes = "No",       
                    no = "Yes") #... assign the negative class (Yes)
# if probability of the positive class (No) is greater than the chosen probability threshold ...
#... assign the positive class (No)

nb2.pred2 <- as.factor(nb2.pred2)

nb2.cm2 <- table(actual = test.data$HighSales, predicted = nb2.pred2)
nb2.cm2
## predicted
## actual No Yes
## No 50 10
## Yes 3 16
nb2.eval2 <- compute.eval.metrics(nb2.cm2)
nb2.eval2
## accuracy precision recall F1
## 0.8354430 0.9433962 0.8333333 0.88495580

data.frame(rbind(nb1.eval, nb2.eval, nb2.eval2),
           row.names = c(paste("NB_", 1:3, sep = "")))
## accuracy precision recall F1
## NB_1 0.8227848 0.8484848 0.9333333 0.8888889
## NB_2 0.8607595 0.8550725 0.9833333 0.9147287
















