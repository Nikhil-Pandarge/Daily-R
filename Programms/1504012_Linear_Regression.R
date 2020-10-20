data(cars)
head(cars)
scatter.smooth(x=cars$speed,y=cars$dist,main='dist~speed')

par(mfrow=c(1,2))
boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ",boxplot.stats(cars$speed)$out)) 

boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))

library(e1071)

par(mfrow=c(1, 2))  # divide graph area in 2 columns

plot(density(cars$speed), main="Density Plot: Speed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(cars$speed), col="orange")

plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  # density plot for 'dist'
polygon(density(cars$dist), col="orange")

cor(cars$speed,cars$dist)

linearMod <- lm(dist ~ speed, data=cars)
print(linearMod)

summary(linearMod)

modelSummary <- summary(linearMod)

modelCoeffs <- modelSummary$coefficients

beta.estimate <- modelCoeffs["speed","Estimate"]


std.error <- modelCoeffs["speed", "Std. Error"]  # get std.error for speed

t_value <- beta.estimate/std.error  # calc t statistic

p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value

f_statistic <- linearMod$fstatistic[1]  # fstatistic

f <- summary(linearMod)$fstatistic  # parameters for model p-value calc

model_p <- pf(f[1], f[2], f[3], lower=FALSE)

t_value

AIC(linearMod)

BIC(linearMod)
set.seed(100)
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))
# row indices for training data
trainingData <- cars[trainingRowIndex, ]  # model training data
testData  <- cars[-trainingRowIndex, ]   # test data
lmMod <- lm(dist ~ speed, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance
summary(lmMod)
AIC(lmMod)
actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))
correlation_accuracy <- cor(actuals_preds) 
correlation_accuracy
head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)

library(DAAG)
cvResults<-suppressWarnings(CVlm(data=cars, form.lm=dist ~ speed, m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."))  

