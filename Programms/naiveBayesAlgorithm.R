#reading data
data=read.csv("C:/Users/Reliance Digital/Downloads/diabetescsv/diabetes.csv")

#Setting outcome variables as categorical
data$Outcome <- factor(data$Outcome, levels = c(0,1), labels = c("False", "True"))


#Studying the structure of the data
str(data)
head(data)

install.packages("psych")
library(psych)

describe(data)

#Convert '0' values into NA
data[, 2:7][data[, 2:7] == 0] <- NA

#visualize the missing data
install.packages('Amelia')
library(Amelia)

missmap(data)


#Use mice package to predict missing values
install.packages('mice')
library(mice)

mice_mod <- mice(data[, c("Glucose","BloodPressure","SkinThickness","Insulin","BMI")], method='rf')
mice_complete <- complete(mice_mod)


#Transfer the predicted missing values into the main data set
data$Glucose <- mice_complete$Glucose
data$BloodPressure <- mice_complete$BloodPressure
data$SkinThickness <- mice_complete$SkinThickness
data$Insulin<- mice_complete$Insulin
data$BMI <- mice_complete$BMI

#To check if there are still any missing values, let's use the missmap plot
missmap(data)


#Data Visualization
#Visual 1
ggplot(data, aes(Age, colour = Outcome)) +
  geom_freqpoly(binwidth = 1) + labs(title="Age Distribution by Outcome")

#visual 2
c <- ggplot(data, aes(x=Pregnancies, fill=Outcome, color=Outcome)) +
  geom_histogram(binwidth = 1) + labs(title="Pregnancy Distribution by Outcome")
c + theme_bw()

#visual 3
P <- ggplot(data, aes(x=BMI, fill=Outcome, color=Outcome)) +
  geom_histogram(binwidth = 1) + labs(title="BMI Distribution by Outcome")
P + theme_bw()

#visual 4
ggplot(data, aes(Glucose, colour = Outcome)) +
  geom_freqpoly(binwidth = 1) + labs(title="Glucose Distribution by Outcome")

#visual 
remove.packages("ggplot2") # Unisntall ggplot
install.packages("ggplot2") # Install it again
library(ggplot2)
install.packages("GGally")
library(GGally)
ggpairs(data)


#Building a model
#split data into training and test data sets
library(caret)
indxTrain <- createDataPartition(y = data$Outcome,p = 0.75,list = FALSE)
training <- data[indxTrain,]
testing <- data[-indxTrain,]


#Check dimensions of the split
prop.table(table(data$Outcome)) * 100
prop.table(table(training$Outcome)) * 100
prop.table(table(testing$Outcome)) * 100


#create objects x which holds the predictor variables and y which holds the response variables
x = training[,-9]
y = training$Outcome

library(e1071) #e1071 package that holds the Naive Bayes function
#create Naive Bayes model by using the training data set
#it will say klaR package is required type yes to install it
model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))

model

#Model Evaluation
#Predict testing set
Predict <- predict(model,newdata = testing )

#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(Predict, testing$Outcome )

