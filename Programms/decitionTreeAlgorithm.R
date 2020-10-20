#install required packages
install.packages("caret")
install.packages("rpart.plot")

#use those packages
library(caret)
library(rpart.plot)

#read downloaded file
car_df = read.csv("C:/Users/Reliance Digital/Downloads/car.data", sep = ',', header = FALSE)

#For checking the structure of data frame 
str(car_df)

#To check top 5-6 rows of the dataset
head(car_df)

#Data Slicing
set.seed(3033)
intrain <- createDataPartition(y = car_df$V7, p= 0.7, list = FALSE)
training <- car_df[intrain,]
testing <- car_df[-intrain,]

#For checking the dimensions of our training data frame and testing data frame, we use these:
dim(training); dim(testing);

#Preprocessing & Training
anyNA(car_df)

#Dataset summarized details
summary(car_df)

#Training the Decision Tree classifier with criterion as information gain
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)


install.packages("e1071")
library(e1071)
#above package is requred 
dtree_fit <- train(V7 ~., data = training, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)

#Plot Decision Tree
prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)

#Prediction
testing[1,]
predict(dtree_fit, newdata = testing[1,])
test_pred <- predict(dtree_fit, newdata = testing)
confusionMatrix(test_pred, testing$V7 )  #check accuracy

#Training the Decision Tree classifier with criterion as gini index
set.seed(3333)
dtree_fit_gini <- train(V7 ~., data = training, method = "rpart",
                        parms = list(split = "gini"),
                        trControl=trctrl,
                        tuneLength = 10)

#Plot Decision Tree        
prp(dtree_fit_gini$finalModel, box.palette = "Blues", tweak = 1.2)

#Prediction
test_pred_gini <- predict(dtree_fit_gini, newdata = testing)
confusionMatrix(test_pred_gini, testing$V7 )  #check accuracy
