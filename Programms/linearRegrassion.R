#import cars data
data=read.csv("C:/Users/Reliance Digital/Downloads/cars.csv")

head(data)

#Visualising Linearity using Scatterplots
scatter.smooth(x=data$speed, y = data$dist, main="Dist ~ Speed")

#Measuring Correlation Coefficient
cor(data$speed, data$dist) #Finding Correlation between speed and distance


#Building the Linear Model
linear_model <- lm(dist~speed, data = data)
print(linear_model)

#Diagnosing the Linear Model
summary(linear_model)

#Calculating Standard Error and F - statistic
Model_Summary <- summary(linear_model)
Model_Coefficients <- Model_Summary$coefficients
std_error <- Model_Coefficients["speed", "Std. Error"]
print(std_error)
