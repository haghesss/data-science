#Simple Linear Regression
#data of height
x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
#data of weight
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)
# Apply the lm() function
relation <- lm(y~x)
print(relation)

#predict
# Find weight of a person with height 170
x_test <- data.frame(x = 189)
result <- predict(relation,x_test)
print(round(result, digit=2))

# Plot the chart
plot(x,y,col = "blue", main = "Height & Weight Regression", abline(lm(y~x)),pch = 16,xlab = "Height in cm", ylab = "Weight in Kg")


x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)

#data of weight
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

#Creating data frame
data1= data.frame(x,y)
#splitting data into training and testing
data1_train<-data1[1:7,]
data1_test<-data1[8:10,]

# Apply the lm() function
relation <- lm(y~x, data1_train)
print(relation)


#Make prediction
x_text <- data.frame(x= data1_test$x)
result <- predict(relation,x_text)
print(result)

mape <- mean(abs((data1_test$y -result)/data1_test$y)*100)
paste("The error - MAPE is: ", round(mape,digit=2),"%")

#Performance measurement [Option 2] Create a data frame to combine the actuals and predicted values
actuals_preds <- data.frame(cbind(actuals=data1_test$y, predicteds=result))

mape <- mean(abs(actuals_preds$actuals - actuals_preds$predicteds )/ actuals_preds$actuals)*100
paste("The error - MAPE is: ", round(mape,digit=2),"%")


df<-read.csv("C:/Users/rayan/Downloads/dsClass/linear regression/income_happiness.csv")

#Split data into training (80%) and testing (20%) sets
#Randomly select row indices for training
train_indices <- sample(1:nrow(df), size = 0.8 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# Apply the lm() function
relation <- lm(happiness~income, data=train_data)
print(relation)

# Prediction
a <- data.frame(x=test_data$income)
colnames(a) <- "income"
result <- predict(relation,a)

#Plot
plot(test_data$income,test_data$happiness,col="red", abline(lm(happiness~income, data=train_data)),pch = 16,xlab = "income",ylab = "happiness")
