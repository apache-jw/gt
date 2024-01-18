
rm(list = ls())

library(tidyverse)

gtcars <- readr::read_csv(file = "gtcars.csv")

summary(gtcars)

df1 = data.frame(gtcars,check.rows = T)
## linear regression to predict msrp from hp mpg_h and mpg_c
#lm1 = lm(msrp ~ hp + mpg_h + mpg_c, data = df1)
#summary(lm1)

## plot residuals
#plot(lm1, which = 1)


## classification on drivetrain
library(caret)

# Convert drivetrain to a factor variable
df1$drivetrain <- as.factor(df1$drivetrain)

# Add trainIndex onto dataframe
df1$trainIndex <- df1$trainIndex

# Split the data into training and testing sets
## column where drivetrain = awd
df1$trainIndex <- ifelse(df1$drivetrain == "awd", 1, 0)

## where drivetrain = rwd
#df1$trainIndex <- ifelse(df1$drivetrain == "rwd", 1, 0)

set.seed(123)
train_index <- createDataPartition(df1$drivetrain, p = 0.7, list = FALSE)
train_data <- df1[train_index, ]
test_data <- df1[-train_index, ]

# Train a classification model using random forest
rf_model <- train(drivetrain ~ ., data = train_data, method = "rf")

# Predict drivetrain using the trained model
predictions <- predict(rf_model, newdata = test_data)
predictions

# Evaluate the model performance
confusionMatrix(predictions, test_data$drivetrain)
