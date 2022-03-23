# COMPETITION
# read data 
library(tidyverse)
library(elasticnet)
library(corrplot)
library(caret)
library(lattice)
library(pls)
library(e1071)
data<- read.csv("https://raw.githubusercontent.com/PittDataScience/CourseContent/master/Assignments/competition/competition-data.csv?token=GHSAT0AAAAAABSJ23EEZ74BPHO5546DNXMQYRSVYNQ")
test_data <- read.csv("https://raw.githubusercontent.com/PittDataScience/CourseContent/master/Assignments/competition/competition-test-x-values.csv?token=GHSAT0AAAAAABSJ23EFB5P55YBCQWUQQWKGYRSVZKA")

# Performing Exploratory data analysis and data pre processing to understand various variables.

#Getting Total Missing values 
total_na <- function(x) sum(is.na(x))

# getting a quick summary of the data set 
summary(data)

# Checking for null values in the data set 
df <- as.data.frame(
  cbind(
    lapply(
      lapply(data, is.na), sum)
  )
)

df
# the given data set is not having any null values
select(data)
# predictors_only <- select(data, -outcome)
# 
# 
# predictors_only1 <- data.frame(
#   predictors_only %>% select(where(is.numeric)))
# predictors_only1_correlation_matrix <- cor(predictors_only1)
# cutoff <- 0.95
# 
# names_of_predictors_to_remove <- findCorrelation(
#   predictors_only1_correlation_matrix, names=TRUE, cutoff=cutoff)
# removed_predictors <- predictors_only1 %>%
#   select(-all_of(names_of_predictors_to_remove))
# 
# 
# summary(removed_predictors)


# pre processing data set using PCA
data2<- data

df_pred <- data.frame(data2 %>%select(-outcome))


# Applying BoxCox transformation

df_preprocess_fit<- preProcess(
  df_pred, method = c("BoxCox", "center", "scale"))
df_preprocess_fit

transformed_predictors <- predict(
  df_preprocess_fit, df_pred)

# checking skewness of the outcome
skewness(data$outcome)
set.seed(123)
data_final <- add_column(transformed_predictors, outcome = data$outcome)


split = sample.split(data_final$outcome, SplitRatio = 0.8)
training_set = subset(data_final, split == TRUE)
test_set = subset(data_final, split == FALSE)

# Fitting Simple Linear Regression to the Training set
Linear_regression = lm( outcome ~ . ,
               data = training_set)

# Predicting the Test set results
y_pred = predict(Linear_regression, newdata = test_set)


#training error
sqrt(mean(Linear_regression$residuals^2))

# test error 

sqrt(mean((y_pred - test_set$outcome)^2))

# RMSE error 

Linear_reg_RMSE<-RMSE(y_pred, test_set$outcome)
Linear_reg_RMSE



# FIT 2

# Creating a regression model where the outcome is also transformed { Log is not used as valiues are 0 also}
center_and_scale <- function(data) { 
        return (data - mean(data)) / sd(data)
}
center_and_scale_outcome<- center_and_scale(data$outcome)

set.seed(123)
data_final_1 <- add_column(transformed_predictors, outcome = center_and_scale_outcome)

split_1 = sample.split(data_final_1$outcome, SplitRatio = 0.8)
training_set_1 = subset(data_final_1, split_1 == TRUE)
test_set_1 = subset(data_final_1, split_1 == FALSE)
#fitting the linaer regression
Linear_regression_1 = lm( outcome ~ . ,
                        data = training_set_1)
# Predicting the Test set results
y_pred_1 = predict(Linear_regression_1, newdata = test_set_1)


#training error
sqrt(mean(Linear_regression_1$residuals^2))

# test error 

sqrt(mean((y_pred_1 - test_set_1$outcome)^2))

# RMSE error 

Linear_reg_RMSE_1<-RMSE(y_pred_1, test_set_1$outcome)
Linear_reg_RMSE_1


#Applying  Linear regression with selective predictors only   - BEST FIT TILL NOW
set.seed(100)
split_2 = sample.split(data$outcome, SplitRatio = 0.8)
training_set_2 = subset(data, split_2 == TRUE)
test_set_2 = subset(data, split_2 == FALSE)

# # Feature Scaling
# training_set_2 = scale(training_set_2)
# test_set_2 = scale(test_set_2)
# training_set_2<- data.frame(training_set_2)
# test_set_2<- data.frame(test_set_2)
# Fitting Simple Linear Regression to the Training set
Linear_regression_2 = lm( outcome ~.,
                        data = training_set_2)
summary(Linear_regression_2)
# Predicting the Test set results
y_pred_2 = predict(Linear_regression_2, newdata = test_set_2)


#training error
sqrt(mean(Linear_regression_2$residuals^2))

# test error 

sqrt(mean((y_pred_2 - test_set_2$outcome)^2))

# RMSE error 

Linear_reg_RMSE_2 <-RMSE(y_pred_2, test_set_2$outcome)
Linear_reg_RMSE_2

# With Highly statistically significant predictors, I got an RMSE Value of 9.56


# TRYING SVM Model

set.seed(100)
split_3 = sample.split(data$outcome, SplitRatio = 0.8)
training_set_3 = subset(data, split_3 == TRUE)
test_set_3 = subset(data, split_3 == FALSE)

regressor_svm = svm( outcome ~ .,
                data = training_set_3,
                type = 'eps-regression',
                kernel = 'radial')
summary(regressor_svm)
y_pred_3 = predict(regressor_svm, newdata = test_set_3)

Linear_reg_RMSE_3 <-RMSE(y_pred_3, test_set_3$outcome)
Linear_reg_RMSE_3
# so, with the RMSE score is 4.12




# TRYING SVM Model 2

data2<- data

df_pred <- data.frame(data2 %>%select(-outcome))


# Applying Box Cox transformation with SVM

df_preprocess_fit<- preProcess(
  df_pred, method = c("BoxCox", "center", "scale"))
df_preprocess_fit

transformed_predictors <- predict(
  df_preprocess_fit, df_pred)

data_final <- add_column(transformed_predictors, outcome = data$outcome)


set.seed(100)
split_4 = sample.split(data_final$outcome, SplitRatio = 0.8)
training_set_4 = subset(data_final, split_4 == TRUE)
test_set_4 = subset(data_final, split_4 == FALSE)

regressor_svm_1 = svm( outcome ~ .,
                     data = training_set_4,
                     type = 'eps-regression',
                     kernel = 'radial')
summary(regressor_svm_1)
y_pred_4 = predict(regressor_svm_1, newdata = test_set_4)

Linear_reg_RMSE_4 <-RMSE(y_pred_4, test_set_4$outcome)
Linear_reg_RMSE_4

# RMSE value increases, so not applying this model fit 

# MODEL -5 
# creating model 5  for box cox transform and linear fit 

set.seed(100)
split_5 = sample.split(data_final$outcome, SplitRatio = 0.8)
training_set_5 = subset(data_final, split_5 == TRUE)
test_set_5 = subset(data_final, split_5 == FALSE)

Linear_regression_5 = lm( outcome ~.,
                          data = training_set_5)
summary(Linear_regression_5)
# Predicting the Test set results
y_pred_5 = predict(Linear_regression_5, newdata = test_set_5)


#training error
sqrt(mean(Linear_regression_5$residuals^2))

# test error 

sqrt(mean((y_pred_5 - test_set_5$outcome)^2))

# RMSE error 

Linear_reg_RMSE_5 <-RMSE(y_pred_5, test_set_5$outcome)
Linear_reg_RMSE_5

# RMSE value increaese so not applying this model 






























# fitted <- preProcess(
#   df_pred, 
#   method = c("pca"),
#   thresh=0.95, # cumulative percent of variance to be retained by PCA
# )
# 
# df_pca <- add_column(
#   predict(fitted, df_pred),
#   outcome = data$outcome)
# 
# 
# # with different threshold 
# 
# 
# df_pred1 <- data.frame(data2 %>%select(-outcome))
# fitted <- preProcess(
#   df_pred, 
#   method = c("pca"),
#   thresh=0.85, # cumulative percent of variance to be retained by PCA
# )
# 
# df_pca1 <- add_column(
#   predict(fitted, df_pred1),
#   outcome = data$outcome)
# 
# # with 85 % threshold, I get 9 PCA variables. 
# 
# 
# # Feature Engineering
# 

# splitting the dataset in test and train



# We will try various regression models on the dataset.





#train the data
limit<-train(df-x,df_y,method="lm")


#train and the test the data
train <- data[split1 == 0, ] 
test <- data[split1== 1, ]    




dt = sort(sample(nrow(data), nrow(data)*.8))
train<-data[dt,]
test<-data[-dt,]

#split the train 0.8 and test 0.2
library(caTools)
split=sample.split(comepetition_data,splitRatio=0.8)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)

#prepredict the data

predict(object,newdata,interval)
#Creates a linear model
my_linear_model <- lm(dist~speed,data = df)

#Prints the model results 
my_linear_model


#Creating a data frame
variable<-data.frame(speed=c(11,11,12,12,12,12,13,13,13,13))

#fitting the linear model
liner_model<-lm(dist~speed,data = df)

#predicts the future values
predict(liner_model,newdata = variable_speed)

#Input data
variable_speed <-data.frame(speed=c(11,11,12,12,12,12,13,13,13,13))

#Fits the model
liner_model<-lm(dist~speed,data = df)

#Predicts the values with confidence interval 
predict(liner_model,newdata = variable_speed,interval = 'confidence')

#Mars method
library(earth)
data("etitanic", package = "earth")
# Non-linear models: Multivariate Adaptive Regression Splines (MARS)

set.seed(100)
ThemarsGrid <- expand.grid(.degree = 1:2, .nprune = c(10, 20, 30, 40))
TheMarsFit <- train(data_x, data_y,
                    method = "earth",
                    tuneGrid = ThemarsGrid,
                    trControl = ctrl
)



ggplot(TheMarsFit)
min(TheMarsFit$results$RMSE)
TheMarsFit$bestTune


# Trees: Regression tree

set.seed(100)
ThecartTune <- train(x = data_x, y = data_y,
                     method = "rpart",
                     tuneLength = 15,
                     trControl = ctrl)
ggplot(ThecartTune)
min(ThecartTune$results$RMSE)
ThecartTune$bestTune


folds <- createFolds(pca_train_outcome, k = 2, returnTrain = TRUE)
ctrl <- trainControl(method = "cv", index = folds)

# LINEAR REGRESSION


set.seed(100)
lmTune<- train(x= pca_train_predictors, y= pca_train_outcome,
               method = "lm",
               trControl = ctrl)
lmTune
print(lmTune$results)
### Evaluating the training error for the Linear regression model
#### RMSE

trainfit_LR <- RMSE( predict(lmTune, pca_train_predictors), pca_train_outcome)
trainfit_LR

#SVM

svmRTuned <- train(
  pca_train_predictors, pca_train_outcome,
  method = "svmRadial",
  tuneLength = 8,
  epsilon = 0.01,
  trControl = ctrl
)
min(svmRTuned$result$RMSE)
svmRTuned$bestTune
ggplot(svmRTuned)





# We will try various regression models on the dataset.


# Trying other models 

#MARS


library(earth)
marsGrid <- expand.grid(.degree = 1:2, .nprune = c(20, 30, 40, 50))
marsFit <- train(pca_train_predictors, pca_train_outcome,
                 method = "earth",
                 tuneGrid = marsGrid,
                 trControl = ctrl
)
print(min(marsFit$results$RMSE))

prediction_1 <- predict(marsFit,newdata=test_data)
prediction <- data.frame(prediction_1)

write.csv(prediction, "Outcome.csv", row.names = FALSE)



# SVM 

#SVM

svmRTuned <- train(
  pca_train_predictors, pca_train_outcome,
  method = "svmRadial",
  tuneLength = 8,
  epsilon = 0.01,
  trControl = ctrl
)
min(svmRTuned$result$RMSE)
svmRTuned$bestTune
ggplot(svmRTuned)














