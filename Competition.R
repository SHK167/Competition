# COMPETITION
# read data 
library(tidyverse)
library(elasticnet)
library(corrplot)
library(caret)
library(lattice)
library(pls)
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
Linear_regression_2 = lm( outcome ~ X1+X2+X6+X9+X10+X11+X12+X17+X18+X19+X20+X21 ,
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

# With Highly statistically significant predictors, I got an RMSE Value of 9.60






































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








