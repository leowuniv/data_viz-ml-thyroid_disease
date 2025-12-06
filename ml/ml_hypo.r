# Following logistic regression implementation according to these resources 
# https://www.sthda.com/english/articles/36-classification-methods-essentials/151-logistic-regression-essentials-in-r/
# https://glmnet.stanford.edu/articles/glmnet.html#logistic-regression-family-binomial

# I will first be building a logistic regression ML model on the allhypo.data dataset.
# I will then analyze the model's performance and extract what features are the most important in determining
# what patients were classified with the hypothyroid disease.

# import packages
library(tidyverse)
library(caret)
library(glmnet)

# analyze dataset and convert types
str(remove_nas_allhypo)
str(remove_nas_allhypo_test)

train_data <- remove_nas_allhypo
test_data <- remove_nas_allhypo_test

# convert numeric variables from character to numeric
numeric_vars <- c("age", "TSH_reading", "T3_reading", "T4_reading", 
                  "thyrox_util_rate_T4U_reading", "FTI_reading")

for(var in numeric_vars) {
  train_data[[var]] <- as.numeric(train_data[[var]])
  test_data[[var]] <- as.numeric(test_data[[var]])
}

# convert boolean variables to numeric
binary_vars <- c("presc_thyroxine", "queried_why_on_thyroxine", 
                 "presc_anthyroid_meds", "sick", "pregnant", 
                 "thyroid_surgery", "radioactive_iodine_therapyI131",
                 "query_hypothyroid", "query_hyperthyroid", "lithium",
                 "goitre", "tumor", "hypopituitarism", "psych_condition",
                 "TSH_measured", "T3_measured", "T4_measured",
                 "thyrox_util_rate_T4U_measured", "FTI_measured", "ref_src")

for(var in binary_vars) {
  train_data[[var]] <- ifelse(train_data[[var]] == "t", 1, 0)
  test_data[[var]] <- ifelse(test_data[[var]] == "t", 1, 0)
}

# convert sex to factor
train_data$sex <- as.factor(train_data$sex)
test_data$sex <- as.factor(test_data$sex)

# convert target variable to factor
train_data$ThyroidClass <- as.factor(train_data$ThyroidClass)
test_data$ThyroidClass <- as.factor(test_data$ThyroidClass)

# remove record_id because useless for model
train_data$record_id <- NULL
test_data$record_id <- NULL

# observe different classes in thyroid class
print(table(unique(train_data$ThyroidClass)))
print(table(unique(test_data$ThyroidClass)))

# since train dataset contains secondary hypothyroid class (which is not seen in test dataset), remove it
test_data <- test_data[test_data$ThyroidClass %in% unique(train_data$ThyroidClass),]

# we want to have the model be binary classification (hypothyroid or not) but we have multiple class
# variables so we need to split them up based on their classification
# compensated hypothyroid -> hypothyroid
# primary hypothyroid -> hypothyroid
# negative -> non-hypothyroid
train_data$ThyroidClass <- ifelse(train_data$ThyroidClass %in% c("compensated hypothyroid", "primary hypothyroid"),
                          "hypothyroid",
                          "non-hypothyroid")
train_data$ThyroidClass <- as.factor(train_data$ThyroidClass)
test_data$ThyroidClass <- ifelse(test_data$ThyroidClass %in% c("compensated hypothyroid", "primary hypothyroid"),
                          "hypothyroid",
                          "non-hypothyroid")
test_data$ThyroidClass <- as.factor(test_data$ThyroidClass)

# confirm data looks correct
str(train_data)
str(test_data)

# convert data into matrix for glmnet model
# [,-1] removes intercept column from matrix
x <- model.matrix(ThyroidClass ~ ., data=train_data)[,-1]
# set y to target variable
y <- train_data$ThyroidClass

# glmnet is a ridge regularized logistic regression model which penalizes the model during training process
# this model is better than base R glm because of the instability of the data (many cases of nonhypothyroid
# vs hypothyroid) Additionally, glmnet uses k-fold cross-validation which splits the dataset into k folds
# (default k=10) in which each fold is used as the validation set once, while the rest k-1 folds are used
# for training. This allows for a more stable model and reduces overfitting.
# source: https://machinelearningmastery.com/k-fold-cross-validation/

# parameters:
# x -> data in numeric matrix format
# y -> target variable
# family = "binomial" -> logistic regression
# alpha = 0 -> ridge regularization
# type.measure = "class" -> gives misclassification error
model <- cv.glmnet(
    x, y, 
    family = "binomial",
    alpha = 0,     # ridge
    type.measure = "class"
)

# convert data into matrix for glmnet model
# [,-1] removes intercept column from matrix
x_test <- model.matrix(ThyroidClass ~ ., test_data)[, -1]

# predict model's probabilities on test set
# newx -> test set
# s = "lambda.min" -> chooses the lambda with the lowest cross-validation error
# type = "response" -> returns probabilities (between 0 and 1)
probabilities <- predict(
    model,
    newx = x_test,
    s = "lambda.min",
    type = "response"
)

# creates binary predictions
# if the probability is over 0.85 (chosen intuitively), then the model will predict that the patient
# with its features is non-hyperthyroid and if it is less than the model will predict that the patient 
# is hypothyroid.
predicted_classes <- ifelse(probabilities > 0.85,
                            "non-hypothyroid",
                            "hypothyroid")
# confusion matrix expects factored type
predicted_classes <- factor(predicted_classes)
# Confusion matrix offers some useful statistics on our model
# source: https://www.digitalocean.com/community/tutorials/confusion-matrix-in-r
print(confusionMatrix(predicted_classes, test_data$ThyroidClass))

# Counts how many predictions from the model matched the actual diagnoses
accuracy <- mean(predicted_classes == test_data$ThyroidClass)
print(paste("Accuracy: ", accuracy))

# observe the coefficients of each variable at the best regularization value
# shows us what features contribute most to classification
print(coef(model, s = "lambda.min"))