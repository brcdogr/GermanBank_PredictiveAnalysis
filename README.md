# GermanBank_PredictiveAnalysis
Graduate Degree Project in R | Predictive Analysis on German Credit Card Dataset

Developed Logistic Regression and Decision Tree predictive models to provide bank manager guidance for making a decision whether to approve a loan to an applicant based on his/her profiles.


---
title: "MSBA2_GERMANCREDITCARD"
author: "Burju Dogru"
date: "12/14/2020"
output:
  word_document: default
  html_document: default
  pdf_document: default
---

A German Bank uses some of the following features checking, duration, history, purpose, amount to determine if a credit card should be given to a customer or not.German Bank would like to discover what features supposed to influence their decision for giving a new credit card to a customer as well as to predict if a new customer who receives the credit card will be business success or failure.

```{r setup, echo = FALSE}
knitr::opts_chunk$set(
	echo = FALSE,
	message = FALSE,
	warning = FALSE
)
library(readxl)
my_german <- read_excel("german credit card.xls")
print(my_german[1:20,1:5])
```

## This is how my data looks like-sample 

We are looking at the first 20 observations and the first 5 variables; looks like the 20 german credit card costumers are different (heterogeneous). My next step would be to clean up a few variables.It is important first to understand all the dataset and then massage it for significant business insights.

```{r massaging}
#using gsub to replace data (patterns)
my_german$new_purpose <- gsub("X", "", my_german$purpose)
# back to converting the data type for purpose
my_german$purpose <- as.numeric(my_german$new_purpose)
# converting good/bad to numeric
my_german$binary <- as.numeric(as.factor(my_german$good_bad)) -1
summary(my_german$binary)
```

## Comment for Binary

We created a binary column for good and bad, which are interpreted as business success if variable is good or equal to 1.

Based on the summary statistics for binary, we are concluding that we have 70% customers that are a business success and 30% customers that are business failure

```{r modeling}
random_sample <- function(df, n ){
  
  training_indexes <- sample(1:nrow(df), n*nrow(df)) ## replace = FALSE by default
  training_dataset <- df[training_indexes, ]
  testing_dataset <- df[-training_indexes, ]
    return(list(training_dataset, testing_dataset))
  } # closing random_sample
my_random_output <- random_sample(df = my_german, n = 0.8)
```

With the logistic regression I can look at the variables impacts on the bank's decision to give a credit card.

We are comparing training and testing using 80/20 to test our logistic regression.

```{r}
training_data <- my_random_output[[1]] #getting training data from list
testing_data <- my_random_output[[2]] #getting testing data from list
# run a logistic for actual data with units
my_logit <- glm(binary ~ checking+duration+age+telephon+amount+savings+installp+coapp,
    data = training_data, family = "binomial")
summary(my_logit)
exp(0.6221)-1
```

## Commentary for Logistic Regression

Due to our model above we can see that variables checking, duration and savings are the most significant variables because their p values < 0.05. Our first coefficient is number of checking accounts. 
For every additional checking account, the odds of business success will increase by %86.3.


```{r}
#install.packages("ROCR")
library(ROCR)  
my_prediction_testing <- predict(my_logit, testing_data, type="response")

pred_val_logit <-prediction(my_prediction_testing, testing_data$binary)

perf_logit <- performance(pred_val_logit, "tpr", "fpr")

plot(perf_logit, col="blue")
```


Above what you see is a plot shows how my logistic regression model performs in terms of AUC-ROC. The AUC-ROC curve is an important evaluation metric for checking any classification model's performance. The higher the AUC (Area Under the Curve) is the better our model is at predicting business success.

Our next step is to put the same data in a decision tree for comparison. 

```{r tree}
library(rpart)
library(rpart.plot)
library(ROCR)
my_germ_tree <- rpart(binary~age + checking + duration + amount + savings + installp,
                      data =training_data, method="class", cp=0.018)
rpart.plot(my_germ_tree, type =1, extra =1, 
           box.palette=c("pink", "green"),
           branch.lty=3, shadow.col="gray")


my_prediction_strat <- predict(my_logit, testing_data, type="response")
pred_germ_logit <- prediction(my_prediction_strat, testing_data$binary)
perf_germ_logit <- performance(pred_germ_logit, "tpr", "fpr")



my_germ_tree_predict <- predict(my_germ_tree, testing_data, type="prob")
my_germ_tree_prediction <- prediction(my_germ_tree_predict[,2],
                                      testing_data$binary)
my_germ_tree_performance <- performance(my_germ_tree_prediction, "tpr", "fpr")

plot(my_germ_tree_performance, col="black", lty=3, lwd=3)
plot(perf_germ_logit, col="blue", lty=3, lwd=3, add=TRUE)

```

## Comparing Models for Business Insights

The logistic regression( indicated as blue) has a slightly higher AUC ROC than my decision tree(indicated as black). 
I would select the logistic regression model as my winner model because the AUR ROC shows the area under the curve is higher for this model.

