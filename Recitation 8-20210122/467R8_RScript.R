### "STAT 467-Recitation 8"


library(CCA)
library(CCP)
library(caret)
library(ISLR)
library(corrplot)
library(pROC)
library(tidyverse)
library(mlbench)
library(e1071)


## Canonical Correlation Analysis (CCA)

##Canonical Correlation Analysis using R##

##Example 1##

data<-read.csv("trial_data.csv",header=T)
head(data)


che<-data[,1:3]
acidity<-data[,4:7]

library(CCA) #canoncial analysis
library(CCP) #significance
#checking the between and within set associations
cormat<-matcor(che,acidity)
#extracting the within study correlations for set 1 and set 2 and #between set cor
round(cormat$Ycor, 4)


round(cormat$Xcor, 4)

#between set associations
round(cormat$XYcor, 4)


#obtaining the canonical correlations
 can_cor1=cc(che,acidity)
 can_cor1$cor

( can_cor1$cor^2)

#raw canonical coefficients
 can_cor1[3:4]

plt.cc(can_cor1, var.label = TRUE)


#computes the canonical loadings
 can_cor2=comput(che,acidity,can_cor1)
 can_cor2[3:6] #displays the canonical loadings

#test of canonical dimensions
 rho=can_cor1$cor
##defining the number of observations, no of variables in first set,
 #and number of variables in second set
 n=dim(che)[1]
 p=length(che)
 q=length(acidity)
 ##Calculating the F approximations using different test statistics
 #using wilks test statistic
 p.asym(rho,n,p,q,tstat="Wilks")

## Classification Problem 


### Logistic Regression


##Example##


library(ISLR)
head(Default)


set.seed(42)
default_idx = sample(nrow(Default), 5000) #produce index number for train data
default_trn = Default[default_idx, ]
default_tst = Default[-default_idx, ]



##Simple Logistic Regression##


model_glm = glm(default ~ balance, data = default_trn, family = "binomial")


summary(model_glm)


head(predict(model_glm))


head(predict(model_glm, type = "link"))


head(predict(model_glm, type = "response"))



#model_glm_pred = ifelse(predict(model_glm, type = "link") > 0, "Yes", "No")
model_glm_pred = ifelse(predict(model_glm, type = "response") > 0.5, "Yes", "No")

calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}



calc_class_err(actual = default_trn$default, predicted = model_glm_pred)

train_tab = table(predicted = model_glm_pred, actual = default_trn$default)
library(caret)
train_con_mat = confusionMatrix(train_tab, positive = "Yes")
c(train_con_mat$overall["Accuracy"], 
  train_con_mat$byClass["Sensitivity"], 
  train_con_mat$byClass["Specificity"])


model_glm_pred_test= ifelse(predict(model_glm, type = "response",newdata = default_tst) > 0.5, "Yes", "No")
#As you see we use newdata argument that equals to test data


test_tab = table(predicted = model_glm_pred_test, actual = default_tst$default)
library(caret)
test_con_mat = confusionMatrix(test_tab, positive = "Yes")
c(test_con_mat$overall["Accuracy"], 
  test_con_mat$byClass["Sensitivity"], 
  test_con_mat$byClass["Specificity"])


##Multiple Logistic Regression##

library(corrplot)
correlations <- cor(default_trn[,3:4])
corrplot(correlations, method="circle")



model_glm_multi = glm(default ~ ., data = default_trn, family = "binomial")

summary(model_glm_multi)


ifelse ((1483.83-809)< qchisq(0.95,4),print("Fail to Reject Ho"),print("Reject Ho"))


model_glm_pred_test= ifelse(predict(model_glm_multi, type = "response",newdata = default_tst) > 0.5, "Yes", "No")
#As you see we use newdata argument that equals to test data

test_tab = table(predicted = model_glm_pred_test, actual = default_tst$default)
library(caret)
test_con_mat = confusionMatrix(test_tab, positive = "Yes")
c(test_con_mat$overall["Accuracy"], 
  test_con_mat$byClass["Sensitivity"], 
  test_con_mat$byClass["Specificity"])

##For 0.1##


model_glm_pred_test_0.1= ifelse(predict(model_glm_multi, type = "response",newdata = default_tst) > 0.1, "Yes", "No")
#As you see we use newdata argument that equals to test data


test_tab = table(predicted = model_glm_pred_test_0.1, actual = default_tst$default)
library(caret)
test_con_mat = confusionMatrix(test_tab, positive = "Yes")
c(test_con_mat$overall["Accuracy"], 
  test_con_mat$byClass["Sensitivity"], 
  test_con_mat$byClass["Specificity"])


##For 0.5##

model_glm_pred_test_0.5= ifelse(predict(model_glm_multi, type = "response",newdata = default_tst) > 0.5, "Yes", "No")
#As you see we use newdata argument that equals to test data


test_tab = table(predicted = model_glm_pred_test_0.5, actual = default_tst$default)
library(caret)
test_con_mat = confusionMatrix(test_tab, positive = "Yes")
c(test_con_mat$overall["Accuracy"], 
  test_con_mat$byClass["Sensitivity"], 
  test_con_mat$byClass["Specificity"])


##For 0.9##



model_glm_pred_test_0.9= ifelse(predict(model_glm_multi, type = "response",newdata = default_tst) > 0.9, "Yes", "No")
#As you see we use newdata argument that equals to test data



test_tab = table(predicted = model_glm_pred_test_0.9, actual = default_tst$default)
library(caret)
test_con_mat = confusionMatrix(test_tab, positive = "Yes")
c(test_con_mat$overall["Accuracy"], 
  test_con_mat$byClass["Sensitivity"], 
  test_con_mat$byClass["Specificity"])


##What is AUC - ROC curve?##


library(pROC)
test_prob=predict(model_glm_multi, type = "response",newdata = default_tst)
test_roc = roc(default_tst$default ~ test_prob, plot = TRUE, print.auc = TRUE)
as.numeric(test_roc$auc)


### Naive Bayes Classifier 


##Example 3##

library(tidyverse)
library(mlbench)
data(PimaIndiansDiabetes)
head(PimaIndiansDiabetes)



PimaIndiansDiabetes%>%glimpse()


PimaIndiansDiabetes$diabetes<-as.factor(ifelse(PimaIndiansDiabetes$diabetes=="neg",1,0))
head(PimaIndiansDiabetes$diabetes)


library(ggcorrplot)
corr=cor(PimaIndiansDiabetes[,-9])
#extract the numerical variables
corr




ggcorrplot(corr, method = "circle")


data<-PimaIndiansDiabetes%>%select(pregnant, glucose,pressure,pedigree,diabetes)


summary(data)


set.seed(123)
trainindex<-sample(1:nrow(data),round(0.8*nrow(data)))
train<-data[trainindex,]
test<-data[-trainindex,]



dim(train)



dim(test)



library(e1071)
NBclassfier=naiveBayes(diabetes~., data=train)
print(NBclassfier)

#### Train and Test Performance

####  Train

##NBC##


train_pred<-predict(NBclassfier, newdata = train, type = "class")



train_tab = table(predicted = train_pred , actual = train$diabetes)
library(caret)
train_con_mat = confusionMatrix(train_tab)
c(train_con_mat$overall["Accuracy"], 
  train_con_mat$byClass["Sensitivity"], 
  train_con_mat$byClass["Specificity"])



#### Test

##NBC##
  

test_pred<-predict(NBclassfier, newdata = test[,-5], type = "class")



test_tab = table(predicted = test_pred , actual = test$diabetes)
library(caret)
test_con_mat = confusionMatrix(test_tab)
c(test_con_mat$overall["Accuracy"], 
  test_con_mat$byClass["Sensitivity"], 
  test_con_mat$byClass["Specificity"])



### Comparision of Logistic Regression and Naive Bayes Classifier


test_tab = table(predicted = model_glm_pred_test_0.5, actual = default_tst$default)
library(caret)
test_con_mat = confusionMatrix(test_tab, positive = "Yes")
c(test_con_mat$overall["Accuracy"], 
  test_con_mat$byClass["Sensitivity"], 
  test_con_mat$byClass["Specificity"])


library(e1071)
NBclassfier=naiveBayes(default~., data=default_trn)
print(NBclassfier)


test_pred<-predict(NBclassfier, newdata = default_tst[,-1], type = "class")


test_tab = table(predicted = test_pred , actual = default_tst$default)
library(caret)
test_con_mat = confusionMatrix(test_tab)
c(test_con_mat$overall["Accuracy"], 
  test_con_mat$byClass["Sensitivity"], 
  test_con_mat$byClass["Specificity"])


