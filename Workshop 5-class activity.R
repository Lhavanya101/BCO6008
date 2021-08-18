Install.packages("MASS") 
Install.packages("ISLR")

library(tidymodels)
library(MASS)
library(ISLR)

set.seed(123)
lm-spec<-linear-reg()%>%
  set_mode("regression")%>%
  set_engine("lm")

lm_spec
data(Boston)

lm_fit<-lm_spec%>%fit(data=Boston, medv ~lstat)


lm_fit
tidy(lm_fit)

predict(lm_fit, new_data =Boston)
final_model<-augment(lm_fit, new_data=Boston)%>%select(medv,.preds())



rm(list=ls())
data(Boston)
#1
model_spec<-linear_reg()%>%
  set_mode("regression")%>%
  set_engine("lm")
#2 specify variable
model_spec%>%fit(data=Boston, medv~age+crim+rm)


#3
model_predicted(model_fit, new_data=Boston)
