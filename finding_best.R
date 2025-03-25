#Previously We discuss the characteristic of the independent variables and now
#We are going to Build Regression model.

library(tidyverse)
library(olsrr)
DF<-ins_data


#In previously we decided to remove the GDP variable from the model because it's 
#not fulfill the assumptions of MLP.

New_DF <- DF
glimpse(New_DF)
View(New_DF)

#------------------------------------------------------------------------------#
# 01 - Find the best regression model
#------------------------------------------------------------------------------#

################################################################################
#ALL POSSIBLE REGRESSIONS
################################################################################

#Creating the model using all Variables
model <- lm(New_DF$Premium~New_DF$Age+New_DF$Income+New_DF$N0_of_dependents+New_DF$Policy.Term+New_DF$Basic.Sum.Assured+New_DF$Gen+New_DF$CS+New_DF$HC+New_DF$BHH )
Models <- select(ols_step_all_possible(model)$result,n,predictors,rsquare,adjr)
Models$rsquare <-round(Models$rsquare*100,4)
Models$adjr <-round(Models$adjr*100,4)
view(Models)
write.csv(Models, "All_possible_models.csv")
confint(model)
model1_summary<-summary(model)
model1_summary$r.squared
vie