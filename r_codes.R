#Exploratory Data Analysis
library(tidyverse)
library(ggplot2)

Ins_data<-read.csv("InsData.csv")
Ins_data
glimpse(Ins_data)

attach(Ins_data)

boxplot(Age)
boxplot(Income)
boxplot(Basic.Sum.Assured)
boxplot(N0_of_dependents)
sd(Basic.Sum.Assured)


hist(Ins_data$Basic.Sum.Assured,xlab = "BSA",ylab = "Frequency",main = "Histogram of BSA")
hist(Ins_data$Age,xlab = "Age",ylab = "Frequency",main = "Histogram of Age")
hist(Ins_data$N0_of_dependents,xlab = "No of Dependents",ylab = "Frequency",main = "Histogram of No of Dependents")
hist(Ins_data$Policy.Term,xlab = "Policy Term",ylab = "Frequency",main = "Histogram of Policy Term")
hist(Ins_data$Income,xlab = "Income",ylab = "Frequency",main = "Histogram of Income")

#categorical variables (scatter plots)

attach(Ins_data)
Ins_data


plot(Basic.Sum.Assured,Premium,main = "BSA v Premium")
mod.BSA<-lm(Premium~Basic.Sum.Assured)
abline(mod.BSA,col=2,lwd=2)
summary(mod.BSA)
names(Ins_data)


plot(Age,Premium,main = "Age v Premium")
mod.age<-lm(Premium~Age)
abline(mod.age,col=2,lwd=2)
summary(mod.age)

plot(N0_of_dependents,Premium,xlab = "No of Dependents",main="NOD v Premium")
mod.nod<-lm(Premium~N0_of_dependents)
abline(mod.nod,col=2,lwd=2)


summary(Income)
summary(Age)
summary(N0_of_dependents)
summary(Policy.Term)
summary(Basic.Sum.Assured)
summary(Premium)

#ANOVA

Ins_data
library(tidyverse)
Ins_data<-set_names(Ins_data,c("Age","Inc","NOD","PT","BSA","Pre","Gen","CS","HC","BHH"))
Ins_data

view(Ins_data)
attach(Ins_data)



full_model<-lm(Ins_data$Pre~Ins_data$Age+Ins_data$Inc+Ins_data$NOD+Ins_data$PT+Ins_data$BSA+Ins_data$Gen+Ins_data$CS+Ins_data$HC+Ins_data$BHH)
summary(full_model)
full_model_anova<-anova(full_model)
full_model_anova
view(full_model_anova)



red_model<-lm(Ins_data$Pre~Ins_data$Age+Ins_data$Inc+Ins_data$PT+Ins_data$BSA+Ins_data$HC)
summary(red_model)
red_model_anova<-anova(red_model)
red_model_anova
view(red_model_anova)



#doing f test for 5

Model_5 <- lm(Premium ~ Age+Income+Policy.Term+Basic.Sum.Assured+HC)
summary(Model_5)
anova(Model_5)

#doing f test for 5 and 4
Model_4 <- lm(Premium ~ Age+Income+Basic.Sum.Assured+HC)
summary(Model_4)
anova(Model_4)

#doing f test for 4 and 3 
Model_3 <- lm(Premium ~ Age+Income+Basic.Sum.Assured)
summary(Model_3)
anova(Model_3)

#doing f test for 3 and 2
Model_2 <- lm(Premium ~ Age+Basic.Sum.Assured)
summary(Model_2)
anova(Model_2)


#Checking 
Model_5 <- lm(Premium ~ Age+Income+Policy.Term+Basic.Sum.Assured+HC)
summary(Model_5)
anova(Model_5)

Mode_5_Reses <- Model_5$residuals
Mode_5_Reses
hist(Mode_5_Reses)
qqnorm(Mode_5_Reses,xlab="Theoretical Quantiles",ylab="Sample Quantiles",main = "qq - plot of model 5")
qqline(Mode_5_Reses,col=2,lwd=2)

Model_4 <- lm(Premium ~ Age+Income+Basic.Sum.Assured+HC)
summary(Model_4)
anova(Model_4)

Mode_4_Reses <- Model_4$residuals
Mode_4_Reses
hist(Mode_4_Reses)
qqnorm(Mode_4_Reses,xlab="Theoretical Quantiles",ylab="Sample Quantiles",main = "qq-plot of model 4")
qqline(Mode_4_Reses,col=2,lwd=2)

Model_3 <- lm(Premium ~ Age+Income+Basic.Sum.Assured)
summary(Model_3)
anova(Model_3)

Mode_3_Reses <- Model_3$residuals
Mode_3_Reses
hist(Mode_3_Reses)
qqnorm(Mode_3_Reses,xlab="Theoretical Quantiles",ylab="Sample Quantiles",main = "qq-plot of model 3")
qqline(Mode_3_Reses,col=2,lwd=2)


Model_2 <- lm(Premium ~ Age+Basic.Sum.Assured)
summary(Model_2)
anova(Model_2)

Mode_2_Reses <- Model_2$residuals
Mode_2_Reses
hist(Mode_2_Reses)
qqnorm(Mode_2_Reses,xlab="Theoretical Quantiles",ylab="Sample Quantiles",main = "qq-plot of model 2")
qqline(Mode_2_Reses,col=2,lwd=2)






getwd()
