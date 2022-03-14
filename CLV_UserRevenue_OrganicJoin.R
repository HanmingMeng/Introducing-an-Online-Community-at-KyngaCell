
rm(list=ls())
library(openxlsx)
library(readxl)
library(Hmisc)
library(MASS)
library(caret)
library(pROC)
library (ROCR)

q2data<-read.xlsx("Assignment3HW3_Data.xlsx",3)
summary(q2data)
Churn<-q2data$Churned.at.3.months.after.launch.of.the.online.community
Customer_Age<-q2data$Customer.Age.with.Firm.at.time.of.launching.the.online.community
Average_Spend<-q2data$Average.Spend.Last.3.months.of.Life.with.the.firm
Join<-q2data$`Joined?`
Customer_ID<-q2data$Customer.ID
#Model Fitting
q2logit<-glm(Churn~Customer_Age+Join+Average_Spend,family=binomial(link="logit"))

#coefficients
summary(q2logit) 
anova(q2logit)
#Confidence Intervals
confint(q2logit) 

#exponentiated coefficients
exp(q2logit$coefficients)
q2logit$coefficients
#exponentiated confidence intervals
exp(confint(q2logit))

AIC(q2logit)

# Model Interpretation: As the odds ratio of JOIN is 2.5033417, which is >1. This result indicates that Join the community would INCREASE the probability of churn.  

#Odds Ratio Calculation, including confidence intervals
oddsr=round(exp(cbind(OddsRatio=coef(q2logit),confint(q2logit))),4)

oddsr
#OddsRatio  2.5 % 97.5 %
#  (Intercept)      1.5879 0.5580 4.5913
# Customer_Age     0.9495 0.8221 1.0962
# Join             2.5033 1.2611 5.1001
# Average_Spend    0.9971 0.9860 1.0082

confmat<-confusion_matrix(q2logit) #Predict True/False Positive/Negative (TP,TN,FP.FN)
#the confmat shows in a different order than the slides, so re-ordering.
#oldconfmat=confmat
#oldconfmat
confmat=t(confmat) 
n_o=c(2,1,3) #new order for better comparison
confmat[n_o,n_o]


## Assessing model predictions and classification
preddata<-with(q2data,data.frame(Customer_ID, Customer_Age,Join,Average_Spend))
probchurn<-predict(q2logit,newdata=preddata,type="response")
predchurn<-ifelse(probchurn > 0.5, 1,0)
missclass<-predchurn!=q2data$Churn
misclasserror<-mean(predchurn!=q2data$Churn)
print(paste('Accuracy',1-misclasserror))

finaldata<-cbind(q2data,probchurn,predchurn,missclass)
View(finaldata)


#magnitude
join<-subset(q2data, q2data$`Joined?` == 1)
not_join<-subset(q2data, q2data$`Joined?` == 0)
join_churn<-subset(join,join$Churned.at.3.months.after.launch.of.the.online.community==1)
notjoin_churn<-subset(not_join,not_join$Churned.at.3.months.after.launch.of.the.online.community==1)
r_Joined<-1-nrow(join_churn)/nrow(join)
r_NotJoined<-1-nrow(notjoin_churn)/nrow(not_join)
r_NotJoined
r_Joined
###CLV
m_Joined <-0.5*q2data[q2data$`Joined?`==1,]$Average.Spend.Last.3.months.of.Life.with.the.firm
m_NotJoined <-0.5*q2data[q2data$`Joined?`==0,]$Average.Spend.Last.3.months.of.Life.with.the.firm
L_Joined <- 1/(1-r_Joined)
L_NotJoined <- 1/(1-r_NotJoined)
CLV_Joined <- m_Joined * L_Joined
CLV_NotJoined <- m_NotJoined * L_NotJoined
mean(CLV_Joined)
mean(CLV_NotJoined)
#################################
q5data<-read.xlsx("Assignment3HW3_Data.xlsx",4)
summary(q5data)
Churn<-q5data$Churned.at.3.months
Customer_Age<-q5data$Customer.Age.with.Firm.at.time.of.launching.the.online.community
Average_Spend<-q5data$Average.Spend.Last.3.months.of.Life.with.the.firm
Join<-q5data$`Joined?`
Customer_ID<-q5data$Customer.ID
campaign<-q5data$`Campaign/Organic`
q5logit<-glm(Churn~campaign+Customer_Age+Join+Average_Spend,family=binomial(link="logit"))
summary(q5logit)
AIC(q5logit)

c<-subset(q5data, q5data$`Campaign/Organic` == 1)
o<-subset(q5data, q5data$`Campaign/Organic`  == 0)
c_churn<-subset(c,c$Churned.at.3.months==1)
o_churn<-subset(o,o$Churned.at.3.months==1)

r_c<-1-nrow(c_churn)/nrow(c)
r_o<-1-nrow(o_churn)/nrow(o)
r_c
r_o
