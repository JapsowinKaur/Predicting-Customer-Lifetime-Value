rm(list=ls())
library(readxl)
library(Hmisc)
library(MASS)
library(caret)
library(regclass)
library(ISLR)
library(boot)
library(vcd)
library(pROC)

mydata<-read_excel("CustomerData.xlsx")
View(mydata)
summary(mydata)
colnames(mydata)
str(mydata)

mydata$Churn <- as.factor(mydata$Churned3Mths)
mydata$Customer <- as.factor(mydata$`Customer ID`)
mydata$Joined <- as.factor(mydata$Joined)
mydata$Age <- as.numeric(mydata$CustomerAge)
mydata$AvgSpend <- as.numeric(mydata$AvgSpendLast3Mths)
mydata$campaign <- as.factor(mydata$`Campaign/Organic`)

# Model 1

mylogit<-glm(Churn ~ Age + AvgSpend + DiffSpend + Joined + campaign,data=mydata,family=binomial(link="logit"))
summary(mylogit)
AIC(mylogit)
BIC(mylogit)

# Model 2

mylogit2<-glm(Churn ~ Joined ,data=mydata,family=binomial(link="logit"))
summary(mylogit2)

AIC(mylogit2)
BIC(mylogit2)

# Predictions

predict(mylogit, mydata, type = 'response')
predict(mylogit2, mydata, type = 'response')


# Goodness of fit

preddata<-with(mydata,data.frame(Age, AvgSpend , DiffSpend , Joined , campaign))
probchurn<-predict(mylogit,newdata=preddata,type="response")
predchurn<-ifelse(probchurn > 0.5, 1,0)
missclass<-predchurn!=mydata$Churn
misclasserror<-mean(predchurn!=mydata$Churn)
print(paste('Accuracy',1-misclasserror))

confmat<-confusion_matrix(mylogit) #Predict True/False Positive/Negative (TP,TN,FP.FN)
confmat

confMat2<-confusionMatrix(data = as.factor(predchurn),reference = as.factor(mydata$Churn),positive = "1")
confMat2

finaldata<-cbind(mydata,probchurn,predchurn,missclass)
View(finaldata)
library(Hmisc)
write.csv(finaldata, file = "FinalData.csv")

# Odds Ratio

#Odds Ratio Calculation, including confidence intervals
oddsr=round(exp(cbind(OddsRatio=coef(mylogit),confint(mylogit))),4)
oddsr


# CLV Testing

CLV <- read.csv("CLV Raw Data.csv")
head(CLV)

str(CLV)
CustID <- as.factor(CLV$Customer.ID)
CLVBefore <- as.numeric(CLV$CLV.before)
CLVAfter <- as.numeric(CLV$CLV.after)

# Hyp null: CLV After <= CLV Before
# Hyp Alternative: CLV After > CLV Before

var.test(CLVAfter, CLVBefore, ratio = 1, alternative = "greater")

### Variances are equal, so run Equal Variance t-test ###

t.test(CLVAfter, CLVBefore, alternative = "greater", mu = 0, paired = TRUE, var.equal = TRUE)

# Result: p is significant (5% alpha), hence CLV increased after introduction of online community

