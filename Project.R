# Dependencies
install.packages("plm")
library(plm)
 install.packages("lme4")
install.packages("Matrix")
library(lme4)
install.packages("arm")
library(arm)
install.packages("MASS")
library(MASS)
#Reading Data
setwd("D:\\Spring 2018\\SDM\\sdm final")
d<-read.csv("sdm_project.csv",header = TRUE)
d
# Set data as panel data
pdata <- plm.data(d, index=c("State","Year"))
pdata

# Descriptive statistics
hist(log(d$Crude.Rate)) 
hist(d$Prescriptions)
summary(d$Prescriptions)



# Pooled OLS estimator
pooling <- plm(log(Crude.Rate)~as.factor(Legalization)+Prescriptions, data=pdata, model= "pooling")
summary(pooling)
plot(pooling$residuals)


# Fixed effects or within estimator

fixed <- plm(log(Crude.Rate) ~ as.factor(Legalization)+Prescriptions, data=pdata, model= "within")
summary(fixed)
fixef(fixed)
coef(fixed)
plot(fixed$residuals) 
qqnorm(fixed$residuals) 
qqline(fixed$residuals,col="Red")



# Random effects estimator
random <- plm(log(Crude.Rate) ~ as.factor(Legalization)+(Prescriptions), data=pdata, model= "random")
summary(random)
ranef(random)
coef(random)
random$residuals
plot(random$residuals)
qqnorm(random$residuals)
qqline(random$residuals,col="Red")

phtest(fixed, random)

#Third Model
m <- lmer(log(Crude.Rate)~as.factor(Legalization)+Prescriptions+ (1 |State),data=pdata)
summary(m)
fixef(m)
ranef(m)
coef(m)


pHtest(fixed,random)
