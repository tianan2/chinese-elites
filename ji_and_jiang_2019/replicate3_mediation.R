# Ji and Jiang "Enlightened One-Party Rule? Ideological Differences between Chinese Communist Party Members and the Mass Public"
# Replication File -- Figures 2019.4.19

# Required packages
# install.packages("mediation")
# install.packages("readstata13")
# install.packages("dummies")

rm(list=ls(all=TRUE))

library(mediation)
library(readstata13)
library(dummies)
setwd("C:/Users/JCY/Dropbox/Ideology/replicate")

data<-read.dta13("data.dta")

data1<-dummies::dummy(data$income5q)
data<-cbind(data,data1)

names(data)
attach(data)

Y <- cbind(social_value)
X <- cbind(male,age,urban,income5q2,income5q3,income5q4,income5q5)

set.seed(2018)

med.fit1 <- lm(edulevel~ party + X, data = data,weight=wcn)
out.fit1 <- lm(Y ~ edulevel+party + X, data = data,weight=wcn)
med.out1 <-mediate(med.fit1, out.fit1, treat = "party",weight=wcn, 
                   mediator = "edulevel",boot = TRUE,sims = 1000)

data1<-subset(data,data$survey==7 |data$survey==2)
names(data1)
attach(data1)
Y <- cbind(political_value)
X <- cbind(male,age,urban,income5q2,income5q3,income5q4,income5q5)
med.fit2 <- lm(edulevel ~ party + X, data = data1,weight=wcn)
out.fit2 <- lm(Y ~ edulevel +party + X, data = data1,weight=wcn)
med.out2 <-mediate(med.fit2, out.fit2, treat = "party",weight=wcn, 
                   mediator = "edulevel",boot = TRUE,sims = 1000)

Y <- cbind(intl_value)
med.fit3 <- lm(edulevel~ party + X, data = data1,weight=wcn)
out.fit3 <- lm(Y ~ edulevel+party + X, data = data1,weight=wcn)
med.out3 <-mediate(med.fit3, out.fit3, treat = "party",weight=wcn, 
                   mediator = "edulevel",boot = TRUE,sims = 1000)

Y <- cbind(modern_all_value)
med.fit4 <- lm(edulevel ~ party + X, data = data1,weight=wcn)
out.fit4 <- lm(Y ~ edulevel +party + X, data = data1,weight=wcn)
med.out4 <-mediate(med.fit4, out.fit4, treat = "party",weight=wcn, 
                   mediator = "edulevel",boot = TRUE,sims = 1000)

summary(med.out1)
summary(med.out2)
summary(med.out3)
summary(med.out4)

out1<-rbind(med.out1[["d0"]],
            med.out1[["tau.coef"]],
            med.out1[["n0"]])
out2<-rbind(med.out2[["d0"]],
            med.out2[["tau.coef"]],
            med.out2[["n0"]])
out3<-rbind(med.out3[["d0"]],
            med.out3[["tau.coef"]],
            med.out3[["n0"]])
out4<-rbind(med.out4[["d0"]],
            med.out4[["tau.coef"]],
            med.out4[["n0"]])
name <- c("ACME", "Total Effect", "Prop Mediated")
out<-data.frame(name,cbind(out1,out2,out3,out4))
names(out)[2]<-"social"
names(out)[3]<-"political"
names(out)[4]<-"international"
names(out)[5]<-"overall modern value"
out
