# Calculating RR from binomial logistic reg
# https://discourse.mc-stan.org/t/binomial-regression-with-a-log-link/4102/5

library(rstanarm)

rm(list=ls(all=TRUE))

#setwd("Z:/My Documents/LateHypothermia/BayesFinalAnalyses")

df1<-read.csv("Z:/My Documents/LateHypothermia/FinalData/LH_outcomes_v2.csv",head=T)

df1<-df1[order(df1$newcenter),]
df1$deathsevere<-rep(NA,nrow(df1))
df1$deathsevere[df1$death==1 | df1$severe==1]<-1
df1$deathsevere[df1$death==0 & df1$severe==0]<-0


l1.1<-stan_glm(po1~trt*lev_enceph,
               family = binomial,data=df1,
               prior = normal(rep(0,3),c(10,10,10),autoscale = F), 
               #prior_intercept = normal(0,10),
               chains = 3, warmup=3000,iter = 10000)
summary(l1.1,digits=4)

nd <- df1[,c("po1","trt","lev_enceph","age")]
nd$trt <- 0

Pr_0 <- posterior_linpred(l1.1, newdata = nd, transform = TRUE)
nd$trt <- 1
Pr_1 <- posterior_linpred(l1.1, newdata = nd, transform = TRUE)
RR1 <- Pr_1 / Pr_0
quantile(RR1,c(.5,.025,.975))

OR1<-(Pr_1/(1-Pr_1))/(Pr_0/(1-Pr_0))
quantile(OR1,c(.5,.025,.975))


df2<-df1
# df2$trt<-df2$trt*2-1
# df2$lev_enceph<-df2$lev_enceph*2-1

df2$trt<-df2$trt-.5
df2$lev_enceph<-df2$lev_enceph-.5

l1.4<-stan_glm(po1~trt*lev_enceph,
               family = binomial,data=df2,
               prior = normal(rep(0,3),c(100,100,100),autoscale = F), 
               #prior_intercept = normal(0,10),
               chains = 3, warmup=3000,iter = 10000)
summary(l1.4,digits=4)

betas<-as.matrix(l1.4,pars="trt")
quantile(exp(betas),c(.5,.025,.975))
nd <- df2[,c("po1","trt","lev_enceph","age")]
nd$trt <- 0-.5

Pr_0.1 <- posterior_linpred(l1.4, newdata = nd, transform = TRUE)
nd$trt <- 1-.5
Pr_1.1 <- posterior_linpred(l1.4, newdata = nd, transform = TRUE)
RR0 <- Pr_1 / Pr_0
quantile(RR0,c(.5,.025,.975))



m1<-glm(po1~trt*lev_enceph,
        family = binomial,data=df1)
summary(m1)
nd1<-df1[1,c("po1","trt","lev_enceph")]
nd1$lev_enceph<-0
nd1$trt<-0

s11<-sum(coef(m1)*c(1,1,1,1))
s10<-sum(coef(m1)*c(1,1,0,0))
s01<-sum(coef(m1)*c(1,0,1,0))
s00<-sum(coef(m1)*c(1,0,0,0))

s11-s01

predict(m1,newdata=nd1,type="response")

m2<-glm(po1~trt*lev_enceph,
        family = binomial,data=df2)
summary(m2)

nd1<-df1[1,c("po1","trt","lev_enceph")]
nd1$lev_enceph<-.5
nd1$trt<-.5

l11<-sum(coef(m2)*c(1,.5,.5,.25))
l10<-sum(coef(m2)*c(1,.5,-.5,-.25))
l01<-sum(coef(m2)*c(1,-.5,.5,-.25))
l00<-sum(coef(m2)*c(1,-.5,-.5,.25))

((l11-l01)+(l10-l00))/2

p11<-predict(m2,newdata=nd1,type="response")
nd1$lev_enceph<-(-.5)
nd1$trt<-.5
p10<-predict(m2,newdata=nd1,type="response")
nd1$lev_enceph<-.5
nd1$trt<-(-.5)
p01<-predict(m2,newdata=nd1,type="response")
nd1$lev_enceph<-(-.5)
nd1$trt<-(-.5)
p00<-predict(m2,newdata=nd1,type="response")
