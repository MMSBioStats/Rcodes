# sample code to run bayes logistic and calculate RR
# prior for log OR is based on prior for RR
library(rstanarm)
rm(list=ls())

load("G:/My Drive/Avritscher/InpatientTrial/InpatientFinalData.RData")

# calculating risk in control group to set prior range
p0<-mean(df1$compositeoutcomeag[df1$group==0])

# We assume prior for RR of 0.5-2
#upper bound
RR<-3
ub<-RR*(1-p0)/(1-RR*p0)
#lower bound
RR<-.33
lb<-RR*(1-p0)/(1-RR*p0)
# calculating sd
sd1<-log(ub)/1.96
sd2<-log(lb)/(-1.96)
# pick smallest sd to be conservative
sd.prior<-min(sd1,sd2)

m1<-glmer(compositeoutcomeag~group+baseline_risk+(1|sibfactor)+offset(log(followupdaysag)),
          nAGQ = 10,family=binomial,data=df1)


l1<-stan_glmer(compositeoutcomeag~group+baseline_risk+(1|sibfactor),
               
               data=df1,
               family=binomial,prior=normal(c(0,0),c(sd.prior,1)), 
               chains = 3, 
               warmup=3000,iter = 10000)
summary(l1,pars=c("alpha","beta"),digits=4)
betas<-as.matrix(l1,pars="group")
# OR summary
quantile(exp(betas),c(.5,.025,.975))

# calculating predicted prob under control
nd <- df1
nd$group <- 0
Pr_0 <- posterior_linpred(l1, newdata = nd,transform = T)

# calculating predicted prob under treat
nd$group <- 1
Pr_1 <- posterior_linpred(l1, newdata = nd,transform = T)

# Creating RR
RR1 <- Pr_1 / Pr_0
quantile(RR1,c(.5,.025,.975))

# checking to make sure OR agrees with above
OR1<-(Pr_1/(1-Pr_1))/(Pr_0/(1-Pr_0))
quantile(OR1,c(.5,.025,.975))

write.csv(df1,file="G:/My Drive/Avritscher/InpatientTrial/InpatientFinalData.csv",row.names = F)
