# sample code to run bayes logistic and calculate RR
# prior for log OR is based on prior for RR
library(brms)
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

m1<-glmer(compositeoutcomeag~group*baseline_risk+(1|sibfactor),
          nAGQ = 10,family=binomial,data=df1)

m1<-geese(compositeoutcomeag~group*baseline_risk, id=sibfactor,
          family=binomial(link="identity"),data=df1,corstr = "exchangeable")
summary(m1)
# get_prior(compositeoutcomeag~group+baseline_risk+(1|sibfactor)+offset(log(followupdaysag)),
#                data=df1,family=binomial)
prior1<-c(set_prior("normal(0,1)", class = "b",coef="group"),
          set_prior("normal(0,1)", class = "b",coef="group:baseline_risk"),
                     set_prior("normal(0,1)", class = "b"),
           set_prior("normal(0,10)", class="Intercept"),     ## see if increasing intercept SD changes anything
           set_prior("normal(0,1)", class="sd"))
l1<-brm(compositeoutcomeag~group*baseline_risk+
          (1|sibfactor),
               data=df1,
               family=bernoulli,prior=prior1, 
               chains = 3, control=list(adapt_delta=.97), 
               warmup=3000,iter = 10000)
summary(l1)
betas<-as.matrix(l1,pars="group")
# OR summary
quantile(exp(betas[,1]),c(.5,.025,.975))
quantile(exp(betas[,1]+betas[,2]),c(.5,.025,.975))

# calculating predicted prob under control
nd <- df1[df1$baseline_risk==1,]
nd$group <- 0
Pr_0 <- posterior_linpred(l1, newdata = nd,transform = T)
quantile(Pr_0,c(.5,.025,.975))

# calculating predicted prob under treat
nd$group <- 1
Pr_1 <- posterior_linpred(l1, newdata = nd,transform = T)
quantile(Pr_1,c(.5,.025,.975))

nd <- df1[df1$baseline_risk==0,]
nd$group <- 0
Pr_0_0 <- posterior_linpred(l1, newdata = nd,transform = T)
quantile(Pr_0_0,c(.5,.025,.975))

# calculating predicted prob under treat
nd$group <- 1
Pr_1_0 <- posterior_linpred(l1, newdata = nd,transform = T)
quantile(Pr_1_0,c(.5,.025,.975))

quantile(Pr_1-Pr_0,c(.5,.025,.975))
quantile(Pr_1_0-Pr_0_0,c(.5,.025,.975))

# Creating RR
RR1 <- Pr_1 / Pr_0
quantile(RR1,c(.5,.025,.975))

# checking to make sure OR agrees with above
OR1<-(Pr_1/(1-Pr_1))/(Pr_0/(1-Pr_0))
quantile(OR1,c(.5,.025,.975))
  
OR1_0<-(Pr_1_0/(1-Pr_1_0))/(Pr_0_0/(1-Pr_0_0))
quantile(OR1_0,c(.5,.025,.975))
#--------------
nd1 = data.frame(n = 1, group = c(1, 0, 1, 0), 
                      
                     baseline_risk = c(0, 0, 1, 1))

lp <- posterior_linpred(l1,  newdata=nd1,transform = TRUE, re_formula = NA)

posterior_summary(lp[, 1] - lp[, 2],probs=c(.5,.025,.975)) #Lap - Drain in IP (low Risk)
posterior_summary(lp[, 3] - lp[, 4],probs=c(.5,.025,.975)) #Lap - Drain in NEC (low Risk)

s1<-stan_glmer(compositeoutcomeag~group*baseline_risk+
                 (1|sibfactor),
               data=df1,
               family=binomial,prior = normal(c(0,0,0),c(.56,1,1)),
               prior_intercept = normal(0,10),
               chains = 3, 
               warmup=3000,iter = 10000)
summary(s1)
