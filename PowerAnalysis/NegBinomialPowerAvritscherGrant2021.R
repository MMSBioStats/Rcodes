##
library(MASS)
#Packages to create tables
library(arsenal)
library(gtsummary)
# To change default stats
mycontrols  <- tableby.control(test=FALSE, 
                               numeric.stats=c("Nmiss", "meansd","medianq1q3"),
                               cat.stats=c("Nmiss","countpct"),
                               stats.labels=list(Nmiss='Missing', meansd="Mean (SD)",medianq1q3='Median (IQR)') )

final<-read.csv("G:/My Drive/Avritscher/TelemedicineTrial/Analysis/final_ver_2 (2).csv")

# dim(final)
# names(final)
# [1] "X"                         "ID"                       
# [3] "ER.Visit"                  "All.Admis."               
# [5] "Hosp.Days"                 "PICU"                     
# [7] "Readmission"               "Prolonged.hospitalization"
# [9] "Telemedicine"              "Regular.Visit"            
# [11] "Follow.up"                 "Provider.Name"            
# [13] "baseline.risk"             "comb.random.gr"           
# [15] "Age.group"                 "sib.ID"                   
# [17] "reason.withdrawn"          "outside_treatment"        
# [19] "WCC"                       "office.only"              
# [21] "PICU.los"                  "num_of_illness"           
# [23] "count.prolonged"           "PICU_binary"              
# [25] "followup.yrs"              "txgroup"                  
# [27] "po_rate"                   "po_rate2"                 
# [29] "baseline_risk"             "age_group"    

final$followup.yrs<-final$Follow.up/365.25
final$txgroup<-ifelse(final$comb.random.gr=="TELEMEDICINE with comprehensive care",1,0)

final$po_rate2<-final$outside_treatment/final$followup.yrs
final$si<-ifelse(final$num_of_illness>0,1,0)
final$si_rate<-final$si/final$followup.yrs
final$si_num_rate<-final$num_of_illness/final$followup.yrs

final$po_total<-round(final$po_rate2*3.4)
final$fu2<-rep(3.4,nrow(final))

final$baseline_risk<-as.factor(final$baseline.risk)
final$age_group<-as.factor(final$Age.group)

# If you wanted a table grouped by "HT" and for just a few variables:
t1<-tableby(txgroup~outside_treatment+po_total+followup.yrs+po_rate2+si+si_rate+si_num_rate,data=final,control=mycontrols,cat.simplify = T)

summary(t1,text=T)


m1<-glm.nb(outside_treatment~txgroup+baseline_risk+age_group+
             
             offset(log(followup.yrs)),data=final)
summary(m1)
 mean(fitted(m1)[final$txgroup==1])
# [1] 15.37536
sd(fitted(m1)[final$txgroup==1])
# [1] 9.875

m1<-glm.nb(num_of_illness~txgroup+baseline_risk+age_group+
             
             offset(log(followup.yrs)),data=final)
summary(m1)
mean(fitted(m1)[final$txgroup==1])
# [1] 0.6948605
sd(fitted(m1)[final$txgroup==1])
# [1] 0.7963735

library(glmmTMB)
form = num_of_illness~txgroup+baseline_risk+age_group+
    offset(log(followup.yrs))
ziform = ~txgroup+baseline_risk+age_group+
    offset(log(followup.yrs))

m4 <- glmmTMB(form,
              zi=~txgroup+baseline_risk+offset(log(followup.yrs)),
              family=nbinom2, data=final)

summary(m4)
mean(fitted(m4)[final$txgroup==0])
# [1] 0.6948605
sd(fitted(m4)[final$txgroup==1])

m1<-glm.nb(po_total~ txgroup+baseline_risk+age_group+
             
             offset(log(fu2)),data=final)
summary(m1)

library(emmeans)

emmeans(
  m1,
  ~ 1,
  type = "response",
  offset = log(3.4)  #  default
)

sims<-simulate(m1,nsim=1000)


summary(sims[,1])
# Simulations for Neg Binom Outcome data
# two groups
# assumes different means but use same sigma
# uses Normal approx of reg coeffs to calculate Bayesian power

library(MASS)
# library(rstanarm)
# options(mc.cores = parallel::detectCores())

# neg bin param: mean=mu, var=mu+mu^2/theta
# checking that simulated data makes sense
mu1=1.184
delta=mu1*.5
mu2=mu1-delta
mu2=.37*.5*800/250

sig2=1.7^2*(800^2/250^2)
theta1=mu1^2/(sig2-mu1)
theta2=mu2^2/(sig2-mu2)

mu1=  .6949

# theta1=mu1^2/(sig2-mu1)
theta1=.9
y1<-rnegbin(10000,mu=mu1,theta=theta1)
hist(y1)
summary(y1)
sd(y1)

y1<-rnbinom(10000,size=theta1,mu=mu1)
summary(y1)
sd(y1)
# timestamp()

# SIMULATIONS FOR POWER -------------
rm(list=ls())

#Group 1
mu1=12
# delta=mu1*.33
# theta1=1.8
#Group 2
delta<-.8
# mu2=mu1-delta
mu2=mu1*delta
# mu2=12.5
# sig2=mu1+mu1^2/theta1
sig2=9.875^2
 theta1=mu1^2/(sig2-mu1)
theta2=mu2^2/(sig2-mu2)

#Sample size per group
ngroup1=380/2
ngroup2=380/2

nsim<-5000
res<-probs<-rep(NA,nsim)

# Prior distribution 
priorsd1<-.6
priormu1<-0
exp(qnorm(c(.5,.025,.975),priormu1,priorsd1))

for(i in 1:nsim){
y1<-rnegbin(ngroup1,mu=mu1,theta=theta1)
y2<-rnegbin(ngroup2,mu=mu2,theta=theta2)

d1<-data.frame(y=c(y1,y2),group=rep(c(0,1),times=c(ngroup1,ngroup2)),fu=rep(3.4,ngroup1+ngroup2))
m1<-glm.nb(y~group,data=d1,link='log')
res[i]<-summary(m1)$coef[2,4]<.05

obsmu<-coef(m1)[2]
obssd<-summary(m1)$coef[2,2]
postvar1<-1/(1/priorsd1^2+1/obssd^2)
postmu1<-(priormu1/(priorsd1^2)+obsmu/(obssd^2))*postvar1
probs[i]<-pnorm(0,mean=postmu1,sd=sqrt(postvar1),lower=T)

# m2<-stan_glm.nb(y~group,data=d1,link='log',prior=normal(0,.57),chains=3)
# beta<-as.matrix((m2))
# probs[i]<-mean(beta[,"group"]>0)

}

mean(res)


#Power under different thresholds
mean(probs>.85)
mean(probs>.8)
mean(probs>.75)
# mean(probs>.67)

ngroup1
ngroup2
mu1
mu2
# sqrt(sig2)
#timestamp()

# SIMULATIONS FOR POWER SERIOUS ILLNESS EPISODES -------------
# Using zero inflated NB to simulate data
rm(list=ls())

mu1=1.3
delta=.67
# mu2=mu1-delta
mu2=mu1*delta

(mu2*(1-.395))/(mu1*(1-.51))
# sig2=1
# theta1=mu1^2/(sig2-mu1)
# theta2=mu2^2/(sig2-mu2)

#prob of zeros
p1<-.49
p2<-.605

# Mean SI rates for each group
mu1*p1
mu2*p2
# RR=(mu2*p2)/(mu1*p1)

theta2<-theta1<-.9
ngroup=380/2

nsim<-10000
probs<-res<-res2<-rep(NA,nsim)
#priormu1<-0

priorsd1<-.7
priormu1<-0


for(i in 1:nsim){
  z1 <- rbinom(n = ngroup, size = 1, prob = (1-p1)) 
  # mean(z == 0)
  y1<- ifelse(z1 == 0, 0, 
              rnegbin(ngroup,mu=mu1,theta=theta1))
  
  z2 <- rbinom(n = ngroup, size = 1, prob = (1-p2)) 
  # mean(z == 0)
  y2<- ifelse(z2 == 0, 0, 
              rnegbin(ngroup,mu=mu2,theta=theta2))
  

  d1<-data.frame(y=c(y1,y2),group=rep(c(0,1),each=ngroup))
  m1<-glm.nb(y~group,data=d1,link='log')
  res2[i]<-summary(m1)$coef[2,4]<.05
  # m4 <- glmmTMB(y~group,
  #               zi=~group,
  #               family=nbinom2, data=d1)
  # res[i]<-summary(m4)$coef$cond[2,4]<.05
  # 
  obsmu<-coef(m1)[2]
  obssd<-summary(m1)$coef[2,2]
  postvar1<-1/(1/priorsd1^2+1/obssd^2)
  postmu1<-(priormu1/(priorsd1^2)+obsmu/(obssd^2))*postvar1
  probs[i]<-pnorm(0,mean=postmu1,sd=sqrt(postvar1),lower=T)
  # probs2[i]<-pnorm(log(1.05),mean=postmu1,sd=sqrt(postvar1),lower=T)


}
# mean(res)
mean(res2)

mean(probs>.9)
mean(probs>.85)
mean(probs>.8)
# # 
# # 
# # ngroup
# # mu1
# # mu2
# # sqrt(sig2)
# # #timestamp()
# # 
# # 

sample(c(1:4),17,r=T)
