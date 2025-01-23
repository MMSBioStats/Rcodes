# Bayesian analysis of STOP-IT trial
#  N Engl J Med 2015;372:1996-2005.
# DOI: 10.1056/NEJMoa1411162
#-----------------
ncontrol<-266
ntreat<-272

out_control<-128
out_treat<-117

ssi_control<-23
ssi_treat<-17

death_control<-2
death_treat<-3

ssr_control<-9
ssr_treat<-6
# y1<-rbern(360,.12)
# p1<-mean(y1)
# y2<-rbern(360,.15)
# p2<-mean(y2)
# 
# pdiff<-rnorm(10000,p2,sqrt(p2*(1-p2)/360))-rnorm(10000,p1,sqrt(p1*(1-p1)/360))
# plot(density(pdiff))


logRR<-log((out_treat/ntreat)/(out_control/ncontrol))

##OBSERVED RATES
 obsmu<-logRR
 obssd<-sqrt(1/out_treat-1/ntreat+1/out_control-1/ncontrol)
# 
# obsmu<-(-0.7579 )
# obssd<-0.4623
# 
# BACK CALCULATING FROM FREQUENTIST RR AND 95% CI--------
 # obsmu=log(obsRR)
# exp(obsmu +/- 1.96*obssd)=(LL,UL)
 
obsRR<-0.75
UL<-1.02
obsmu<-log(obsRR)
obssd<-(log(UL)-log(obsRR))/1.96

# obsmu<-log(.8)
# checking calculations
exp(obsmu+c(0,-1,1)*1.96*obssd)
exp(obsmu-1.96*obssd)

obsmu<-0.085913819
obssd<-0.05516043 
# 
# #-----------------------------
###NEUTRAL PRIOR w/ 95% CrI: 0.3-3.3 ------------
#-----------------------------
priormu1<-log(1)
priorsd1<-.7
exp(qnorm(c(0.5,.025,.975),mean=priormu1, sd=priorsd1))

postvar1<-1/(1/priorsd1^2+1/obssd^2)
postmu1<-(priormu1/(priorsd1^2)+obsmu/(obssd^2))*postvar1

# Bayesian results w/ NEUTRAL PRIOR
exp(qnorm(c(0.5,.025,.975),mean=postmu1, sd=sqrt(postvar1)))

pnorm(log(.14/.12),mean=postmu1, sd=sqrt(postvar1))

pnorm(0,mean=postmu1, sd=sqrt(postvar1))

pnorm(log(.9),mean=postmu1, sd=sqrt(postvar1))

# EXPERT PRIOR: RR OF 0.78, 95% CrI 0.55-1.09 ---------

priormu1<-log(.78)
priorsd1<-(log(1.09)-log(.78))/1.96
priormu1<-log(.89)
priorsd1<-.242

postvar1<-1/(1/priorsd1^2+1/obssd^2)
postmu1<-(priormu1/(priorsd1^2)+obsmu/(obssd^2))*postvar1
# Bayesian results w/ EXPERT PRIOR
exp(qnorm(c(0.5,.025,.975),mean=postmu1, sd=sqrt(postvar1)))


#-------------------------------------------------
pnorm(0,mean=postmu1,sd=sqrt(postvar1),lower=T)

pnorm(log(.8),mean=postmu1,sd=sqrt(postvar1))

pnorm(log(1.05),mean=postmu1,sd=sqrt(postvar1),lower=F)


y1<-rnorm(50000,postmu1,sd=sqrt(postvar1))
quantile(exp(y1),c(0.5,.025,.975))

pnorm(0,mean=postmu1,sd=sqrt(postvar1))
pnorm(log(.95),mean=postmu1,sd=sqrt(postvar1))
pnorm(log(.9),mean=postmu1,sd=sqrt(postvar1))

OR.cal<-function(p1,p2){
  (p1/(1-p1))/(p2/(1-p2))
}


