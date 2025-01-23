library(Hmisc)
library(arm)
library(extraDistr)

rm(list=ls())

OR<-function(p1,p2){
  or<-(p1/(1-p1))/(p2/(1-p2))
  return(or)
}

# ncase<-14
# ntotal<-100
ncase<-19
ntotal<-64

ncontrol<-ntotal-ncase

rate.case<-.21
rate.cont<-.2
case.negrate<-ncase*(1-rate.case)/(ncase*(1-rate.case)+ncontrol*(1-rate.cont))
exp.rate<-.24

person<-1:(ncase+ncontrol)
id<-rep(1:(ncase+ncontrol),each=52)

b0<-logit(case.negrate)
b0<-logit(.26)
#b3<-logit(rate.case)-logit(rate.cont)
b3<-log(2.3)

# sd_person<-.5
sd_person<-sqrt(.08*.7/.3)


time<-rep(1:52,ncase+ncontrol)

nsim<-250
sig<-rep(NA,nsim)

for(j in 1:nsim){
# case<-rep(c(1,0),times=c(ncase,ncontrol))
# 
# abnorm.case<-sample(person[case==1],ncase*rate.case)
# abnorm.control<-sample(person[case==0],ncontrol*rate.cont)
# abnorms<-c(abnorm.case,abnorm.control)
abnorms<-rbern(ntotal,exp.rate)
exp.ids<-person[abnorms==1]

yout<-treat<-exposure<-rep(0,(ncase+ncontrol)*52)

for(i in 1:length(exp.ids)){
  tstart<-  sample(5:48,1)
exposure[id==exp.ids[i]][tstart:(tstart+3)]<-1  
}

# treat<-ifelse(person%in%c(abnorm.case,abnorm.control),1,0)

nu<-rnorm(length(person),0,sd_person)
nu.all<-rep(nu,each=52)

p1<-invlogit(b0+b3*exposure+nu.all)

y<-rbinom(ntotal*52,1,p1)
# ids.cases<-person[y==1]
# 
# ids.cases.abnorms<-ids.cases[ids.cases%in%abnorms]
# ids.neg<-ids.cases[ids.cases%nin%abnorms]

# for(i in 1:length(ids.cases.abnorms)){
#   t_case<-max(time[id==ids.cases.abnorms[i] & exposure==1])
#   yout[id==ids.cases.abnorms[i] & time>t_case]<-1
#   }
# 
# for(i in 1:length(ids.neg)){
#   tstart<-sample(5:48,1)
#   yout[id==ids.neg[i] & time>tstart]<-1  
# }

datafake<-data.frame(id,exposure,timeout=time,yout=y)

# try({m1<-glmer(yout~exposure+(1|id),family=binomial,data=datafake)
# sig[j]<-(summary(m1)$coef[2,3]>(2))})

m1<-glmer(yout~exposure+(1|id),family=binomial,data=datafake)
  sig[j]<-(summary(m1)$coef[2,3]>(2))
}

length(!is.na(sig))
mean(sig)
