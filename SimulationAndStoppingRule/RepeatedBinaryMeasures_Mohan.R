library(Hmisc)
library(arm)
ncase<-24
ncontrol<-80-ncase

rate.case<-.4
rate.cont<-.2
case.negrate<-ncase*(1-rate.case)/(ncase*(1-rate.case)+ncontrol*(1-rate.cont))

person<-1:(ncase+ncontrol)
id<-rep(1:(ncase+ncontrol),each=52)

b0<-logit(case.negrate)
b0<-logit(.25)
b3<-logit(rate.case)-logit(rate.cont)
b2<-0

sd_person<-.5

time<-rep(1:52,ncase+ncontrol)

nsim<-1000
sig<-rep(NA,nsim)

for(j in 1:nsim){
case<-rep(c(1,0),times=c(ncase,ncontrol))

abnorm.case<-sample(person[case==1],ncase*rate.case)
abnorm.control<-sample(person[case==0],ncontrol*rate.cont)
abnorms<-c(abnorm.case,abnorm.control)

yout<-treat<-exposure<-rep(0,(ncase+ncontrol)*52)
for(i in 1:length(abnorms)){
  tstart<-  sample(5:48,1)
exposure[id==abnorms[i]][tstart:(tstart+3)]<-1  
}

treat<-ifelse(person%in%c(abnorm.case,abnorm.control),1,0)

nu<-rnorm(length(person),0,sd_person)
nu.all<-rep(nu,each=52)

p1<-invlogit(b0+b3*treat)

y<-rbinom(ncase+ncontrol,1,p1)
ids.cases<-person[y==1]

ids.cases.abnorms<-ids.cases[ids.cases%in%abnorms]
ids.neg<-ids.cases[ids.cases%nin%abnorms]

for(i in 1:length(ids.cases.abnorms)){
  t_case<-max(time[id==ids.cases.abnorms[i] & exposure==1])
  yout[id==ids.cases.abnorms[i] & time>t_case]<-1
  }

for(i in 1:length(ids.neg)){
  tstart<-sample(5:48,1)
  yout[id==ids.neg[i] & time>tstart]<-1  
}

datafake<-data.frame(id,exposure,timeout=time,yout)

try({m1<-glmer(yout~exposure+(1|id),family=binomial,data=datafake)
sig[i]<-(summary(m1)$coef[2,3]<(-2))})

}

mean(sig)