library(lme4)
library(arm)

rm(list=ls())

nsub=400

time<-rep(1:2,nsub)
person<-rep(1:nsub,each=2)
#sub<-rep(person,2)

group<-c(rep(0:1,nsub/2),rep(c(1,1),nsub/2))
group1<-c(rep(1,nsub),rep(2,nsub))

mu1<-12
mu1<-0.45
mu.a<-log(mu1)

sigma.a.true<-.6

b.true<-log(.67)*group
sig2=15^2

b.true<-log(.7)*group
sig2=1.3

nsim<-250
priorsd1<-.6
priormu1<-0
exp(qnorm(c(.5,.025,.975),priormu1,priorsd1))

probs2<-probs<-sig<-rep(NA,nsim)

for(i in 1:nsim){
a.true<-rnorm(nsub,mu.a,sigma.a.true)
mu.y<-exp(a.true[person]+b.true)

theta.y=mu.y^2/(sig2-mu.y)

y<-rnegbin(nsub*2,mu.y,theta.y)

datafake<-data.frame(y,person,group,time,cluster=group1)

#m1<-glmer.nb(y~group+as.factor(time)+(1|person),data=datafake)
m1<-glmer(y~group+as.factor(time)+(1|person),data=datafake,family=poisson)

sig[i]<-summary(m1)$coef[2,4]<0.05

obsmu<-fixef(m1)[2]
obssd<-summary(m1)$coef[2,2]
postvar1<-1/(1/priorsd1^2+1/obssd^2)
postmu1<-(priormu1/(priorsd1^2)+obsmu/(obssd^2))*postvar1
probs[i]<-pnorm(0,mean=postmu1,sd=sqrt(postvar1),lower=T)
probs2[i]<-pnorm(log(1.05),mean=postmu1,sd=sqrt(postvar1),lower=T)

}

mean(sig)
mean(probs>.9)
mean(probs>.85)
mean(probs>.8)

mean(probs>.75)
mean(probs>.67)


p1<-ggplot(datafake,aes(x=time,y=y,group=person))+geom_line(show.legend = F)+
  facet_wrap(~cluster)

