library(lme4)
library(arm)

rm(list=ls())

nsub=120

time<-rep(1:2,nsub)
person<-rep(1:nsub,each=2)
sub<-rep(person,2)

group<-sample(rep(0:1,c(30,90)))
group1<-rep(group,each=2)
group2<-c(group1, group1*(-1)+1)

#mu.a.drug<-2.2
mu.a.drug<-100

#mu.a.placebo<-2.7
sigma.a.true<-29

#b.true<-0.55*group2+1*(1-group2)
b.true<-(-23)*group1+(-10)*(1-group1)

sigma.y<-15

sigma.a.true^2/(sigma.a.true^2+sigma.y^2)

nsim<-1000

sig<-rep(NA,nsim)
for(i in 1:nsim){
a.true.drug<-rnorm(nsub,mu.a.drug,sigma.a.true)
#a.true.placebo<-rnorm(nsub,mu.a.placebo,sigma.a.true)
y<-rnorm(nsub*2,a.true.drug[person]+b.true*(time-1),sigma.y)

datafake<-data.frame(y,person,group1)

fit1<-lmer(y~group1*as.factor(time)+(1|person),data=datafake)
sig[i]<-(fixef(fit1)[4]+2*se.fixef(fit1)[4])<0
}

mean(sig)

p1<-ggplot(datafake,aes(x=time,y=y,group=person))+geom_line(show.legend = F)+
  facet_wrap(~group1)
