library(lme4)
# library(arm)

rm(list=ls())

nsub=100

time<-rep(1:2,nsub)
person<-rep(1:nsub,each=2)
sub<-rep(person,2)

# group<-sample(rep(0:1,c(30,90)))
# group1<-rep(group,each=2)
# group2<-c(group1, group1*(-1)+1)

#mu.a.drug<-2.2
mu.a<-16.5
b.x<-3.8

#mu.a.placebo<-2.7
#sigma.a.true<-29

#b.true<-0.55*group2+1*(1-group2)
b.time<-0.3
# b.module<-
  #(-23)*group1+(-10)*(1-group1)

sigma.y<-5.1
rho<-0.6
#sigma.a.true^2/(sigma.a.true^2+sigma.y^2)
sigma.a<-sqrt((rho*sigma.y^2)/(1-rho))
  
nsim<-500

beta<-sig<-rep(NA,nsim)

for(i in 1:nsim){
# module<-rnorm(nsub,module.mean,sigma.module)
x<-rnorm(nsub,3.4,.8)
x.n<-rep(x,each=2)
# x<-runif(nsub)
a.true<-rnorm(nsub,mu.a,sigma.a)
ymean<-a.true[person]+b.time*(time-1)+b.x*x.n
y<-rnorm(nsub*2,ymean,sigma.y)

datafake<-data.frame(y,person,x1=rep(x,each=2),time=time-1)

fit1<-lmer(y~x1+(time)+(1|person),data=datafake)
sig[i]<-summary(fit1)$coef[2,5]<0.005
beta[i]<-summary(fit1)$coef[2,1]
}

#Power
mean(sig)
mean(beta)

p1<-ggplot(datafake,aes(x=time,y=y,group=person))+geom_line(show.legend = F)
+
  facet_wrap(~group1)
