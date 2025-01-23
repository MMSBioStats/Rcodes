library(lme4)
library(lmerTest)

rm(list=ls())

nsub=40

person<-rep(1:nsub,each=2)
sub<-rep(person,2)


group<-sample(rep(0:1,c(nsub/2,nsub/2)))

group1<-rep(group,each=2)
group2<-abs(group1-rep(c(1,0),length=length(group1)))


#mu.a.drug<-2.2
b0.true<-log(58)
mu.a.drug<-0

#mu.a.placebo<-2.7
sigma.a.true<-1

#b.true<-0.55*group2+1*(1-group2)
b.true<-log(.75)

sigma2.y<-17^2


# sigma.a.true^2/(sigma.a.true^2+sigma.y^2)

nsim<-100

sig<-rep(NA,nsim)

for(i in 1:nsim){
a.true.drug<-rnorm(nsub,mu.a.drug,sigma.a.true)
#a.true.placebo<-rnorm(nsub,mu.a.placebo,sigma.a.true)
mu.person<-exp(a.true.drug[person]+b.true*group2+b0.true)


n=mu.person^2/(sigma2.y-mu.person)

y<-rnbinom(nsub*2,size=n,mu=mu.person)

datafake<-data.frame(y,person,group2)
# datafake %>% 
#   gtsummary::tbl_summary(by=group2)
                         # ,
                         # statistic = list(y ~ "{mean} ({sd})"))


fit1<-glmer.nb(y~group2+(1|person),data=datafake)
# sig[i]<-(fixef(fit1)[4]+2*se.fixef(fit1)[4])<0 
sig[i]<-summary(fit1)$coef["group2",4]<.05
}

mean(sig)

# assuming Normal dist ------
rm(list = ls())
nsub=30

person<-rep(1:nsub,each=2)
sub<-rep(person,2)


group<-sample(rep(0:1,c(nsub/2,nsub/2)))

group1<-rep(group,each=2)
group2<-abs(group1-rep(c(1,0),length=length(group1)))


#mu.a.drug<-2.2
b0.true<-(58)
mu.a.drug<-0

#mu.a.placebo<-2.7
sigma.a.true<-8

#b.true<-0.55*group2+1*(1-group2)
b.true<-(-b0.true*(.05))

sigma2.y<-9^2


sigma.a.true^2/(sigma.a.true^2+sigma2.y)

nsim<-1000

sig<-rep(NA,nsim)

for(i in 1:nsim){
  a.true.drug<-rnorm(nsub,mu.a.drug,sigma.a.true)
  #a.true.placebo<-rnorm(nsub,mu.a.placebo,sigma.a.true)
  mu.person<-(a.true.drug[person]+b.true*group2+b0.true)
  
  
 
  y<-rnorm(nsub*2,mu.person,sd=sqrt(sigma2.y))
  
  datafake<-data.frame(y,person,group2)
  # datafake %>% 
  #   gtsummary::tbl_summary(by=group2)
  # ,
  # statistic = list(y ~ "{mean} ({sd})"))
  
  
  fit1<-lmer(y~group2+(1|person),data=datafake)
  # sig[i]<-(fixef(fit1)[4]+2*se.fixef(fit1)[4])<0
  sig[i]<-summary(fit1)$coef["group2",4]<.05
}

mean(sig)

# Second scenario ---------
rm(list = ls())
nsub=30

person<-rep(1:nsub,each=2)
sub<-rep(person,2)


group<-sample(rep(0:1,c(nsub/2,nsub/2)))

group1<-rep(group,each=2)
group2<-abs(group1-rep(c(1,0),length=length(group1)))


#mu.a.drug<-2.2
b0.true<-(39)
mu.a.drug<-0

#mu.a.placebo<-2.7
sigma.a.true<-8

#b.true<-0.55*group2+1*(1-group2)
b.true<-(-b0.true*(.05))

sigma2.y<-9^2


sigma.a.true^2/(sigma.a.true^2+sigma2.y)

nsim<-1000

sig<-rep(NA,nsim)

for(i in 1:nsim){
  a.true.drug<-rnorm(nsub,mu.a.drug,sigma.a.true)
  #a.true.placebo<-rnorm(nsub,mu.a.placebo,sigma.a.true)
  mu.person<-(a.true.drug[person]+b.true*group2+b0.true)
  
  
  
  y<-rnorm(nsub*2,mu.person,sd=sqrt(sigma2.y))
  
  datafake<-data.frame(y,person,group2)
  # datafake %>% 
  #   gtsummary::tbl_summary(by=group2)
  # ,
  # statistic = list(y ~ "{mean} ({sd})"))
  
  
  fit1<-lmer(y~group2+(1|person),data=datafake)
  # sig[i]<-(fixef(fit1)[4]+2*se.fixef(fit1)[4])<0
  sig[i]<-summary(fit1)$coef["group2",4]<.05
}

mean(sig)

