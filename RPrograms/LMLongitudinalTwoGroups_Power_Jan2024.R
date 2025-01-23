library(lme4)
library(lmerTest)
library(readxl)
library(dplyr)
library(gtsummary)
library(truncnorm)
library(ggplot2)
# library(arm)

rm(list=ls())

d<-read_xlsx("C:/Users/cpedroza/OneDrive - UTHealth Houston/Miyake/R01 stats_1.26.24.xlsx")
dim(d)
d<-data.frame(d)
names(d)
d<-rename(d,cat.two=Two.CAT..Category.0.not.on.vitamins.at.time.of.study.or.started.after.age.10...1.started.vitamins.before.age.10.years,cat.3=THREE.CAT..0.no.vitamins.or.started.after.age.10..1.started.in.childhood..3..started.as.toddler,age=Age.actual.at.time.of.Dp4..y.)
d$cat.3<-factor(d$cat.3)
levels(d$cat.3)<-c("0" ,"1","3","3")

t1<-d%>%select(cat.two,age,General.Development.Score)%>%tbl_summary(by=cat.two,statistic=all_continuous()~"{mean} ({sd})")%>%add_overall()
t1
m1<-lm(General.Development.Score~cat.two*age,data=d)
plot(d$age,d$General.Development.Score)
summary(m1)


# Simulations
nsub=100

time<-rep(1:3,nsub)
person<-rep(1:nsub,each=3)
sub<-rep(person,3)

group<-sample(0:1,nsub,replace = T,prob=c(.35,.65))
group1<-rep(group,each=3)
# group2<-c(group1, group1*(-1)+1)

#mu.a.drug<-2.2
mu.a<-44
# b.time<-0
mu.b<-77
b.group<-mu.b-mu.a
# sigma.a.true<-29

#b.true<-0.55*group2+1*(1-group2)
b.time<-6
# b.module<-
  #(-23)*group1+(-10)*(1-group1)

sigma.y<-13
rho<-0.4
#sigma.a.true^2/(sigma.a.true^2+sigma.y^2)
sigma.a<-sqrt((rho*sigma.y^2)/(1-rho))
  
nsim<-1000

beta<-sig<-rep(NA,nsim)

# ytest<-rtruncnorm(1000, a=0, b=Inf, mean = 44, sd = 20)
# mean(ytest)

for(i in 1:nsim){
a.true<-rtruncnorm(nsub,a=0,mean=mu.a+b.group*group,sd=sigma.a)
y<-rtruncnorm(nsub*3,a=0,mean=a.true[person]+group1*b.time*(time-1),sd=sigma.y)

datafake<-data.frame(y,person,group1,time=time-1)

fit1<-lmer(y~group1*time+(1|person),data=datafake)
sig[i]<-summary(fit1)$coef["group1:time",5]
beta[i]<-summary(fit1)$coef["group1:time",1]
}

#Power
mean(sig<.05)

p1<-ggplot(datafake,aes(x=time,y=y,group=person))+geom_line(show.legend = F)+
  facet_wrap(~group1)
p1


