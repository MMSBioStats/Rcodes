library(MASS)
# SIMULATIONS FOR POWER -------------
rm(list=ls())

# Neg Bin with mean mu and variance mu + mu^2/theta.
#Group 1
# mean=13.53125  sd=10.60731  

mu1=13.5
# delta=mu1*.33
# theta1=1.8
#Group 2
delta<-.7
# mu2=mu1-delta
mu2=mu1*delta
# mu2=12.5
# sig2=mu1+mu1^2/theta1
sig2=11^2
theta1=mu1^2/(sig2-mu1)
theta2=mu2^2/(sig2-mu2)

#Sample size per group
ngroup1=100
ngroup2=100

nsim<-3000
res2<-res<-rep(NA,nsim)


for(i in 1:nsim){
  y1<-rnegbin(ngroup1,mu=mu1,theta=theta1)
  y2<-rnegbin(ngroup2,mu=mu2,theta=theta2)
  
  d1<-data.frame(y=c(y1,y2),group=rep(c(0,1),times=c(ngroup1,ngroup2)),fu=rep(3.4,ngroup1+ngroup2))
  m1<-glm.nb(y~group,data=d1,link='log')
  res[i]<-summary(m1)$coef["group","Pr(>|z|)"]<.05
  res2[i]<-wilcox.test(y~group,data=d1)$p.value<.05
  
}

mean(res)
mean(res2)


#Power under different thresholds
mean(probs>.85)
mean(probs>.8)
mean(probs>.75)
# mean(probs>.67)

ngroup1
ngroup2
mu1
mu2