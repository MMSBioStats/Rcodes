library(MASS)
library(rstanarm)

options(mc.cores = parallel::detectCores())

# neg bin param: mean=mu, var=mu+mu^2/theta
mu=23
sig2=mu+(mu^2)/3.2

mu1=11.8
sig2=11^2
theta1=mu1^2/(sig2-mu1)

y1<-rnegbin(10000,mu=mu1,theta=theta1)
hist(y1)
summary(y1)
sd(y1)

timestamp()
# SIMULATIONS FOR POWER -------------
mu1=11.8
delta=5
mu2=11.8+delta

sig2=11^2
theta1=mu1^2/(sig2-mu1)

ngroup=80/2

nsim<-1000
probs<-res<-rep(NA,nsim)
priormu1<-0

priorsd1<-.61
for(i in 1:nsim){
y1<-rnegbin(ngroup,mu=mu1,theta=theta1)
y2<-rnegbin(ngroup,mu=mu2,theta=theta1)

d1<-data.frame(y=c(y1,y2),group=rep(c(0,1),each=ngroup))
m1<-glm.nb(y~group,data=d1,link='log')
res[i]<-summary(m1)$coef[2,4]<.05

obsmu<-coef(m1)[2]
obssd<-summary(m1)$coef[2,2]
postvar1<-1/(1/priorsd1^2+1/obssd^2)
postmu1<-(priormu1/(priorsd1^2)+obsmu/(obssd^2))*postvar1
probs[i]<-pnorm(0,mean=postmu1,sd=sqrt(postvar1),lower=F)

# m2<-stan_glm.nb(y~group,data=d1,link='log',prior=normal(0,.57),chains=3)
# beta<-as.matrix((m2))
# probs[i]<-mean(beta[,"group"]>0)

}
mean(res)
mean(probs>.9)
mean(probs>.8)
mean(probs>.75)
mean(probs>.67)

ngroup
mu1
mu2
sqrt(sig2)
timestamp()

