rm(list = ls())

# 2020 US$ would be $110,556 (SD=77,677). 
mu<-168000
tau2<-55000^2
a=mu^2/tau2
s=tau2/mu

y1<-rgamma(10000,shape=a,scale=s)
# hist(y1)
summary(y1)
sd(y1)

mu2<-mu-90000
a2=mu2^2/tau2
s2=tau2/mu2

ngroup=300

nsim<-1000
probs<-res<-rep(NA,nsim)
#priormu1<-0

priorsd1<-.6
priormu1<-0
exp(qnorm(c(.5,.025,.975),priormu1,priorsd1))

for(i in 1:nsim){
  y1<-rgamma(ngroup,shape=a,scale=s)
  y2<-rgamma(ngroup,shape=a2,scale=s2)
  
  d1<-data.frame(y=c(y1,y2),group=rep(c(0,1),each=ngroup))
  m1<-glm(y~group,family=Gamma(link = "log"),data=d1)
  res[i]<-summary(m1)$coef[2,4]<.05
  
  obsmu<-coef(m1)[2]
  obssd<-summary(m1)$coef[2,2]
  postvar1<-1/(1/priorsd1^2+1/obssd^2)
  postmu1<-(priormu1/(priorsd1^2)+obsmu/(obssd^2))*postvar1
  probs[i]<-pnorm(0,mean=postmu1,sd=sqrt(postvar1),lower=T)
  
  # m2<-stan_glm.nb(y~group,data=d1,link='log',prior=normal(0,.57),chains=3)
  # beta<-as.matrix((m2))
  # probs[i]<-mean(beta[,"group"]>0)
  
}
cat(paste("Assuming means of ",mu," vs ", mu2, ", SD of ",sqrt(tau2),
          ", \nand ",ngroup, " patients per group, power is:",sep=""))
cat(paste("Frequentist: ",mean(res),sep=""))
cat(paste("Bayesian with 95% threshold: ",mean(probs>.95),sep=""))

cat(paste("Bayesian with 90% threshold: ",mean(probs>.9),sep=""))
cat(paste("Bayesian with 85% threshold: ",mean(probs>.85),sep=""))


cat(paste("Bayesian with 80% threshold: ",mean(probs>.8),sep=""))



