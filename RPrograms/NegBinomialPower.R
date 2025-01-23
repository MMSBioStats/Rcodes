days<-c(5,20,4,4,3,3,3,5,4,52,26,35,1,3,4,7,25,1,8,2,6)
mu=log(median(days))

s2=(log(mean(days))-mu)*2

vardays<-(exp(s2)-1)*exp(2*mu+s2)

yfake<-rlnorm(1000,mean(log(days)),sd(log(days)))

yfake<-rlnorm(10000,1.35,.95)

mu1<-mean(log(days))+.12
#sd1<-sd(log(days))
sd1<-1
mu2<-1.4
sd2<-.95

ntot<-60
sig<-sig2<-sig3<-sig4<-rep(NA,1000)
treat<-c(rep(0,ntot),rep(1,ntot))
for(i in 1:1000){
  y1<-rlnorm(ntot,mu1,sd1)
  y2<-rlnorm(ntot,mu2,sd2)
  y1[y1>60]<-60
  y2[y2>60]<-60
  
  yfake<-c(y1,y2)
  yfake2<-yfake
  yfake2[yfake==0]<-1
  test1<-wilcox.test(y1,y2,exact = FALSE, correct = FALSE,conf.int=T)
  #test1$p.value
  #m1<-glm(yfake2~treat,family=Gamma('log'))
  m2<-glm(log(yfake2)~treat)
  m3<-glm.nb(yfake~treat)
  
  sig[i]<-(test1$p.value<.05)
  # sig2[i]<-(summary(m1)$coef[2,4]<.05)
  sig3[i]<-(summary(m2)$coef[2,4]<.05)
  sig4[i]<-(summary(m3)$coef[2,4]<.05)
}

mean(sig)
#mean(sig2)
mean(sig3)
mean(sig4)


############################
#############
mu1=13.8
s2=6.9^2
theta1=mu1^2/(s2-mu1)

mu2=16.6
s2=6.9^2
theta2=mu2^2/(s2-mu2)

#yfake<-rnegbin(1000,mu2,theta2)

priormu1<-(0)
priorsd1<-log(2)/(1.96)

#clinically important diff
ES<-log(15.6/16.6)
ES2<-log(14.6/16.6)

ntot<-68
sig<-sig2<-sig3<-sig4<-sig5<-sig6<-rep(NA,1000)
treat<-c(rep(1,ntot),rep(0,ntot))
for(i in 1:1000){
  y1<-rnegbin(ntot,mu1,theta1)
  y2<-rnegbin(ntot,mu2,theta2)
  
  yfake<-c(y1,y2)
  yfake2<-yfake

  test1<-wilcox.test(y1,y2,exact = FALSE, correct = FALSE,conf.int=T)
  m2<-lm(yfake2~treat)
  m3<-glm.nb(yfake~treat)
  
  #bayesian approx w/neutral informative prior
  obsmu<-summary(m3)$coef[2,1]
  obssd<-summary(m3)$coef[2,2]
  postvar1<-1/(1/priorsd1^2+1/obssd^2)
  postmu1<-(priormu1/(priorsd1^2)+obsmu/(obssd^2))*postvar1
  
  
  sig2[i]<-pnorm(0,mean=postmu1,sd=sqrt(postvar1),lower=T)
  sig5[i]<-pnorm(ES,mean=postmu1,sd=sqrt(postvar1),lower=T)
  sig6[i]<-pnorm(ES2,mean=postmu1,sd=sqrt(postvar1),lower=T)
  
  sig[i]<-(test1$p.value<.05)
 #sig2[i]<-(summary(m1)$coef[2,4]<.05)
  sig3[i]<-(summary(m2)$coef[2,4]<.05)
  sig4[i]<-(summary(m3)$coef[2,4]<.05)
}

mean(sig)
#mean(sig2)
mean(sig3)
mean(sig4)
mean(sig5>.9)
mean(sig6>.9)

mean(sig2>.9)
