###########################
### BAYESIAN POWER CALCULATION FOR NON-INFERIORITY TRANSFUSION TRIAL
#### ASSUMING PRIORS FOR RATES OF PRIMARY OUTCOME CENTERED AT 10%
#### 95% CI: 6.7-14%, n=300/group gives 80% power
#### 95% CI: 5-16.5%, n=525/group gives 80% power

#### ASSUMING PRIORS FOR RATES OF PRIMARY OUTCOME CENTERED AT 12%
#### 95% CI: 8.3-16%, n=330/group gives 80% power
#### 95% CI: 6.4-19%, n=525/group gives 80% power
#### assuming a prior probability of 0.05 that difference is less than margin of 0.05, 
#### implies a prior sample size of 230, and n=350/group gives 80% power

#p1<-rbeta(10000,24.5,253.5-24)
#p2<-rbeta(10000,28.5,249.5-28)
#pdiff<-p1-p2

n1<-200     #100     #250
a1<-.12*n1
b1<-n1-a1
qbeta(c(.025,.975),a1,b1)

n2<-200    #100     #250
a2<-.14*n2
b2<-n2-a2
qbeta(c(.025,.975),a2,b2)

prior1<-rbeta(100000,a1,b1)
prior2<-rbeta(100000,a2,b2)
priordiff<-prior1-prior2
quantile(priordiff,c(.025,.975))

OR<-function(p1,p2){
  (p1/(1-p1))/(p2/(1-p2))
}
quantile(OR(prior1,prior2),c(.025,.975))

mean(OR(prior1,prior2))

plot(density(OR(prior1,prior2)),xlab="Odds Ratio",main="",lwd=3, xlim=c(0,2.2),
     ylim=c(0,2.2),col="red")

lines(density(log(OR(prior1,prior2))),col="red")

####
### analysis prior
nprio<-150
alpha1<-.12*nprio
beta1<-nprio-alpha1

nprio2<-150
alpha2<-.12*nprio2
beta2<-nprio2-alpha2
qbeta(c(.025,.975),alpha2,beta2)

qbeta(c(.025,.975),alpha1,beta1)
qbeta(c(.025,.975),alpha2,beta2)


 prior1<-rbeta(1000000,alpha1,beta1)
 prior2<-rbeta(1000000,alpha2,beta2)
quantile(OR(prior1,prior2),c(.025,.975))
mean(OR(prior1,prior2)<1)

plot(density(OR(prior1,prior2)),xlab="Odds Ratio",main="",lwd=3, xlim=c(0,2.2),ylim=c(0,3.5))

lines(density(OR(prior1,prior2)),lwd=3,col="black")
legend("topright",c("Neutral Prior","Enthusiastic Prior"),
       col=c("black","red"),lty=1,lwd=2,bty="n")

 priordiff<-prior1/prior2
quantile(priordiff,c(.025,.975))
# mean(priordiff<(-.05))

a1<-b1<-4

a1<-.1*alpha1
b1<-.1*beta1
qbeta(c(.025,.975),a1,b1)
prior1<-rbeta(10000,a1,b1)
prior2<-rbeta(10000,a1,b1)
priordiff<-prior1/prior2
quantile(priordiff,c(.025,.975))
#################
nsim<-1000
ntot1<-ntot2<-750
pp1<-<-pp3<-rep(NA,nsim)
for(i in 1:nsim){
  nprior<-700
  a1.d<-.12*nprior
  b1.d<-nprior-a1.d
  
  a2.d<-.11*nprior
  b2.d<-nprior-a2.d
  
  theta1<-rbeta(1,a1.d,b1.d)
  theta2<-rbeta(1,a2.d,b2.d)
#theta1<-.12
#theta2<-.17
y1<-rbinom(1,ntot1,theta1)
y2<-rbinom(1,ntot2,theta2)

p1<-rbeta(10000,y1+a1,ntot1-y1+b1)
p2<-rbeta(10000,y2+a2,ntot2-y2+b2)
#quantile(OR(p1,p2),c(.025,.975))
#mean(OR(p1,p2)<1)

#plot(density(OR(p1,p2)),xlab="Odds Ratio",main="",lwd=3)

p1.n<-rbeta(10000,y1+alpha1,ntot1-y1+beta1)
p2.n<-rbeta(10000,y2+alpha2,ntot2-y2+beta2)
#mean(OR(p1,p2)<1)

# lines(density(OR(p1,p2)),lwd=3,col="red")
# legend("topright",c("Neutral Prior","Posterior"),
#        col=c("black","red"),lty=1,lwd=2,bty="n")
# abline(v=1,lty=3,lwd=2)
# 
# p1<-rbeta(10000,y1+1,ntot1-y1+1)
# p2<-rbeta(10000,y2+1,ntot2-y2+1)
# mean(OR(p1,p2)<1)




#pp1[i]<-quantile(pdiff,.975)
#pp2[i]<-mean(pdiff>(-.05))
pp1[i]<-mean((p1.n-p2.n)<0)
pp3[i]<-mean((p1-p2)<0)
}

#mean(pp1<0)
mean(pp1>.8)
mean(pp3>.8)

### 30
mean(pp1>.975)
mean(pp2>.95)
mean(pp3>.95)

mean(pp1<.1)
mean(pp2<.05)

########################
p1<-rbeta(10000,24+a1,253-24+b1)
p2<-rbeta(10000,28+a1,249-28+b1)

pdiff<-p1-p2
mean(pdiff<(-.08))
mean(pdiff<(-.05))

######CHECKING FROM SLIDES

nsim<-1000
ntot1<-ntot2<-800
pp1<-pp2<-pp3<-rep(NA,nsim)
for(i in 1:nsim){
  theta1<-rbeta(1,a1,b1)
  theta2<-rbeta(1,a2,b2)
  
  y1<-rbinom(1,ntot1,theta1)
  y2<-rbinom(1,ntot2,theta2)
  
  p1<-rbeta(10000,y1+alpha1,ntot1-y1+beta1)
  p2<-rbeta(10000,y2+alpha1,ntot2-y2+beta1)
  pdiff<-p1-p2
  pp1[i]<-quantile(pdiff,.975)
  #pp2[i]<-mean(pdiff>(-.05))
  #pp3[i]<-mean(pdiff<0)
}

mean(pp1)
