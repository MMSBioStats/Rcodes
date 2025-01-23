rm(list=ls())
source("/Users/cpedroza/Google Drive/RCode/betaparamsfunctions.R")

#library("SampleSizeProportions")

pcontrol<-.02
a.c<-90*pcontrol
b.c<-90-a.c
qbeta(c(.025,.975,.5),a.c,b.c)

# betas.control<-beta.params(pcontrol,2)
# 
# p1<-rbeta(10000,betas.control[1],betas.control[2])
# p2<-rbeta(10000,betas.control[1],betas.control[2])
# quantile(p1/p2,c(.5,.025,.975))

RR.analysis<-1.235
RR.true<-1.235
# ntot<-round(sum(betas.control))
# 
  
 a1.analysis<-(ntot)*pcontrol*RR.true
 b1.analysis<-ntot-a1

a1.analysis<-.5
b1.analysis<-.5

p1<-rbeta(100000,a.c,b.c)
p2<-rbeta(100000,a1.analysis,b1.analysis)
quantile(p2/p1,c(.5,.025,.975))
# 
# qbeta(c(.025,.975,.5),a1,b1)

a1.design<-90/4*.04

  #pcontrol*RR.analysis
b1.design<-90/4-a1.design
qbeta(c(.025,.975,.5),a1.design,b1.design)

p1<-rbeta(100000,a.c,b.c)
p2<-rbeta(100000,a1.design,b1.design)
quantile(p2/p1,c(.5,.025,.975))
mean(p2-p1>0.1)

# #propdiff.alc(.05,betas.control[1],betas.control[2],a1,b1,level=0.95)
# 
# # p1<-rbeta(10000,betas.control[1],betas.control[2])
# # 
# p1<-rbeta(10000,a.c,b.c)
# 
# p2<-rbeta(10000,a1,b1)
# 
# # 
# RR<-p2/p1
# 
# quantile(RR,c(.5,.025,.975))

#a1.design<-b1.design<-a.c<-b.c<-1

n1<-n2<-52

nsim<-1000

sig<-rep(NA,nsim)
sig3<-sig2<-rep(NA,nsim)

for(i in 1:nsim){
#  p1<-rbeta(1,betas.control[1],betas.control[2])
 # p1<-rbeta(1,a.c,b.c)
#  p2<-rbeta(1,a1.design,b1.design)
  p1<-0.02
  p2<-0.02
  x1 <- rbinom(1, n1, p1)
  x2 <- rbinom(1, n2, p2)
  
  #posterior
#   c1.control<-x1+betas.control[1]
#   d1.control<-n1-x1+betas.control[2]

  c1.control<-x1+a.c
  d1.control<-n1-x1+b.c
  
  c1.treat<-x2+a1.design
  d1.treat<-n2-x2+b1.design

#   c1.control<-x1+1
#   d1.control<-n1-x1+1
#   
#   c1.treat<-x2+1
#   d1.treat<-n2-x2+1
  
  
  pc.post<-rbeta(10000,c1.control,d1.control)
  pt.post<-rbeta(10000,c1.treat,d1.treat)

  sig[i]<-mean((pt.post-pc.post)<0)
  sig2[i]<-mean((pt.post-pc.post)<.02)
sig3[i]<-mean((pt.post-pc.post)<.03)

  
#   sig[i]<-quantile(p2.post/p1.post,c(.975))<1
#   sig2[i]<-quantile(p2.post/p1.post,c(.975))<1.1
  
  }

mean(sig>.95)
mean(sig2>.67)
mean(sig2>.75)

mean(sig>.51)
mean(sig2>.6)
mean(sig3>.7)

mean(sig>.95)

mean(sig2>.67)

#---------------------------------------------------------
library(arm)
library(MASS)
library(BRugs)       
library(R2OpenBUGS) 

setwd("C:/Users/cpedroza/Documents/Blakely/R21/")

Sys.time()

nsim<-500
sig.prob<-matrix(NA,ncol=2,nrow=nsim)
sig.CI<-matrix(NA,ncol=2,nrow=nsim)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Model 1
#log RR ~ Normal(log(.6), s=.21) 

modelstring = "
# BUGS model specification begins here...
model {
for( i in 1 : nData ) {
y[i] ~ dbern (p.bound[i])
p.bound[i] <- max(0, min(1, mu[i]))
log(mu[i]) <-   b0   + b1*x[i]
}
b0 ~ dnorm( -2.302585 , 25 )
#b0 ~ dnorm( -2.120264 , 25 )
b1 ~ dnorm( -0.5108256 , 22.67574 )   
#b1 ~ dnorm( -0.6931472 , 22.67574 )   #log(.5)

}
# ... end BUGS model specification
" # close quote for modelstring
# Write model to a file:
writeLines(modelstring,con="modelOptim.txt")
modelCheck( "modelOptim.txt" )  #checking model 

#------------------------------------------------------------------------------
# Model 2
#log RR ~ Normal(0, s=.1) 

modelstring = "
# BUGS model specification begins here...
model {
for( i in 1 : nData ) {
y[i] ~ dbern (p.bound[i])
p.bound[i] <- max(0, min(1, mu[i]))
log(mu[i]) <-   b0   + b1*x[i]
}
b0 ~ dnorm( -2.302585 , 400 )
#b0 ~ dnorm( -2.120264 , 25 )
#b1 ~ dnorm( 0 , 25)T(,0.09531018)   
b1 ~ dnorm( 0 , 3.18)   

#b1 ~ dnorm( 0 , .001)   
}
# ... end BUGS model specification
" # close quote for modelstring
# Write model to a file:
writeLines(modelstring,con="modelSkep.txt")
modelCheck( "modelSkep.txt" )  #checking model 

ngroup<-150
treatment<-rep(c(1,0),each=ngroup)

niter<-5000
Sys.time()
for(i in 1:nsim){
  b0<-rnorm(1,log(.10),sd=0.05)
  b1<-rnorm(1,log(.6),sd=.21)
  
  p.ij<-exp(b0+b1*treatment)
  
  y<-rbinom(ngroup*2,1,p.ij)
  
  datalist = list(
    x = treatment ,
    y = y, 
    nData = ngroup*2 
  )
  
  m1<-glm(y~x,family=binomial,data=datalist)
  inits1 <- list(list(b0=coef(m1)[1],b1=coef(m1)[2]))
  
  #run the chains
 # M1<-bugs(data=datalist,inits1,model.file = "modelOptim.txt",para = c("b0", "b1"),n.chains=1,n.burnin=1000,debug=F ,n.iter=niter )
  M2<-bugs(data=datalist,inits1,model.file = "modelSkep.txt",para = c("b0", "b1"),n.chains=1,n.burnin=1000,debug=F ,n.iter=niter )
  
#  res<-sim.ests(M1)
#  res2<-sim.ests(M2)
#  sig.CI[i,1]<-quantile(M1$sims.matrix[,2],c(.95))
#  sig.CI[i,2]<-quantile(M2$sims.matrix[,2],c(.95))

#sig.CI[i,1]<-mean(M1$sims.matrix[,2])
sig.CI[i,2]<-mean(M2$sims.matrix[,2])

#sig.prob[i,1]<-mean(M1$sims.matrix[,2]<0)
  sig.prob[i,2]<-mean(M2$sims.matrix[,2]<0)
}

apply(sig.CI,2,mean)  

mean(sig.prob[,1]>=.8 )
mean(sig.prob[,2]>=.8 )

mean(sig.prob[,1]>=.8 & sig.CI[,1]<log(1.1))
mean(sig.prob[,2]>=.8 & sig.CI[,2]<log(1.1))

Sys.time()

