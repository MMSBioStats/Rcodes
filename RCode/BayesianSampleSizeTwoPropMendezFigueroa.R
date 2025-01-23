rm(list=ls())
source("/Users/cpedroza/Google Drive/RCode/betaparamsfunctions.R")

#library("SampleSizeProportions")

pcontrol<-.075
a.c<-100*pcontrol
b.c<-100-a.c
qbeta(c(.025,.975,.5),a.c,b.c)

 betas.control<-beta.params(pcontrol,2)
# 
# p1<-rbeta(10000,betas.control[1],betas.control[2])
# p2<-rbeta(10000,betas.control[1],betas.control[2])
# quantile(p1/p2,c(.5,.025,.975))

# RR.analysis<-1.235
# RR.true<-1.235
 ntot<-round(sum(betas.control))
# 
  
 a1.analysis<-(ntot/3)*.125
#*RR.true
 b1.analysis<-ntot/3-a1.analysis

 a1.analysis<-.5
 b1.analysis<-.5



p1<-rbeta(100000,a.c,b.c)
p2<-rbeta(100000,a1.analysis,b1.analysis)
quantile(p2-p1,c(.5,.025,.975))
mean(p2-p1>0.05)
# 
# qbeta(c(.025,.975,.5),a1,b1)

a1.design<-200*0.15

  #pcontrol*RR.analysis
b1.design<-200-a1.design
qbeta(c(.025,.975,.5),a1.design,b1.design)

p1<-rbeta(100000,a.c,b.c)
p2<-rbeta(100000,a1.design,b1.design)
quantile(p2-p1,c(.5,.025,.975))
quantile(p2-p1,c(.5,.1,.9))

mean(p2-p1>0.05)

n1<-n2<-500

nsim<-1000

sig<-rep(NA,nsim)
sig3<-sig2<-rep(NA,nsim)

for(i in 1:nsim){
  p1<-rbeta(1,a.c,b.c)
  p2<-rbeta(1,a1.design,b1.design)
  x1 <- rbinom(1, n1, p1)
  x2 <- rbinom(1, n2, p2)
  
  c1.control<-x1+a.c
  d1.control<-n1-x1+b.c
  
#    c1.treat<-x2+a1.design
#    d1.treat<-n2-x2+b1.design

c1.treat<-x2+a1.analysis
d1.treat<-n2-x2+b1.analysis

  pc.post<-rbeta(10000,c1.control,d1.control)
  pt.post<-rbeta(10000,c1.treat,d1.treat)

  sig[i]<-mean((pt.post-pc.post)>0.025)
  sig2[i]<-quantile(pt.post-pc.post,c(.025))
sig3[i]<-mean((pt.post-pc.post)>.05)
  
  }

mean(sig2>0)

mean(sig>.8)
mean(sig2>.67)
mean(sig2>.75)
mean(sig2>.8)

mean(sig3>.5)


