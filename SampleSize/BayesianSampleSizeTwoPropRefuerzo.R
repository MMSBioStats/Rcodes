rm(list=ls())
source("/Users/cpedroza/Google Drive/RCode/betaparamsfunctions.R")

#library("SampleSizeProportions")

pcontrol<-.769
a.c<-100*pcontrol
b.c<-100-a.c
qbeta(c(.025,.975,.5),a.c,b.c)

betas.control<-beta.params(pcontrol,2)
ntot<-round(sum(betas.control))

  
 a1.analysis<-(ntot)*pcontrol
#*RR.true
 b1.analysis<-ntot-a1.analysis

# a1.analysis<-.5
# b1.analysis<-.5

p1<-rbeta(100000,a.c,b.c)
p2<-rbeta(100000,a1.analysis,b1.analysis)
quantile(p2/p1,c(.5,.025,.975))
# 
# qbeta(c(.025,.975,.5),a1,b1)

a1.design<-90/11*.95

  #pcontrol*RR.analysis
b1.design<-90/11-a1.design
qbeta(c(.025,.975,.5),a1.design,b1.design)

p1<-rbeta(100000,a.c,b.c)
p2<-rbeta(100000,a1.design,b1.design)
quantile(p2/p1,c(.5,.025,.975))
mean(p2-p1>0.1)

n1<-n2<-40

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

  sig[i]<-mean((pt.post-pc.post)>0)
  sig2[i]<-mean((pt.post-pc.post)>.1)
#sig3[i]<-mean((pt.post-pc.post)>.05)

}

#mean(sig>.95)
mean(sig2>.67)
mean(sig2>.75)
mean(sig2>.8)




