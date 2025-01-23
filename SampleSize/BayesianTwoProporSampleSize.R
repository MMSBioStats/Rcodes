rm(list=ls())
# survival to 6 months in the US Data in the contemporary managed group was 33%.  
# In the TOTAL Trial, that expectantly managed group (NONFETO) was 15%.

#Sample size per arm
n1<-n2<-3800

#assumed rates
p.control<-0.15
p.treat<-0.135

#difference to detect
p.diff<-0
#thresholds of posterior probability to declare success
p.thresh<-c(.8,.9)

#Setting the prior to use for both control & treatment arm
a.c<-36*p.control
b.c<-36-a.c
# qbeta(c(.025,.975,.5),a.c,b.c)

a.t<-a.c
b.t<-b.c

# Checking that assumed prior gives wanted RR
p1<-rbeta(100000,a.c,b.c)
p2<-rbeta(100000,a.c,b.c)
quantile(p2/p1,c(.5,.025,.975))
# # 

# a.c<-1
# b.c<-1

nsim<-1000

sig<-rep(NA,nsim)

for(i in 1:nsim){
   x1 <- rbinom(1, n1, p.control)
  x2 <- rbinom(1, n2, p.treat)
  
  c1.control<-x1+a.c
  d1.control<-n1-x1+b.c
  
  c1.treat<-x2+a.t
  d1.treat<-n2-x2+b.t


  pc.post<-rbeta(10000,c1.control,d1.control)
  pt.post<-rbeta(10000,c1.treat,d1.treat)

  sig[i]<-mean((pt.post-pc.post)<p.diff)

  
#   sig[i]<-quantile(p2.post/p1.post,c(.975))<1

  }

#Power for each success threshold
mean(sig>p.thresh[1])
mean(sig>p.thresh[2])


