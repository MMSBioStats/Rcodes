rm(list=ls())

pcontrol<-.02
a.c<-90*pcontrol
b.c<-90-a.c
qbeta(c(.025,.975,.5),a.c,b.c)

a.c<-b.c<-a1.analysis<-1
b1.analysis<-1


#sample size --
n1<-n2<-270

#true rates for treat and control groups
pc<-0.01
pt<-0.01


nsim<-3000

sig<-sig3<-sig2<-rep(NA,nsim)

for(i in 1:nsim){
  x1 <- rbinom(1, n1, pc)
  x2 <- rbinom(1, n2, pt)
  
  #posterior
  c1.control<-x1+a.c
  d1.control<-n1-x1+b.c
  
  c1.treat<-x2+a1.analysis
  d1.treat<-n2-x2+b1.analysis

  pc.post<-rbeta(10000,c1.control,d1.control)
  pt.post<-rbeta(10000,c1.treat,d1.treat)

  # probabilities of risk difference being smaller than some margin
  sig[i]<-mean((pt.post-pc.post)<0.02)  #treat has lower rate than control
  sig2[i]<-mean((pt.post-pc.post)<.05)  #treat is no higher than 2% vs control
sig3[i]<-mean((pt.post-pc.post)<.04) #treat is no higher than 10% vs control


  }

# POWER FOR DIFFERENT POSTERIOR PROBABILITY THRESHOLDS ----
mean(sig>.9)
mean(sig>.85)
mean(sig>.8)
mean(sig>.75)
mean(sig>.65)

# 
mean(sig2>.8)
mean(sig2>.85)
mean(sig2>.9)

# 
mean(sig3>.95)
mean(sig3>.9)
mean(sig3>.85)
mean(sig3>.8)
mean(sig3>.75)
mean(sig3>.65)

nt<-30

nsim<-1000
sig2<-sig<-rep(NA,nsim)
for(i in 1:nsim){
x<-rnorm(nt,40,15)
y<-rnorm(nt,40,15)
t1<-t.test(x,y)
sig2[i]<-t1$conf.int[1]>(-10) & t1$conf.int[2]<(10)

sig[i]<-pnorm(10/t1$stderr,t1$statistic)-pnorm(-10/t1$stderr,t1$statistic)
}
mean(sig>.9)
mean(sig2)
