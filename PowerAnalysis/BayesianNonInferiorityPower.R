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

# n1<-210     #100     #250
# a1<-.12*n1
# b1<-n1-a1
# qbeta(c(.025,.975),a1,b1)
# 
# prior1<-rbeta(10000,a1,b1)
# prior2<-rbeta(10000,a1,b1)
# priordiff<-prior1-prior2
# mean(priordiff<(-.15))

# a1<-b1<-4

a1<-b1<-1
nsim<-2000
ntot1<-ntot2<-20

pp1<-pp2<-pp3<-rep(NA,nsim)

for(i in 1:nsim){
y1<-rbinom(1,ntot1,.04)
y2<-rbinom(1,ntot2,.04)

p1<-rbeta(10000,y1+a1,ntot1-y1+b1)
p2<-rbeta(10000,y2+a1,ntot2-y2+b1)
pp2[i]<-mean(p2<.12)
pdiff<-p2-p1
pp1[i]<-mean(pdiff<(.08))
#pp2[i]<-prop.test(c(y1,y2),c(ntot1,ntot2),conf.level = .9)$conf.int[1]>(-.15)
#pp2[i]<-mean(pdiff>(-.05))
#pp3[i]<-mean(pdiff<0)
}

### 30
mean(pp1>.7)

mean(pp2>.7)

mean(pp2>.975)
mean(pp3>.975)

mean(pp1<.1)
mean(pp2<.05)

# ONE ARM DESIGN --------
a1<-b1<-1
nsim<-1000
ntot1<-55
pp1<-pp2<-pp3<-rep(NA,nsim)
for(i in 1:nsim){
  y1<-rbinom(1,ntot1,.15)
 # y2<-rbinom(1,ntot2,.12)
  
  p1<-rbeta(10000,y1+a1,ntot1-y1+b1)
#  p2<-rbeta(10000,y2+a1,ntot2-y2+b1)
#  pdiff<-p1-P2
  pdiff<-p1-.1
  pp1[i]<-mean(pdiff<(.15))
  #pp2[i]<-prop.test(c(y1,y2),c(ntot1,ntot2),conf.level = .9)$conf.int[1]>(-.15)
  #pp2[i]<-mean(pdiff>(-.05))
  #pp3[i]<-mean(pdiff<0)
}

### 30
mean(pp1>.8)
