# BAYESIAN SAMPLE SIZE APPROX FOR TWO PROPORTIONS
# USING NEUTRAL INFORMATIVE PRIOR FOR RR, 0.5-2

rm(list=ls())
ntreat<-ncontrol<-180/3

#sample size range of 300-1000

t.effect<-c(.7,.75,.8)
pcontrol<-0.4
ptreat<-pcontrol*t.effect
#ptreat<-.16

#-----------------------------
###NEUTRAL PRIOR w/ 95% CrI: 0.5-2.0
#-----------------------------

#priormu1<-log(.67)
priormu1<-0
priorsd1<-log(2)/(1.96)
priorsd1<-.7

priormu1 <-(-log(1.2))
priorsd1 =  .35 

# priormu1<-log(.85)
# 
# priorsd1<-.35

#priorsd1<-10000

results<-matrix(NA, nrow=length(t.effect),ncol=2)
nsim<-2000


pval<-sig2<-sig<-rep(NA,nsim)
#sig3<-sig2<-rep(NA,nsim)

for(k in 1:length(t.effect)){
for(i in 1:nsim){
  # ytreat <- rbern(ntreat, ptreat[k])
  # ytreat2 <- rbern(ntreat, ptreat[k])
  # 
  # ycontrol <- rbern( ncontrol, pcontrol)
  # d1<-data.frame(y=c(ycontrol,ytreat,ytreat2),
  #                treat=rep(c("A","B","C"),each=ntreat))
  # f1<-glm(y~treat,data = d1,family=binomial)
  # pval[i]<-anova(f1,test = "Chi")$"Pr"[2]
  ytreat <- rbinom(1,ntreat, ptreat[k])
  ytreat2 <- rbinom(1,ntreat, ptreat[k])

  ycontrol <- rbinom(1, ncontrol, pcontrol)
  
  
  logRR<-log((ytreat/ntreat)/(ycontrol/ncontrol))
  logRR2<-log((ytreat2/ntreat)/(ycontrol/ncontrol))

  ##OBSERVED RATES
  obsmu<-logRR
  obssd<-sqrt(1/ytreat-1/ntreat+1/ycontrol-1/ncontrol)

  obsmu2<-logRR2
  obssd2<-sqrt(1/ytreat2-1/ntreat+1/ycontrol-1/ncontrol)


postvar1<-1/(1/priorsd1^2+1/obssd^2)
postmu1<-(priormu1/(priorsd1^2)+obsmu/(obssd^2))*postvar1

postvar2<-1/(1/priorsd1^2+1/obssd2^2)
postmu2<-(priormu1/(priorsd1^2)+obsmu2/(obssd2^2))*postvar2

sig[i]<-pnorm(0,mean=postmu1,sd=sqrt(postvar1),lower=T)
sig2[i]<-pnorm(0,mean=postmu2,sd=sqrt(postvar2),lower=T)

}

results[k,]<-c(mean(sig>.9 | sig2>.9),mean(sig>.8 | sig2 >.8))
# results[k,]<-mean(pval<.05)

}

results2<-as.data.frame(round(results,2))

names(results2)<-c("90%","80%")
results2$effect<-t.effect
results2

