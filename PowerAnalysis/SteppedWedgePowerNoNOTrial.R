# Bayesian Power for No iNO trial
# PI: Harting
# Jan 2022

library(swCRTdesign)
library(arm)
library(geepack)
library(broom)

rm(list=ls())

log.or<-function(pnum,pden){
  log((pnum/(1-pnum))/(pden/(1-pden)))
}

# 12 centers, 3 centers cross over every 1 year
# total of 5 yrs and 5 time periods
# assume 10/center/year

# design -----
des<-swDsn( clusters=rep(3,4) )

# checking icc value -----
# p<-.35
# #s2<-.02^2
# s2<-.09^2
# v1<-s2*p^2*(1+exp(logit(.35)))^(-2)+p*(1-p)
# icc<-s2/(s2+v1)
# icc
# tau.sd<-sqrt(s2)

# tau.sd<-sqrt(0.03323099) # corresponds to ICC of 0.01 for rates of 35%
tau.sd<-.01
p_con<-.45          #control rate
p_tx<-.4    # intervention rate
nperiod<-10    #number patients/center/time period

delta<-.05  # non-inferiority margin

priorsd1<-1000
priormu1<-0
# exp(qnorm(c(.5,.025,.975),priormu1,priorsd1))
# 
# swPwr1 <- swPwr(des, 
#                 distn="binomial",
#                 n=nperiod, mu0=pc, mu1=pt,
#                 tau=tau.sd, eta=0,
#                 rho=0,gamma=0,
#                 alpha=.05, retDATA = T)
# 
# swPwr1$pwrCLOSED
# 
# simulate -----
# generate SW data 

# d1 <-swSim(design = design.bernoulli,
#            
#            family = binomial(link = "logit"), n = nperiod,
#            
#            mu0 = logit(pc), mu1 = logit(pt),time.effect = 0,
#            
#            tau = tau.sd, eta = 0, rho = 0, gamma = 0)
# 
# swSummary(response.var, tx.var, time.var, cluster.var, d1 , 
#           type="n", digits=3)$response.cluster
# 
# swSummary(response.var, tx.var, time.var, cluster.var, d1 , 
#           type="n", digits=3)$swDsn
# 
# m1.1<-glmer(response.var~tx.var+time.var+(1|cluster.var),
#           family=binomial,data=simd,nAGQ = 5)
# m1.2<-glm(response.var~tx.var+time.var,
#           family=binomial,data=simd)
# m1.3<-stan_glmer(response.var~tx.var+time.var+(1|cluster.var),
#                  family=binomial,data=simd)
# summary(m1.3)
# betas<-as.matrix(m1.3)
# mean(betas[,2]<log.or(.426,.346))
# 
# m1<-geese(response.var~tx.var+time.var,id=cluster.var,
#         family=binomial,corstr = "exchangeable",data=d1)

nsim<-100
res<-matrix(NA,ncol=12,nsim)
 my_data<-list()

for(i in 1:nsim){
simd <- swSim( des,
               family=binomial(link="logit"), n=nperiod,
               mu0=logit(p_con), 
               mu1=logit(p_tx),
               tau=tau.sd, eta=0,
               rho=0,gamma=0,time.effect = 0,
               retTimeOnTx=FALSE)
simd$sub<-rep(c(1:10),length=nrow(simd))

my_data[[i]]<-simd
d2<-simd[simd$time.var*100+simd$sub<306,]

m1<-geeglm(response.var~tx.var+time.var,id=cluster.var,
          family=binomial,corstr = "exchangeable",data=simd)
m2<-geeglm(response.var~tx.var+time.var,id=cluster.var,
           family=binomial,corstr = "exchangeable",data=d2)
pc<-invlogit(coef(m1)[1])
# pc<-(coef(m1)[1])

# Decreased outcome is worse
# sig[i]<-confint(m1,parm = "trt",level=.90,method="Wald")[1] > log.or(pc-delta,pc)

# Increased outcome is worse
sig<-tidy(m1,conf.int = T,conf.level = .95)$conf.high[2] < log.or(pc+delta,pc)

obsmu<-coef(m1)["tx.var"]
obs.var<-vcov(m1)["tx.var","tx.var"]
postvar1<-1/(1/priorsd1^2+1/obs.var)
postmu1<-(priormu1/(priorsd1^2)+obsmu/(obs.var))*postvar1

# postvar1<-obs.var
# postmu1<-obsmu

obsmu2<-coef(m2)["tx.var"]
obs.var2<-vcov(m2)["tx.var","tx.var"]
postvar2<-1/(1/priorsd1^2+1/obs.var2)
postmu2<-(priormu1/(priorsd1^2)+obsmu2/(obs.var2))*postvar2

# postvar2<-obs.var2
# postmu2<-obsmu2

#Use this for Decreased outcome is worse
# probs[i]<-pnorm(log.or(pc-delta,pc),mean=postmu1,sd=sqrt(postvar1),lower=F)
p.ni<-pnorm(log.or(pc+delta,pc),mean=postmu1,sd=sqrt(postvar1),lower=T)
# p.ni<-pnorm(.08,mean=postmu1,sd=sqrt(postvar1),lower=T)

p.sup<-pnorm(0,mean=postmu1,sd=sqrt(postvar1),lower=T)
p.early<-pnorm(0,mean=postmu2,sd=sqrt(postvar2),lower=T)

#Use this for Increased outcome is worse
# probs[i]<-pnorm(0,mean=postmu1,sd=sqrt(postvar1),lower=T)

#Observed risk diff
obs.txeff.full<-invlogit(coef(m1)[1])-invlogit(coef(m1)[1]+coef(m1)["tx.var"])
obs.txeff.early<-invlogit(coef(m2)[1])-invlogit(coef(m2)[1]+coef(m2)["tx.var"])

res[i,]<-c(p_con,p_tx,pc,invlogit(coef(m1)[1]+coef(m1)["tx.var"]),obs.txeff.full,obs.txeff.early,sqrt(postvar1),sig,p.ni,p.sup,p.early,1-p.early)

}

 
# Re-loading saved sims data results ----
 
 # p_con<-0.35
 # p_tx<-0.43
 # res.data<-read.csv(file=paste("G:/My Drive/Harting/Sims/Pcontrol",p_con,"Ptx",p_tx,".csv",sep=""),header = T)
 
stop.ben<-.98
stop.harm<-.9
ni.th<-.9
sup.th<-.85

# RESULTS -----

res.data<-data.frame(res)
colnames(res.data)<-c("p_ino","p_nono","obs_ino","obs_nono","obs.eff.full","obs.eff.early","post_sd",
                      "freq.sig","p.ni","p.sup","earlyben","earlyharm")

res.data$outcome<-NA
res.data$outcome<-ifelse(res.data$earlyben>stop.ben,
                         "EarlyBen",res.data$outcome)
res.data$outcome<-ifelse(res.data$earlyharm>stop.harm,
                         "EarlyHarm",res.data$outcome)
res.data$outcome<-ifelse(res.data$earlyben<stop.ben & 
                           res.data$earlyharm<stop.harm & 
                           res.data$p.ni>ni.th,"FullNI",
                         res.data$outcome)
res.data$outcome<-ifelse(res.data$earlyben<stop.ben &
                           res.data$earlyharm<stop.harm & 
                           res.data$p.ni<ni.th,"FullInf",
                         res.data$outcome)

res.data$sup<-ifelse(res.data$outcome=="EarlyBen" | res.data$p.sup>sup.th,1,0
                       )

res.data$ss<-ifelse(res.data$earlyben>stop.ben | res.data$earlyharm>stop.harm, 300,600)
# ---------------------

# min(res.data$obs.eff.full[res.data$p.ni>.9 & res.data$outcome=="FullNI"])
# 
# (res.data[res.data$obs.eff.full<(-.03) & res.data$p.ni>.9,])

# plot(res.data$obs.eff.full[res.data$outcome=="FullNI"],res.data$p.ni[res.data$outcome=="FullNI"])

# results<-apply(res.data[,c(1:2,6:10,12)],2,mean)
prob.res<-prop.table(table(res.data$outcome))
prob.res

#prob of NI
prob.res["EarlyBen"]+prob.res["FullNI"]

mean(res.data$freq.sig)

res.all<-c(p_con-p_tx,p_con,p_tx,prob.res["EarlyHarm"],prob.res["EarlyBen"],
           prob.res["EarlyBen"]+prob.res["FullNI"],
          mean(res.data$sup),mean(res.data$ss),
           mean(res.data$p.ni>.9),mean(res.data$p.ni>.9 & res.data$p.sup>.85))
res.all<-data.frame(t(res.all))
names(res.all)<-c("RiskDiff","iNO Rate","noiNO Rate","EarlyHarm","EarlyBen",
                  "NIPower","CombPower","MeanSS","FixedNIPow","FixedSupPow")

# Write results to file -----------------
write.csv(res.data,file=paste("G:/My Drive/Harting/Sims/Pcontrol",p_con,"Ptx",p_tx,".csv",sep=""),row.names=F)

write.csv(res.all,file=paste("G:/My Drive/Harting/Sims/RES_pcontrol",p_con,"ptx",p_tx,".csv",sep=""),row.names=F)


