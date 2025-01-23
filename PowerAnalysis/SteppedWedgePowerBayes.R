library(swCRTdesign)
library(arm)
rm(list=ls())

# 5-6 centers, 1 centers cross over every 3 mo
# total of 21 mon and 7 time periods
# assume 50/center/month

# design -----
#des1<-swDsn( clusters=rep(2,6) )
des2<-swDsn( clusters=rep(1,18),extra.time = 1 )


# checking icc value -----
# icc=.01, p=.04, s2 (std dev)=.02
# icc=.05, p=.06, s2=.055
p<-.4
#s2<-.02^2
s2<-.1^2
v1<-s2*p^2*(1+exp(logit(.4)))^(-2)+p*(1-p)
icc<-s2/(s2+v1)
icc
tau.sd<-sqrt(s2)

pc<-.4          #control rate
pt<-.37         # intervention rate
nperiod<-10    #number patients/center/time period

swPwr1 <- swPwr(des2, 
                distn="binomial",
                n=nperiod, mu0=pc, mu1=pt, tau=tau.sd,gamma=0, 
                eta=0, rho=0, retDATA = T,alpha=.05)
swPwr1$pwrCLOSED

# simulate -----
# generate SW data 
# sanity check looking at implied data for assumed design 


swGenData.nScalar <- swSim( des2,
                            family=binomial(link="logit"), 
                            n=nperiod,
                            mu0=logit(pc), 
                            mu1=logit(pt),
                             tau=tau.sd, eta=0,
                            rho=0,time.effect = 0,gamma=0,
                            retTimeOnTx=FALSE)


swSummary(response.var, tx.var, time.var, cluster.var, swGenData.nScalar, 
          type="n", digits=3)$response.cluster

swSummary(response.var, tx.var, time.var, cluster.var, swGenData.nScalar, 
          type="n", digits=3)$swDsn
m1<-glmer(response.var~tx.var+time.var+(1|cluster.var),
          family=binomial,data=simd,nAGQ = 5)
OR<-function(p1,p2){
  or<-(p1/(1-p1))/(p2/(1-p2))
  return(or)
}
# CODE TO APPROXIMATE BAYESIAN POWER -------------------
pp_12<-rep(NA,1000)
pp_10<-rep(NA,1000)

for(i in 1:100){
simd <- swSim( des2,
               family=binomial(link="logit"), n=nperiod,
               mu0=logit(pc),
               mu1=logit(pt),
               tau=tau.sd, eta=0,
               rho=0,gamma=0,time.effect = 0,
               retTimeOnTx=FALSE)
p1<-mean(simd$response.var[simd$tx.var==0])
log.diff<-logit(p1+.02)-logit(p1)
m1<-glmer(response.var~tx.var+time.var+(1|cluster.var),
          family=binomial,data=simd,nAGQ = 5)
pp_10[i]<-pnorm(log.diff,mean=summary(m1)$coef[2,1],sd=summary(m1)$coef[2,2])
}
mean(pp_10>.9,na.rm=T)
mean(pp_10>.8,na.rm=T)
mean(pp_10>.75,na.rm=T)
# 
# simd<-simd[order(simd$cluster.var),]
# m1<-geese(response.var~tx.var+time.var,id=cluster.var,
#           family=binomial,data=simd)
# 


