library(swCRTdesign)
library(arm)
rm(list=ls())

# 5-6 centers, 1 centers cross over every 3 mo
# total of 21 mon and 7 time periods
# assume 50/center/month

# design -----
#des1<-swDsn( clusters=rep(2,6) )
des2<-swDsn( clusters=rep(1,5),extra.time = 2 )


# checking icc value -----
# icc=.01, p=.04, s2 (std dev)=.02
# icc=.05, p=.06, s2=.055
p<-.081
#s2<-.02^2
s2<-.05^2
v1<-s2*p^2*(1+exp(logit(.18)))^(-2)+p*(1-p)
icc<-s2/(s2+v1)
icc

pc<-.081          #control rate
pt<-.054         # intervention rate
nperiod<-70    #number patients/center/time period

swPwr1 <- swPwr(des2, 
                distn="binomial",
                n=nperiod, mu0=pc, mu1=pt, tau=s2, 
                eta=0, rho=0, retDATA = T,alpha=.05)
swPwr1$pwrCLOSED

# simulate -----
# generate SW data 
# sanity check looking at implied data for assumed design 


swGenData.nScalar <- swSim( des2,
                            family=binomial(link="logit"), n=nperiod,
                            mu0=logit(pc), 
                            mu1=logit(pt),
                             tau=s2, eta=0,
                            rho=0,seed=5,time.effect = 0,
                            retTimeOnTx=FALSE)


swSummary(response.var, tx.var, time.var, cluster.var, swGenData.nScalar, 
          type="n", digits=3)$response.cluster

swSummary(response.var, tx.var, time.var, cluster.var, swGenData.nScalar, 
          type="n", digits=3)$swDsn

# CODE TO APPROXIMATE BAYESIAN POWER -------------------
# pp_12<-rep(NA,1000)
# pp_10<-rep(NA,1000)
# 
# for(i in 1:1000){
# simd <- swSim( des2,
#                family=binomial(link="logit"), n=150,
#                mu0=logit(pc), 
#                mu1=logit(pt),
#                tau=s2, eta=0,
#                rho=0,time.effect = 0,
#                retTimeOnTx=FALSE)
# m1<-glmer(response.var~tx.var+time.var+(1|cluster.var),
#           family=binomial,data=simd,nAGQ = 5)
# pp_10[i]<-pnorm(0,mean=summary(m1)$coef[2,1],sd=summary(m1)$coef[2,2])
# }
# mean(pp_10>.9)
# mean(pp_10>.8)
# mean(pp_10>.7)
# 
# simd<-simd[order(simd$cluster.var),]
# m1<-geese(response.var~tx.var+time.var,id=cluster.var,
#           family=binomial,data=simd)
# 


