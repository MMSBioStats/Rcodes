library(swCRTdesign)
library(arm)
library(gee)
rm(list=ls())

# 15 centers, 1 centers cross over every 6 weeks
# total of 24 mon and 16 time periods
# assume 2/center/month

# BRIEF trial to increase enrollment into CPT trial

# design -----
des1<-swDsn( clusters=rep(1,15 ))
# des2<-swDsn( clusters=c(0,2,2,2,2,2),extra.time = 2 )

# des2<-swDsn( clusters=c(2,2,2,2,2),extra.time = 2 )



# Binary outcome ---------------
tau.sd<-sqrt(.033)
pc<-.55          #control rate
pt<-.68       # intervention rate
nperiod<-2.5    #number patients/center/time period

swPwr1 <- swPwr(des1, 
                distn="binomial",
                n=4, mu0=pc, mu1=pt,
                cac=.11,icc=.01,
                # tau=tau.sd, eta=0,
                # 
                # rho=0,gamma=0,# tau=s2, 
                #                  eta=0, rho=0,gamma=0,
                alpha=.05, retDATA = T)

swPwr1$pwrCLOSED

# simulate -----
# generate SW data 
# sanity check looking at implied data for assumed design 

pc<-.45          #control rate
pt<-.62    # intervention rate

swGenData.nScalar <- swSim( des1,
                            family=binomial(link="logit"), 
                            n=2,
                            mu0=logit(pc), 
                            mu1=logit(pt),
                             tau=tau.sd, eta=0,
                            
                            rho=0,gamma=0,time.effect = 0,
                            retTimeOnTx=FALSE)


swSummary(response.var, tx.var, time.var, cluster.var, swGenData.nScalar, 
          type="n", digits=3)$response.cluster

swSummary(response.var, tx.var, time.var, cluster.var, swGenData.nScalar, 
          type="n", digits=3)$swDsn

nsim<-2000
#pp_12<-rep(NA,1000)
pval<-rep(NA,nsim)
pp<-matrix(NA,nrow=nsim,3)

for(i in 1:nsim){
simd <- swSim( des1,
               family=binomial(link="logit"), n=2,
               mu0=logit(pc), 
               mu1=logit(pt),
               tau=.2, eta=0,
               rho=0,gamma=0,
               time.effect = 0,
               retTimeOnTx=FALSE)
m1<-glmer(response.var~tx.var+time.var+(1|cluster.var),
          family=binomial,data=simd)
# summary(m1)
pval[i]<-summary(m1)$coef["tx.var","Pr(>|z|)"]

p0<-mean(simd$response.var[simd$tx.var==0])
inc_5<-log(((p0+.05)/(1-p0-.05))/(p0/(1-p0)))
inc_10<-log(((p0+.10)/(1-p0-.1))/(p0/(1-p0)))


# simd<-simd[order(simd$cluster.var),]
# m1<-gee(response.var~tx.var+time.var,id=cluster.var,
#         family=binomial,corstr = "exchangeable",data=simd)

pp[i,]<-pnorm(c(0,inc_5,inc_10),mean=summary(m1)$coef["tx.var",1],sd=summary(m1)$coef["tx.var",2],lower=F)
}

# frequentist
mean(pval<.05)

# Bayesian power
mean(pp[,1]>.9)
mean(pp[,2]>.85)
mean(pp[,3]>.8)

simd<-simd[order(simd$cluster.var),]
m1<-gee(response.var~tx.var+time.var,id=cluster.var,
          family=binomial,corstr = "exchangeable",data=simd)

# RESULTS -----
# > pc
# [1] 0.04
# > pt
# [1] 0.03
# > icc
# [1] 0.01030916
# > mean(pp_12>.9)
# [1] 0.82
# > mean(pp_12>.8)
# [1] 0.911
# > mean(pp_12>.7)
# [1] 0.954

# > pc
# [1] 0.04
# > pt
# [1] 0.027
# > icc
# [1] 0.01030916
# > mean(pp_12>.9)
# [1] 0.951
# > mean(pp_12>.8)
# [1] 0.983
# > mean(pp_12>.7)
# [1] 0.991

# > pc
# [1] 0.06
# > pt
# [1] 0.045
# > icc
# [1] 0.05089823
# > mean(pp_12>.9)
# [1] 0.919
# > mean(pp_12>.8)
# [1] 0.967
# > mean(pp_12>.7)
# [1] 0.985

# > pc
# [1] 0.04
# > pt
# [1] 0.03
# > icc
# [1] 0.05089823
# > s2
# [1] 0.02
# > mean(pp_10>.9)
# [1] 0.704
# > mean(pp_10>.8)
# [1] 0.836
# > mean(pp_10>.7)
# [1] 0.908

library(swdpwr)

dataset = matrix(c(rep(c(0,1,1),6),rep(c(0,0,1),6)),12,3,byrow=TRUE)

#specify meanresponse_start, meanresponse_end0 and meanresponse_end1
swdpower(K = 2, design = des1$swDsn, family = "binomial", model = "marginal", link = "identity",
         type = "cross-sectional", meanresponse_start = 0.55, 
         meanresponse_end1 = 0.68, typeIerror = 0.05, alpha0 = 0.01,
         alpha1 = 0.01) 

