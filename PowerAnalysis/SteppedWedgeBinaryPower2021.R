# Stepped Wedge design w/binary outcome
# Noninferiority design

library(simstudy)
library(lme4)
library(arm)
# library(brms)

rm(list=ls())
or.cal<-function(pnum,pden){
  ((pnum/(1-pnum))/(pden/(1-pden)))
}

OR<-function(pnum,pden){
  ((pnum/(1-pnum))/(pden/(1-pden)))
}
log.or<-function(pnum,pden){
  log((pnum/(1-pnum))/(pden/(1-pden)))
}

# iNO rate 
p0<-.35
logit.p0<-logit(p0)
# [1] -1.734601

# deimplementation rate
p1<-0.43

# NI margin 
delta<-.08

# MCID 
mc<-.05
# treatment effect - log OR
trt.ef<-log.or(p1,p0)

# period effect -log OR
per.ef<-(.0)

# icc
icc<-0.01

var.re<-iccRE(icc,dist="binary")

defc <- defData(varname = "ceffect", formula = 0, variance = var.re, 
                dist = "normal", id = "cluster")
defc <- defData(defc, "m", formula = 5, dist = "nonrandom")

defa <- defDataAdd(varname = "osat", 
                   formula = "..logit.p0 + trt *..trt.ef + period *..per.ef  + ceffect",
                  dist = "binary",link = "logit")
                  
#set.seed(608477)
nsim<-1000
priorsd1<-1
priormu1<-0
exp(qnorm(c(.5,.025,.975),priormu1,priorsd1))

pr.mc<-pr.ni<-pr.sup<-sig<-sig.ni<-rep(NA,nsim)

timestamp()
for(i in 1:nsim){
dc <- genData(15, defc)
dp <- addPeriods(dc, 8, "cluster", timevarName = "t")
dp <- trtStepWedge(dp, "cluster", nWaves = 5, lenWaves = 1, 
                   startPer = 2, grpName = "trt")

dd <- genCluster(dp, cLevelVar = "timeID", "m", "id")
dd <- addColumns(defa, dd)


m1<-glmer(osat~period+trt+(1|cluster),data=dd,family=binomial,
          nAGQ = 5)
# emm.s <- emmeans(m1, "trt")

pINO<-invlogit(fixef(m1)["(Intercept)"])
pDE<-invlogit(fixef(m1)["(Intercept)"]+fixef(m1)["trt"])

sig[i]<-summary(m1)$coef["trt",4]<.05
# Decreased outcome is worse
# sig[i]<-confint(m1,parm = "trt",level=.90,method="Wald")[1] > log.or(pc-delta,pc)

# Increased outcome is worse
sig.ni[i]<-confint(m1,parm = "trt",level=.95,method="Wald")[2] < log.or(pINO+delta,pINO)

# if(!is.null(m1)){sig[i]<-summary(m1)$coef[3,4]<.05
obsmu<-fixef(m1)["trt"]
obssd<-summary(m1)$coef["trt",2]
postvar1<-1/(1/priorsd1^2+1/obssd^2)
postmu1<-(priormu1/(priorsd1^2)+obsmu/(obssd^2))*postvar1

# obsmu2<-summary(emm.s)[2,2]
# obssd2<-summary(emm.s)[2,3]
# postvar2<-1/(1/priorsd1^2+1/obssd2^2)
# postmu2<-(priormu1/(priorsd1^2)+obsmu2/(obssd2^2))*postvar2

#Use this for Decreased outcome is worse
  # probs[i]<-pnorm(log.or(pc-delta,pc),mean=postmu1,sd=sqrt(postvar1),lower=F)
 
 #Prob of deimplementation superior
 pr.sup[i]<-pnorm(0,mean=postmu1,sd=sqrt(postvar1),lower=F)

 #Prob pDE-pINO>MCID: benefit from iNO < MCID
  # pr.mc[i]<-pnorm(log.or(pDE,pDE-mc),mean=postmu1,
  #                 sd=sqrt(postvar1),lower=F)
  
  # pnorm(log.or(pINO+mc,pINO),mean=postmu1,
  #       sd=sqrt(postvar1),lower=F)

  #Prob pDE-pINO<delta: deimplementation non-inferior
  pr.ni[i]<-pnorm(log.or(pINO+delta,pINO),mean=postmu1,
                     sd=sqrt(postvar1),lower=T)
  
}
timestamp()

cat(paste("iNO rate =",p0,"deimple rate= ",p1, 
          " ICC=", icc, "n patients=",nrow(dd),
          "Min CID=", mc,
          " NI margin=",delta))

round(c(mean(sig.ni),
mean(pr.ni>.95),
mean(pr.ni>.9),
mean(pr.ni>.85),
mean(pr.ni>.8),
mean(pr.ni>.7)),3)

# round(c(mean(pr.mc>.95),
#         mean(pr.mc>.9),
#         mean(pr.mc>.85),
#         mean(pr.mc>.8),
#         mean(pr.mc>.7)),2)
# 
round(c(mean(sig),
        mean(pr.sup>.95),
        mean(pr.sup>.9),
        mean(pr.sup>.85),
        mean(pr.sup>.8),
        mean(pr.sup>.7)),2)




# SANITY CHECK FOR RESULTS ------------------------------------
# ---
lnpNO<-rnorm(100000,-0.853, 0.164  )
lnpDE<-rnorm(100000,-0.446, 0.16)

p1.s<-invlogit(lnpNO)
p2.s<-invlogit(lnpDE)
hist(p1.s)
hist(p2.s)
pdiff<-p2.s-p1.s
hist(pdiff)
summary(pdiff)
mean(pdiff<(.05))
mean(pdiff<(0))

mean(p2.s>(-p1.s+.05))
mean(p1.s<(p2.s+.05))

summary(p1.s)
summary(p2.s)
pnorm(0,mean=obsmu,sd=(obssd),lower=T)

pnorm(log.or(pINO+mc,pINO),mean=obsmu,sd=(obssd),
            lower=F)
pnorm(log.or(pINO+mc,pINO),mean=postmu1,
      sd=sqrt(postvar1),lower=T)