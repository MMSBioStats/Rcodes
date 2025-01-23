# Stepped Wedge design w/binary outcome
# Noninferiority design

library(simstudy)
library(lme4)
library(arm)
# library(brms)

rm(list=ls())

log.or<-function(pnum,pden){
  log((pnum/(1-pnum))/(pden/(1-pden)))
}

# iNO rate 
p0<-.35
logit.p0<-logit(p0)
# [1] -1.734601

# NI margin 
delta<-.08
# treatment effect - log OR
trt.ef<-log.or(.3,.35)

# period effect -log OR
per.ef<-(.01)

# icc
icc<-0.01

var.re<-iccRE(icc,dist="binary")

defc <- defData(varname = "ceffect", formula = 0, variance = var.re, 
                dist = "normal", id = "cluster")
defc <- defData(defc, "m", formula = 10, dist = "nonrandom")

defa <- defDataAdd(varname = "y", 
                   formula = "..logit.p0 + trt *..trt.ef + period *..per.ef  + ceffect",
                  dist = "binary",link = "logit")
                  
#set.seed(608477)
nsim<-500
priorsd1<-.71
priormu1<-0
exp(qnorm(c(.5,.025,.975),priormu1,priorsd1))

dc <- genData(12, defc)
dp <- addPeriods(dc, 5, "cluster", timevarName = "t")
dp <- trtStepWedge(dp, "cluster", nWaves = 4, 
                   lenWaves = 1, 
                   startPer = 1, grpName = "trt")

dd <- genCluster(dp, cLevelVar = "timeID", "m", "id")
dd <- addColumns(defa, dd)
m1<-glmer(y~period+trt+(1|cluster),data=dd,family=binomial,
          nAGQ = 5)
pc<-invlogit(fixef(m1)[1])

probs<-sig<-rep(NA,nsim)

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
pc<-invlogit(fixef(m1)[1])

# Decreased outcome is worse
# sig[i]<-confint(m1,parm = "trt",level=.90,method="Wald")[1] > log.or(pc-delta,pc)

# Increased outcome is worse
sig[i]<-confint(m1,parm = "trt",level=.90,method="Wald")[2] < log.or(pc+delta,pc)

# if(!is.null(m1)){sig[i]<-summary(m1)$coef[3,4]<.05
obsmu<-fixef(m1)[3]
obssd<-summary(m1)$coef[3,2]
postvar1<-1/(1/priorsd1^2+1/obssd^2)
postmu1<-(priormu1/(priorsd1^2)+obsmu/(obssd^2))*postvar1

#Use this for Decreased outcome is worse
  # probs[i]<-pnorm(log.or(pc-delta,pc),mean=postmu1,sd=sqrt(postvar1),lower=F)
 
 #Use this for Increased outcome is worse
 probs[i]<-pnorm(0,mean=postmu1,sd=sqrt(postvar1),lower=T)
 
}
timestamp()

cat(paste("iNO rate =",p0," ICC=", icc, "n patients=",nrow(dd),"NI margin=", delta))

round(c(mean(sig),
mean(probs>.95),
mean(probs>.9),
mean(probs>.85),
mean(probs>.8),
mean(probs>.67)),2)




