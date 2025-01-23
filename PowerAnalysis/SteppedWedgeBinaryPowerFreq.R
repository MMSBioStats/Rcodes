# Stepped Wedge design w/binary outcome

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

# usual rate 
p0<-.157
logit.p0<-logit(p0)
# [1] -1.734601

# EQUITY rate
p1<-0.127

# treatment effect - log OR
trt.ef<-log.or(p1,p0)

# period effect -log OR
per.ef<-(.0)

# icc
icc<-0.01

var.re<-iccRE(icc,dist="binary")

defc <- defData(varname = "ceffect", formula = 0, variance = var.re, 
                dist = "normal", id = "cluster")

# Number of subjects per cluster per time point
defc <- defData(defc, "m", formula = 58, dist = "nonrandom")

defa <- defDataAdd(varname = "readmit", 
                   formula = "..logit.p0 + trt *..trt.ef + period *..per.ef  + ceffect",
                  dist = "binary",link = "logit")
                  
#set.seed(608477)
nsim<-1000
# priorsd1<-1
# priormu1<-0
# exp(qnorm(c(.5,.025,.975),priormu1,priorsd1))

sig<-rep(NA,nsim)

timestamp()
for(i in 1:nsim){
  # Generate random cluster effect; Number is num clusters
dc <- genData(10, defc)

#Number of total time periods
dp <- addPeriods(dc, 9, "cluster", timevarName = "t")

dp <- trtStepWedge(dp, "cluster", nWaves = 5, lenWaves = 1, 
                   startPer = 2, grpName = "trt")

dd <- genCluster(dp, cLevelVar = "timeID", "m", "id")
dd <- addColumns(defa, dd)


m1<-glmer(readmit~period+trt+(1|cluster),data=dd,family=binomial)
# summary(m1)

sig[i]<-summary(m1)$coef["trt",4]<.05

}
timestamp()

cat(paste("Usual Care rate =",p0,"Intervention Rate= ",p1, 
          " ICC=", icc, "n patients=",nrow(dd),"Power=",mean(sig)))


mean(sig)

round(c(mean(sig),
        mean(pr.sup>.95),
        mean(pr.sup>.9),
        mean(pr.sup>.85),
        mean(pr.sup>.8),
        mean(pr.sup>.7)),2)


# Number of patients for each procedure per hospital per year
#Colectomy
colec<-c(250,220,170,75,143,87,141,471,555,1150)
#Cholecystectomy
chole<-c(688,424,448,256,1272,680,472,288,1152,1152)

colec/chole

#Annual volume
vol<-c(21171,22872,14371,34384,19629,6854,35560,26596,25303,38896)


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