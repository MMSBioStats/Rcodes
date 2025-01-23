# BAYESIAN ANALYSIS OF ECMO TRIAL, NEJM 2018
# PRIMARY OUTCOME: DEATH AT 60 DAYS

##OBSERVED RATES
rm(list=ls())

necmo<-124
ncontrol<-125

out_control<-57
out_ecmo<-44

logRR<-log((out_ecmo/necmo)/(out_control/ncontrol))

obsmu<-logRR
obssd<-sqrt(1/out_ecmo-1/necmo+1/out_control-1/ncontrol)

# r1<-1.16
# r2<-1.31
# obsmu<-log(r1)
# obssd<-(log(r2)-log(r1))/1.96

exp(obsmu+1.96*obssd)
exp(obsmu-1.96*obssd)

# matrix to hold results
res<-matrix(NA, nrow=2,ncol=10)
#obsmu<-(-.364)
#obssd<-0.115
#-----------------------------
###NEUTRAL PRIOR w/ 95% CrI: 0.5-2.0
#-----------------------------
priormu1<-(0)
priorsd1<-log(2)/(1.96)
#priorsd1<-0.57

postvar1<-1/(1/priorsd1^2+1/obssd^2)
postmu1<-(priormu1/(priorsd1^2)+obsmu/(obssd^2))*postvar1

res[1,1:3]<-exp(qnorm(c(0.5,.025,.975),mean=postmu1, sd=sqrt(postvar1)))

res[1,4:5]<-pnorm(c(0,log(.9)),mean=postmu1,sd=sqrt(postvar1))

#pnorm(log(1.05),mean=postmu1,sd=sqrt(postvar1),lower=F)

### PRIOR FROM CESAR TRIAL: 0.73, 0.52-1.03 
#-----------------------------
priormu2<-log(.73)
priorsd2<-(log(1.03)-log(.73))/(1.96)

postvar2<-1/(1/priorsd2^2+1/obssd^2)
postmu2<-(priormu2/(priorsd2^2)+obsmu/(obssd^2))*postvar2

res[2,1:3]<-exp(qnorm(c(0.5,.025,.975),mean=postmu2, sd=sqrt(postvar2)))

res[2,4:5]<-pnorm(c(0,log(.9)),mean=postmu2,sd=sqrt(postvar2))

# USING HR ESTIMATE: 0.7, 0.47-1.04 -------------
rm(obsmu,obssd)

r1<-0.7
r2<-1.04
obsmu<-log(r1)
obssd<-(log(r2)-log(r1))/1.96

#-----------------------------
###NEUTRAL PRIOR w/ 95% CrI: 0.5-2.0
#-----------------------------
priormu1<-(0)
priorsd1<-log(2)/(1.96)
#priorsd1<-0.57

postvar1<-1/(1/priorsd1^2+1/obssd^2)
postmu1<-(priormu1/(priorsd1^2)+obsmu/(obssd^2))*postvar1

res[1,6:8]<-exp(qnorm(c(0.5,.025,.975),mean=postmu1, sd=sqrt(postvar1)))

res[1,9:10]<-pnorm(c(0,log(.9)),mean=postmu1,sd=sqrt(postvar1))

### PRIOR FROM CESAR TRIAL: 0.73, 0.52-1.03 
#-----------------------------
priormu2<-log(.73)
priorsd2<-(log(1.03)-log(.73))/(1.96)

postvar2<-1/(1/priorsd2^2+1/obssd^2)
postmu2<-(priormu2/(priorsd2^2)+obsmu/(obssd^2))*postvar2

res[2,6:8]<-exp(qnorm(c(0.5,.025,.975),mean=postmu2, sd=sqrt(postvar2)))

res[2,9:10]<-pnorm(c(0,log(.9)),mean=postmu2,sd=sqrt(postvar2))


res2<-as.data.frame(res)
res2$Prior<-c("Neutral, 0, 0.5-2","CESAR TRIAL, 0.77, 0.6-0.95")
names(res2)[1:10]<-c("RR","95L","95U","Pr(RR<1)","Pr(RR<0.90)",
                     "HR","95L","95U","Pr(HR<1)","Pr(HR<0.90)")

write.csv(res2[,c(11,1:10)],file="/Users/cpedroza/Google Drive/Kao/ECMOBayesResultsJune2018.csv",row.names = F)

#---------------------------------
# BLEEDING ----------

ae_control<-35
ae_ecmo<-57

logRR<-log((ae_ecmo/necmo)/(ae_control/ncontrol))

obsmu<-logRR
obssd<-sqrt(1/ae_ecmo-1/necmo+1/ae_control-1/ncontrol)

# r1<-1.16
# r2<-1.31
# obsmu<-log(r1)
# obssd<-(log(r2)-log(r1))/1.96

exp(obsmu+1.96*obssd)
exp(obsmu-1.96*obssd)

# matrix to hold results
res.ae<-matrix(NA, nrow=1,ncol=5)
#obsmu<-(-.364)
#obssd<-0.115
#-----------------------------
###NEUTRAL PRIOR w/ 95% CrI: 0.5-2.0
#-----------------------------
priormu1<-(0)
priorsd1<-log(2)/(1.96)
#priorsd1<-0.57

postvar1<-1/(1/priorsd1^2+1/obssd^2)
postmu1<-(priormu1/(priorsd1^2)+obsmu/(obssd^2))*postvar1

res.ae[1,1:3]<-exp(qnorm(c(0.5,.025,.975),mean=postmu1, sd=sqrt(postvar1)))

res.ae[1,4:5]<-pnorm(c(0,log(1.2)),mean=postmu1,sd=sqrt(postvar1),lower=F)

#pnorm(log(1.05),mean=postmu1,sd=sqrt(postvar1),lower=F)


#---------

