costdata<-read.csv("/Users/cpedroza/Documents/CostData.txt",head=T)
table(costdata$group,costdata$txassingnment)
boxplot(sumofcost~as.factor(txassingnment),data=costdata)
boxplot((sumofcost/followupyears)~as.factor(txassingnment),data=costdata)
boxplot(logcost~as.factor(txassingnment),data=costdata)

fit1<-lm(log(sumofcost+1)~as.factor(txassingnment),data=costdata)

fit2<-glm((sumofcost+1)~as.factor(txassingnment),family=Gamma(link="log"),data=costdata)

fit3<-glm((sumofcost+1)~as.factor(txassingnment)+offset(followupyears),family=Gamma(link="log"),data=costdata)

fit4<-glm((sumofcost+1)~as.factor(txassingnment)+(followupyears),family=Gamma(link="log"),data=costdata)

fit5<-glm(((sumofcost+1)/followupyears)~as.factor(txassingnment),family=Gamma(link="log"),data=costdata)
### FINAL MODEL GAMMA W/ INVERSE LINK
fit5.1<-glm(((sumofcost+1)/followupyears)~as.factor(txassingnment)+followupyears,family=Gamma,data=costdata)
fit5.2<-glm(((sumofcost+1)/followupyears)~as.factor(txassingnment)+followupyears,family=Gamma(link="log"),data=costdata)

fit6<-glm((log(sumofcost+1)/followupyears)~as.factor(txassingnment)+followupyears,data=costdata)
fit7<-glm(((sumofcost+1)/followupyears)~as.factor(txassingnment),family=Gamma,data=costdata)

m1<-bayesglm(((sumofcost+1)/followupyears)~as.factor(txassingnment)+followupyears,
             family=Gamma,data=costdata)
display(m1,digits=8)

m2<-bayesglm(((sumofcost+1)/followupyears)~as.factor(txassingnment)+followupyears,
             family=Gamma,data=costdata, prior.scale=.0001,prior.scale.for.intercept=.0001)
display(m2,digits=8)
## PROBABILITY OF DECREASED COSTS with COMPREHENSIVE CARE
pnorm(0,-0.00003047,0.00001401)
#[1] 0.9851803

xyplot(sumofcost~followupyears|as.factor(txassingnment),data=costdata)