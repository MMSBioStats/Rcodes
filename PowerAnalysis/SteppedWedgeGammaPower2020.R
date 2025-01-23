defc <- defData(varname = "ceffect", formula = 0, variance = 0.20, 
                dist = "normal", id = "cluster")
defc <- defData(defc, "m", formula = 15, dist = "nonrandom")

defa <- defDataAdd(varname = "Y", 
                   formula = "0 + ceffect + 0.1*period + trt*1.5", 
                   variance = 1.75, dist = "normal")

set.seed(608477)

dc <- genData(30, defc)
dp <- addPeriods(dc, 24, "cluster", timevarName = "t")
dp <- trtStepWedge(dp, "cluster", nWaves = 5, lenWaves = 4, 
                   startPer = 4, grpName = "trt")

dd <- genCluster(dp, cLevelVar = "timeID", "m", "id")
dd <- addColumns(defa, dd)

dd

dSum <- dd[, .(Y = mean(cost)), keyby = .(cluster, period, trt, startTrt)]

ggplot(data = dSum, 
       aes(x = period, y = Y, group = interaction(cluster, trt))) +
  geom_line(aes(color = factor(trt))) +
  facet_grid(factor(startTrt, labels = c(2 : 5)) ~ .) +
   scale_x_continuous(breaks = seq(0, 7, by = 1), name = "Period") +
  scale_color_manual(values = c("#b8cce4", "#4e81ba")) +
  theme(panel.grid = element_blank(),
        legend.position = "none") 

# Gamma ----------
# install.packages("R2admb")
# install.packages("glmmADMB", repos = c("http://glmmadmb.r-forge.r-project.org/repos",
#                                        getOption("repos")), type = "source")

# library(glmmADMB)

library(simstudy)
library(lme4)
library(brms)

rm(list=ls())

mu<-168000
var1<-60000^2
disp1=var1/(mu^2)

log(150/168)
# [1] -0.1133287
log(148/168)
# [1] -0.1267517
log(146/168)
# [1] -0.1403574

var.cost<-iccRE(.1,dist="gamma",disp=disp1)

defc <- defData(varname = "ceffect", formula = 0, variance = var.cost, 
                dist = "normal", id = "cluster")
defc <- defData(defc, "m", formula = 5, dist = "nonrandom")

defa <- defDataAdd(varname = "cost", 
                   formula = "12.03172 + trt *(-0.1403574) + period *.0057  + ceffect",
                   variance = disp1, dist = "gamma",link = "log")
                  
prior2 <- c(set_prior("normal(0,0.61)",coef="trt"),
            
            set_prior("normal(0,1)", class = "b"),
            
            set_prior("normal(0,10)", class="Intercept"),
            set_prior("normal(0,1)", class="sd"))


options(mc.cores=3)

#set.seed(608477)
nsim<-200
priorsd1<-.6
priormu1<-0
exp(qnorm(c(.5,.025,.975),priormu1,priorsd1))


probs<-sig<-rep(NA,nsim)

timestamp()
for(i in 1:nsim){
dc <- genData(16, defc)
dp <- addPeriods(dc, 8, "cluster", timevarName = "t")
dp <- trtStepWedge(dp, "cluster", nWaves = 4, lenWaves = 1, 
                   startPer = 2, grpName = "trt")

dd <- genCluster(dp, cLevelVar = "timeID", "m", "id")
dd <- addColumns(defa, dd)

if(i==1){z1 <- brm(cost~period+trt+(1|cluster),data=dd,family=Gamma(link="log"), 
          prior=prior2,
          warmup = 1000, iter = 5000, chains=1, 
          control=list(adapt_delta=0.95))}
if(i>1){z2 <- update(z1,newdata=dd)
# summary(z2,digits=4)

draws<-posterior_samples(z2)
probs[i]<-mean(draws[,"b_trt"]<0)}

# dd

# m1<-glmer(cost~period+trt+(1|cluster),data=dd,family=Gamma(link="log"),nAGQ = 24)
# summary(m1)
# dd$cluster<-as.factor(dd$cluster)
# # m1<-glmmadmb(cost~period+trt+(1|cluster),data=dd,family="gamma")
# 
# m1 <- tryCatch(glmmadmb(cost~period+trt+(1|cluster),data=dd,family="gamma"), 
#                 error=function(e) NULL )
# # summary(m1)
# if(!is.null(m1)){sig[i]<-summary(m1)$coef[3,4]<.05
# obsmu<-fixef(m1)[3]
# obssd<-summary(m1)$coef[3,2]
# postvar1<-1/(1/priorsd1^2+1/obssd^2)
# postmu1<-(priormu1/(priorsd1^2)+obsmu/(obssd^2))*postvar1
# probs[i]<-pnorm(0,mean=postmu1,sd=sqrt(postvar1),lower=T)}

}
timestamp()

# mean(sig,na.rm=T)
mean(probs>.95,na.rm=T)
mean(probs>.9,na.rm=T)
mean(probs>.85,na.rm=T)

> sum(is.na(probs))
[1] 1
> # mean(sig,na.rm=T)
  > mean(probs>.95,na.rm=T)
[1] 0.8341709
> mean(probs>.9,na.rm=T)
[1] 0.8743719
> mean(probs>.85,na.rm=T)
[1] 0.8994975

# Results with ICC=0.2
# > sum(is.na(sig))
# [1] 71
# > mean(sig,na.rm=T)
# [1] 0.7596899
# > mean(probs>.95,na.rm=T)
# [1] 0.7984496
# > mean(probs>.9,na.rm=T)
# [1] 0.8837209
# > mean(probs>.85,na.rm=T)
# [1] 0.9224806

# Results with ICC=0.1
# > mean(sig,na.rm=T)
# [1] 0.8294574
# > mean(probs>.95,na.rm=T)
# [1] 0.8914729
# > mean(probs>.9,na.rm=T)
# [1] 0.9379845
# > mean(probs>.85,na.rm=T)
# [1] 0.9534884
# > mean(is.na(sig))
# [1] 0.355




z2 <- brm(cost~period+trt+(1|cluster),data=dd,family=Gamma(link="log"), 
      prior=prior2,
          warmup = 2000, iter = 10000, chains=1, 
          control=list(adapt_delta=0.95))

summary(z2,digits=4)

draws<-posterior_samples(z2)
RR<-exp(draws[,1])
quantile(RR,c(.5,.025,.975))
mean(draws[,1]<0)

dd$lncost<-log(dd$cost)
m2<-glmer(lncost~period+trt+(1|cluster),data=dd)
summary(m2)

#Frequentist power 67% for difference of 18,000

s1<-iccRE(.25,dist="gamma",disp = disp1)

def <- defData(varname = "a", formula = 0, variance = s1, 
               dist = "normal")
def <- defData(def, varname = "n", formula = 250, dist = "nonrandom")

defc <- defDataAdd(varname = "g", formula = "12 + a", 
                   variance = disp1, dist = "gamma", link = "log")

dt <- genData(1000, def)
dc <- genCluster(dt, "id", "n", "id1")
dc <- addColumns(defc, dc)
dc
dc[, lg := log(g)]

davg <- dc[, .(avgg = mean(lg)), keyby = id]
(between <- davg[, var(avgg)])

# individual variance within each group
dvar <- dc[, .(varg = var(lg)), keyby = id]
(within <- dvar[, mean(varg)])
between/(between+within)

glmerfit <- glmer(g ~ 1 + (1|id), 
                  family = Gamma(link="log"),nAGQ = 7, data= dc)

summary(glmerfit)
estnu <- as.data.table(VarCorr(m1))[2,4]
estsig <- as.data.table(VarCorr(m1))[1,4] / estnu 

estsig/(estsig + trigamma(1/estnu))

m2<-lmer(lg ~ 1 + (1|id), 
         data= dc)

summary(m2)
#Frequentist power 78% for difference of 20,000

#Frequentist power 83% for difference of 22,000


davg <- dc[, .(avgg = mean(lg)), keyby = id]
(between <- davg[, var(avgg)])

# individual variance within each group
dvar <- dc[, .(varg = var(lg)), keyby = id]
(within <- dvar[, mean(varg)])

between.var/(between.var + within.var)

m1<-glmer(cost~period+trt+(1|cluster),family=gaussian(link="log"),data=dd)
summary(m1)
