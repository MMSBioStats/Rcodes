# BAYESIAN SAMPLE SIZE APPROX FOR TWO PROPORTIONS
# USING NEUTRAL INFORMATIVE PRIOR FOR RR, 0.5-2

rm(list=ls())
ntreat<-ncontrol<-75

#sample size range of 300-1000

t.effect<-c(0.6,.8,.85)
pcontrol<-0.18
ptreat<-pcontrol*t.effect
#ptreat<-.16

#-----------------------------
###NEUTRAL PRIOR w/ 95% CrI: 0.5-2.0
#-----------------------------

#priormu1<-log(.67)
priormu1<-0
priorsd1<-log(2)/(1.96)
priorsd1<-.7
#priorsd1<-10000

results<-matrix(NA, nrow=length(t.effect),ncol=4)
nsim<-10000


sig<-rep(NA,nsim)
#sig3<-sig2<-rep(NA,nsim)

for(k in 1:length(t.effect)){
for(i in 1:nsim){
  ytreat <- rbinom(1, ntreat, ptreat[k])
  ycontrol <- rbinom(1, ncontrol, pcontrol)
  
  logRR<-log((ytreat/ntreat)/(ycontrol/ncontrol))
  
  ##OBSERVED RATES
  obsmu<-logRR
  obssd<-sqrt(1/ytreat-1/ntreat+1/ycontrol-1/ncontrol)
  

postvar1<-1/(1/priorsd1^2+1/obssd^2)
postmu1<-(priormu1/(priorsd1^2)+obsmu/(obssd^2))*postvar1


sig[i]<-pnorm(0,mean=postmu1,sd=sqrt(postvar1),lower=T)
}

results[k,]<-c(mean(sig>.7,na.rm=T),mean(sig>.75,na.rm=T),
               mean(sig>.8,na.rm = T),
mean(sig>.85,na.rm=T))

}

results2<-as.data.frame(round(results,2))

names(results2)<-c("70%","75%","80%","85%")
results2$effect<-t.effect
results2
#results2$"Treatment Effect"<-t.effect
write.csv(results2[,c(5,1:4)],file=paste("G:/My Drive/Holmes/BayesPowerN",2*ntreat,
                                         ".csv",sep=""),row.names=F)

#---------------------------------------------


write.csv(results2[,c(5,1:4)],file=paste("G:/My Drive/ObGyn/Amro/BayesPowerN",2*ntreat,
                             ".csv",sep=""),row.names=F)
 logit <- function(p) { log(p/(1-p)) } 
 expit <- function(x) { exp(x)/(1+exp(x)) }   
 p0 = 0.4 
 p1 = 0.6 
 suc = logit(23.5/46) - logit(p0) 
 fut = logit(7.5/16) - logit(p0) 
 design <- gsbDesign(nr.stages=2,                     
                     patients=cbind( c(0,0), c(16,30) ),    
                     sigma=2,                    
                     criteria.success=rbind( c(NA,NA), c(suc,0.5) ), 
                     criteria.futility=rbind( c(fut,0.5), c(NA,NA) ),
                     prior.control=c( logit(p0),1000),       
                     prior.treatment=c( logit(p0),0.001))  
 
 simulation <- gsbSimulation(truth=list(logit(p0),seq(0,0.8,0.01)), 
                             grid.type = "sliced",            
                             type.update = "per arm",         
                             nr.sim = 100000,                 
                             warnings.sensitivity = 2000,     
                             seed = 1)      
 
 result <- gsb(design,simulation)     
 result 
 tab(result, what="cumulative all") 
 tab(result, what="sample size") 
 plot(result, what="sample size", sliced=TRUE) 
 plot(result, what="cumulative all", sliced=TRUE)
 
 
 #-------------
 p0 = 0.665 
 p1 = 0.721
 design <- gsbDesign(nr.stages=3,                     
                     patients=20,    
                     sigma=c(2.2,2.1),                    
                     criteria.success=c(0,0.95) , 
                     criteria.futility=c(0,.9))

 simulation <- gsbSimulation(truth=c(log(1),log(1.3)),
                             grid.type = "manually",            
                             type.update = "treatment effect",         
                             nr.sim = 100000,                 
                             warnings.sensitivity = 2000,     
                             seed = 1)      
 result <- gsb(design,simulation)     
 tab(result, what="cumulative all") 
 tab(result,what="cumulative success")
 tab(result,what="cumulative futility")
 
 tab(result, what="sample size") 
 
  sim2<-gsbSimulation(truth = list(c(logit(p0)), 
                                   c(logit(.582),logit(p0), logit(p1))),
               type.update = "per arm", method = "simulation", 
               grid.type = "plot",
               nr.sim = 10000, warnings.sensitivity = 500, 
               seed = 1)
 
 result2 <- gsb(design,sim2)     
 result2
 tab(result2, what="cumulative all") 
 tab(result2,what="cumulative success")
 tab(result2,what="cumulative futility")
 
 tab(result2, what="sample size") 
 pdf(file="C:/Users/cpedroza/Desktop/OCplot.pdf")
 plot(result2, what="cumulative all")
 dev.off()
 
 
 
 