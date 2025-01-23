
rm(list=ls())

y1<-c(25.38,34.92,27.83,32.49)
y2<-c(28.87,
      36.35,
      28.49,
      33.74)
df1<-data.frame(y=c(y1,y2),time=rep(c(0,1),each=4),id=rep(c(1:4),length=8))

m1<-lmer(y~time+(1|id),data=df1)

cbf_change<-(y2-y1)/y1

# POWER ANALYSIS FOR TWO TIME POINTS COMPARING DRUG VS PLACEBO -------
#total number of subjects
nsub=50

time<-rep(0:1,nsub)
person<-rep(1:nsub,each=2)
#sub<-rep(person,2)

group<-sample(rep(0:1,nsub/2))
group1<-rep(group,each=2)

mu.a.drug<-mean(y1)
#mu.a.placebo<-2.7
sigma.a.true<-sqrt(12)
#sigma.a.true<-as.data.frame(VarCorr(m1))$sdcor[1]

#assuming a 15% change from baseline in drug group and 5% in placebo group
b.true<-mean(y1)*.15*group1+1.5*(1-group1)
sigma.y<-sqrt(4)
#sigma.y<-as.data.frame(VarCorr(m1))$sdcor[2]

nsim<-1000
sig<-rep(NA,nsim)
for(i in 1:nsim){
 if(i>1) rm(a.true.drug,y,datafake,fit1)
a.true.drug<-rnorm(nsub,mu.a.drug,sigma.a.true)
#a.true.placebo<-rnorm(nsub,mu.a.placebo,sigma.a.true)
y<-rnorm(nsub*2,a.true.drug[person]+b.true*(time),sigma.y)

datafake<-data.frame(y,time,person,group1)

fit1<-lmer(y~group1*as.factor(time)+(1|person),data=datafake)
sig[i]<-(fixef(fit1)[4]-2*se.fixef(fit1)[4])>0
}

mean(sig)

xyplot(jitter(y)~time|group1,groups=person,data=datafake,type="b")

time<-c(0,24,48)
b1<-c(0,15,15)
b2<-c(0,0,15)

plot(time,b1,type="l",lwd=3,xlab="Week",
     ylab="% Change Cerebral Blood Flow",col="blue",ylim=c(0,20),axes=F)
lines(c(0,24),c(0,0),col="green",lwd=3,lty=2)
lines(c(24,48),c(0,15),col="blue",lwd=3)
axis(1,at=c(0,12,24,36,48))
axis(2)
text(14,15,"Early-start (ACZ-ACZ)",cex=.9)
text(40,3,"Delayed-start (placebo-ACZ)",cex=.9)
box()

