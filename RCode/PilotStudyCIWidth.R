#COMPUTING 95% CI WIDTH FOR PILOT TRIAL
## PI: SHAINE MORRIS
## APRIL 16, 2015

se.diff<-se.beta<-rep(NA,500)
nsamp<-6
for(j in 1:500){
nu1<-rnorm(nsamp,0,sd=.45)

y<-matrix(NA,nrow=nsamp,ncol=4)
for(i in 1:nsamp)
y[i,]<-3.6+c(3,6,9,12)*.16+nu1[i]+rnorm(1,0,sd=.6)

#sd(y)
ytreat<-c(y)

nu2<-rnorm(nsamp,0,sd=.45)

y2<-matrix(NA,nrow=nsamp,ncol=4)
for(i in 1:nsamp)
  y2[i,]<-3.6+c(3,6,9,12)*.12+nu2[i]+rnorm(1,0,sd=.6)
yc<-c(y2)
#sd(y)

data1<-data.frame(y=c(ytreat,yc),group=c(rep(1,nsamp*4),rep(-1,nsamp*4)),
                  time=rep(c(1:4),length=nsamp*4),id=rep(1:(nsamp*2),each=4))
fit1<-lmer(y~group*time+(1|id),data=data1)
se.beta[j]<-sqrt(diag(summary(fit1)$vcov)[4]*2)
se.diff[j]<-summary(fit1)$coef[4,2]
}

mean(se.beta)
mean(se.diff)
