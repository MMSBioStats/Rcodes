n1<-100
x1<-c(rep(1,n1),rep(0,n1))
x2<-rep(c(1,0),n1)
b1<-1
b2<-1
b3<-1
b4<-.5

nsim<-1000
pval<-rep(NA,nsim)
for(i in 1:nsim){
y<-rnorm(n1*2,b1+b2*x1+b3*x2+b4*x1*x2, sd=.5)

fit<-lm(y~x1*x2)
pval[i]<-summary(fit)$coef[4,4]<.05
}

mean(pval)

#############
nsim<-1000
pval<-rep(NA,nsim)
mu<-exp(b1+b2*x1+b3*x2+b4*x1*x2)
for(i in 1:nsim){
  y<-rnorm(n1*2,b1+b2*x1+b3*x2+b4*x1*x2, sd=.5)
  
  fit<-lm(y~x1*x2)
  pval[i]<-summary(fit)$coef[4,4]<.05
}

mean(pval)