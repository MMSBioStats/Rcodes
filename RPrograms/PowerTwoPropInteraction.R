n1<-n2<-750

person<-1:(n1+n2)
b0<-log(.55)
b3<-log(.4/.55)


nsim<-1000
sig<-rep(NA,nsim)

for(i in 1:nsim){
hiat<-sample(0:1,n1+n2,replace=T)

treatids1<-sample(person[hiat==1],sum(hiat)/2)
treatids2<-sample(person[hiat==0],(n1+n2-sum(hiat))/2)

treatment<-rep(0,n1+n2)
treatment<-ifelse(person%in%c(treatids1,treatids2),1,0)

p1<-exp(b0+b3*treatment*hiat)

y<-rbinom(n1+n2,1,p1)

datafake<-data.frame(person,treatment,hiat,y)

m1<-glm(y~treatment*hiat,data=datafake)
sig[i]<-summary(m1)$coef[4,3]<(-2)
}

mean(sig)