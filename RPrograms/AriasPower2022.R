
library(Rlab) 
library(arm)
n=1000
mic<-rep(c(1,0,1,0),times=c(n*c(.1,.37,.12,.41)))
cef<-rep(c(0,1),times=n*c(.47,.53))

p1<-invlogit(-2.9704+( -0.8138)*cef+0.6678 *mic+1.62*cef*mic)

nsim<-5000
pval<-rep(NA,nsim)
for(i in 1:nsim){
y<-rbern(length(p1),p1)

d1<-data.frame(y=y,cef=cef,mic=mic)
f1<-glm(y~cef*mic,family=binomial,data=d1)
pval[i]<-summary(f1)$coef[4,4]<.05

}
mean(pval)


with(df1,table(mic,cef))


#Observed data from Lee 2018
# 30 day mortality
# 1/11 and 2/41 in Nafcilin group CIE-P and CIE-N
# 2/13 and 0/45 in Cefazolin group CIE-P and CIE-N
mic<-rep(c(1,0,1,0),times=c(11,41,13,45))
cef<-rep(c(0,1),times=c(52,58))
y<-rep(c(1,0,1,0,1,0,1,0),times=c(1,10,2,39,2,11,1,44))

df1<-data.frame(y=y,cef=cef,mic=mic)

m1<-glm(y~cef*mic,family=binomial,data=df1)
summary(m1)

OR<-function(p1,p2){
  (p1/(1-p1))/(p2/(1-p2))
}