library(MASS)
mu=6892
s2=1400^2

n=mu^2/(s2-mu)
p=n/(n+mu)

mu2=4836
s2.2=1339^2
n2=mu2^2/(s2.2-mu2)

ngroup=10
nsim<-1000
res<-rep(NA,nsim)
for(i in 1:nsim){
y1<-rnbinom(ngroup,size=n,mu=mu)
y2<-rnbinom(ngroup,size=n2,mu=mu2)
d1<-data.frame(y=c(y1,y2),group=rep(c(0,1),each=ngroup))
m1<-glm.nb(y~group,data=d1,link=identity)
res[i]<-summary(m1)$coef[2,4]<.05
}
mean(res)
