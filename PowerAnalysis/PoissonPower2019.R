library(MASS)
mu=4.2


mu2=6.2

ngroup=46
y1<-rpois(150,10)
hist(y1)
summary(y1)
nsim<-1000
res<-rep(NA,nsim)

for(i in 1:nsim){
y1<-rpois(ngroup,mu)
y2<-rpois(ngroup,mu2)
d1<-data.frame(y=c(y1,y2),group=rep(c(0,1),each=ngroup))
m1<-glm(y~group,family=poisson,data=d1)
res[i]<-summary(m1)$coef[2,4]<.05
}
mean(res)
