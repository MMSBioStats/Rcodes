library(MASS)
# From article, median (IQR)
# 165.0 (77.0-280.0)

mu=117
s2=135^2

n=mu^2/(s2-mu)
p=n/(n+mu)

ysamp<-rnbinom(10000,size=n,mu=mu)
summary(ysamp)

# mu2=mu*.59
mu2=mu+48

# setting variance so that CV is the same as in control group
s2.2= (mu2*sqrt(s2)/mu)^2
n2=mu2^2/(s2.2-mu2)
ysamp<-rnbinom(10000,size=n2,mu=mu2)
summary(ysamp)

# Control group: mu=210, s2=171^2
# Control group, median (IQR):  168.0   (86- 286.0)

# Intervention group: mu2=mu*.5, s2.2=s2*.18
# Intervention group, median (IQR):  88.0   (51.0 -   140.0)

# Intervention group: mu2=mu*.58, s2.2=s2*.18
# Intervention group, median (IQR):  108.0   (69.0 -   160.0)

ngroup=178
nsim<-5000
res2<-res<-rep(NA,nsim)
for(i in 1:nsim){
y1<-rnbinom(ngroup,size=n,mu=mu)
y2<-rnbinom(ngroup,size=n2,mu=mu2)
d1<-data.frame(y=c(y1,y2),group=rep(c(0,1),each=ngroup))
m1<-glm.nb(y~group,data=d1,link=log)
res[i]<-summary(m1)$coef["group",4]<.05
m2<-lm(y~group,data=d1)
res2[i]<-summary(m2)$coef["group",4]<.05
}
mean(res)
mean(res2)
