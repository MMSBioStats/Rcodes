# Bayesian power calculations comparing means between >=2 groups

nsim<-3000
ngroup<-10

mu1<-150
mu2<-mu1
mu3<-mu1+10
sd2<-sd3<-sd1<-15

# d<-(.5/qnorm(.025))^2 ##smallest treatment effect considered medically important is
# ##half-standard deviation and there's .025 prob. of it
# dinv<-15  #skeptical prior for interaction terms
# Dinv<-diag(c(0,0,dinv,dinv))

# prior variance 
Dinv<-diag(rep(1/1000^2,3))

group<-factor(c(0,1,2))

sig3<-sig<-sig2<-rep(NA,nsim)
for(i in 1:nsim){
y1<-rnorm(ngroup,mean=mu1,sd=sd1)
y2<-rnorm(ngroup,mean=mu2,sd=sd2)
y3<-rnorm(ngroup,mean=mu3,sd=sd3)

d1<-data.frame(y=c(y1,y2,y3),group=rep(group,each=ngroup))

m1<-lm(y~group,data=d1)
summary(m1)

beta<-coef(m1)
Sig<-vcov(m1)

Sig_inv<-solve(Sig)

Binv<-Sig_inv+Dinv
B<-solve(Binv)

Bb<-B%*%Sig_inv%*%beta

sig3[i]<-anova(m1)$`Pr(>F)`[1]
sig[i]<-pnorm(0,mean=Bb[2],sd=sqrt(B[2,2]),lower.tail = T)
sig2[i]<-pnorm(0,mean=Bb[3],sd=sqrt(B[3,3]),lower.tail = F)
}

mean(sig>.9)
mean(sig2>.9)
mean(sig2>.8)
mean((sig3)<.05)
 
mean(sig>.9 | sig2>.9)
 mean(sig>.8 | sig2>.8)
 
