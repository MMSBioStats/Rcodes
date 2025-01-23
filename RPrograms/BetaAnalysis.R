n1<-101
n2<-25

case1<-101*.495
case2<-25*.36

a1<-b1<-5
a1<-b1<-.5
#b1<-7.5

#a1<-10
#b1<-15
prior1<-rbeta(100000,a1,b1)
prior2<-rbeta(100000,a1,b1)

ORprior<-(prior1/(1-prior1))/(prior2/(1-prior2))
quantile(ORprior,c(.025,.975))
mean(ORprior)

p1post<-rbeta(100000,a1+case1,b1+n1-case1)
p2post<-rbeta(100000,a1+case2,b1+n2-case2)

ORpost<-(p1post/(1-p1post))/(p2post/(1-p2post))
quantile(ORpost,c(.025,.975))

mean(ORpost>1)
median(ORpost)
quantile(ORpost,c(.025,.975))

#pdf(file="/Users/cpedroza/Documents/CLEARplot.pdf")
plot(density(log(ORprior)),xlim=c(-2,2),ylim=c(0,1),xlab="Log Odds Ratio",
     ylab="Probability", main="",lty=3,lwd=2,bty="l")
  
lines(density(log(ORpost)),lwd=2)
#a1<-b1<-.5
#p1post<-rbeta(100000,a1+case1,b1+n1-case1)
#p2post<-rbeta(100000,a1+case2,b1+n2-case2)

#ORpost<-(p1post/(1-p1post))/(p2post/(1-p2post))
#lines(density(log(ORpost)),lty=2,lwd=2)

legend(-1.5,.3,"Prior",bty="n")
legend(.36,1,"Posterior",bty="n")
#legend(1.1,.3,"Current Data",bty="n")
abline(v=0)
#dev.off()

RRpost<-p1post/p2post

x<-seq(0,1,length=1000)

y<-dbeta(x,a1,b1)/dbeta(x,a1,b1)



