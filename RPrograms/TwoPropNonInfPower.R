n1<-322
nsim<-1000
sig<-rep(NA,nsim)

for(i in 1:nsim){
y1<-rbinom(1,n1,.15)
y2<-rbinom(1,n1,.15)

sig[i]<-prop.test(c(y1,y2),rep(n1,2),conf.level = .9,correct = F)$conf.int[2]<.07

}
mean(sig)