# SAMPLE SIZE three PROPORTIONS
library(Rlab)

rm(list=ls())
n<-3000
n1<-n*.88
n2<-n*.1
n3<-n*.02

p1<-.08
p2<-.01
p3<-.2

nsim<-2000
pvals<-matrix(NA, nrow=nsim,ncol=2)

# pval<-sig2<-sig<-rep(NA,nsim)
#sig3<-sig2<-rep(NA,nsim)

for(i in 1:nsim){
  y1 <- rbern(n1, p1)
  y2 <- rbern(n2, p2)

  y3 <- rbern(n3, p3)
  d1<-data.frame(y=c(y1,y2,y3),
                 treat=rep(c("A","B","C"),times=c(n1,n2,n3)))
  f1<-glm(y~treat,data = d1,family=binomial)
  pvals[i,]<-summary(f1)$coef[2:3,4]
  # pval[i]<-anova(f1,test = "Chi")$"Pr"[2]


}
mean(pvals[,1]<.05)
mean(pvals[,2]<.05)

results2<-as.data.frame(round(results,2))

names(results2)<-c("90%","80%")
results2$effect<-t.effect
results2

