# POWER & SAMPLE SIZE CALCULATIONS FOR BIOMARKERS
# ASSUMES TWO GROUPS
library(ssize.fdr)

d<-2.625 #difference in differentially expressed genes to be detected
s<-2.5 ##standard deviation
a<-0.05 ##false discovery rate to be controlled
pwr<-0.8 ##desired power
p0<-c(0.9,0.93,0.95) ##proportions of non-differentially expressed genes
N<-30##maximum sample size for calculations
ts<-ssize.twoSamp(delta=d,sigma=s,fdr=a,power=pwr,
                  pi0=p0,maxN=N,side="two-sided")
ts$ssize ##first sample sizes to reach desired power for each proportion of
##non-differentially expressed genes
ts$power ##calculated power for each sample size
ts$crit.vals ##calculated critical value for each sample size

#varying mean differences and SD across genes
tsv<-ssize.twoSampVary(deltaMean=d,deltaSE=s,
                       a=3,b=1,fdr=a,power=pwr,pi0=p0,
                       maxN=N,side="two-sided")
tsv$ssize	##first sample size(s) to reach desired power
tsv$power ##calculated power for each sample size

#-------------------
library(pROC)

nsub<-150
nsim<-2000
brier<-lci<-beta<-auc<-rep(NA,nsim)
for(i in 1:nsim){
y1<-rnorm(nsub,5.6,sd=2.6)
y2<-rnorm(nsub,0,sd=2.6)
d1<-data.frame(y=c(y1,y2),group=rep(c(1,0),each=nsub))

# t.test(y~group,data=d1)

m1<-glm(group~y,data=d1,family="binomial")
beta[i]<-m1$coefficients["y"]
d1$fit1<-predict(m1,  type="response")

brier[i]<-mean((d1$fit1-d1$group)^2)
r1<-roc(d1$group, d1$fit1, direction="<",auc=T,ci=T,quiet=T)
auc[i]<-r1$auc
lci[i]<-r1$ci[1]
}

mean(auc>.91)
mean(beta>log(1.9))
mean(lci>.9)
mean(brier<.11)

d2<-data.frame(sen=r1$sensitivities,spec=r1$specificities,youden=(r1$sensitivities+r1$specificities-1))

d2[d2$youden==max(d2$youden),]
