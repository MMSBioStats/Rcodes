# Calculate sample size/power for diagnostic tests based on sensitivity/specificity
# Must specify hypothesized sen/sp, minimum acceptable value for sens/spec
# prevalence
# power, alpha
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("limma")

library(MKmisc)
# Example
# we hypothesize sensitivity of 90%, minimum of 80% (delta=0.1), prevalence=10%

prev.rate<-c(.5,.6,.7,.8,.9)
prev.rate<-c(.5,.4,.3,.2,.1)

res<-matrix(NA,nrow=length(prev.rate),ncol=5)

for(i in 1:length(prev.rate)){
sp<-power.diagnostic.test(spec = .9, sig.level = 0.05, 
                      power=.8,delta=.1,
                      prev=prev.rate[i])

sen<-power.diagnostic.test(sens = 0.6, sig.level = 0.05, 
                      delta = 0.18, power=.8,
                      prev=prev.rate[i])
res[i,]<-c(prev.rate[i],sp$n1,sp$n,sen$n,sen$n1)
}
res
res1<-data.frame(res)
names(res1)<-c("prev","ncontrols_spec","ncases_spec","ncontrols_sen","ncases_sen")
res1

1401*(.47+.15)+105*(.295+.20)+32*(.188+.094)+43*(.093*2)+47*(.043+.021)+125*(.136+.056)
401+105+32+43+47+125

# Diagnostic test exact power calculation 
# 
# sens = 0.9
# n = 94
# n1 = 846
# delta = 0.1
# sig.level = 0.05
# power = 0.8
# prev = 0.1
# 
# NOTE: n is number of cases, n1 is number of controls

# if we know disease status beforehand, then we don't need to specify prevalence
power.diagnostic.test(sens = 0.7, sig.level = 0.05, delta = 0.14,power = 0.8,method = "asymptotic")

power.diagnostic.test(sens = 0.7, sig.level = 0.05, delta = 0.12,n=100,method = "asymptotic")

power.diagnostic.test(spec  = 0.9, sig.level = 0.05, delta=.1,power=.8,method = "asymptotic")

power.diagnostic.test(sens = 0.95, sig.level = 0.05, delta = 0.05,power=.8,method = "asymptotic")
power.diagnostic.test(spec  = 0.98, sig.level = 0.05, delta=.08,power=.8)
power.diagnostic.test(sens  = 0.985, sig.level = 0.05, delta=.085,prev=.42,n=50)

# Diagnostic test exact power calculation 
# 
# sens = 0.9
# n = 94
# n1 = 94
# delta = 0.1
# sig.level = 0.05
# power = 0.8
# prev = NULL
# 
# NOTE: n is number of cases, n1 is number of controls
sen.rate<-c(.45,.55,.6,.65)
del<-c(.38,.42,.44)
res<-list()
k<-1
# Different values for sensitivity and delta
for(i in 1:length(sen.rate)){
  for(j in 1:length(del)){
  
    sen<-power.diagnostic.test(sens = sen.rate[i], sig.level = 0.05, 
                             delta = sen.rate[i]-del[j], n=100)
  res[[k]]<-c(sen.rate[i],del[j],sen$power)
  k<-k+1
  }
}
res

# POWER AUC
library(pROC)
power.roc.test(ncases=100, ncontrols=100, sig.level=0.05, power=0.95)

