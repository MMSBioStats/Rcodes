###
## BAYESIAN POWER ANALYSIS FOR TWO ARM RCT W/BINARY OUTCOME
## WE ASSUME A CONTROL RATE OF 10% FOR SAE'S, N=150/GROUP
## PRIMARY POWER IS FOR TREATMENT EFFECT OF RR=0.6 (SEE CI'S BELOW)
## ALSO CALCULATED POWER FOR RR=0.5
## AND FOR N=100/GROUP

## FOR SCENARIO 1, POWER=81% UNDER NEUTRAL PRIOR
## FOR SCENARIO 2, POWER=91% UNDER NEUTRAL PRIOR
## FOR SCENARIO 3, POWER=% UNDER NEUTRAL PRIOR

#---------------------------------------------------------
library(arm)
library(MASS)
library(BRugs)       
library(R2OpenBUGS) 

rm(list=ls())

setwd("C:/Users/cpedroza/Google Drive/RCode/")

nsim<-500

#SAMPLE SIZE PER GROUP
ngroup<-300

ESclinimport<-log(.9)  ##clinically important treatment effect
confsig<-0.95
confimport<-0.5

logRR<-log(.75)
sdlogRR<-(log(1.05)-log(.75))/1.96
#exp(qnorm(c(.025,.975),logRR,sd=sdlogRR))

pcontrol<-.35
sdcontrol=(log(.55)-log(.35))/1.96

curve(dnorm(x,logRR,sdlogRR),xlim=c(-.8,.5))

curve(dnorm(x,pcontrol,sdcontrol),xlim=c(0,1))
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Model 1
## OPTIMISTIC PRIOR
modelstring = "
# BUGS model specification begins here...
model {
for( i in 1 : nData ) {
y[i] ~ dbern (p.bound[i])
p.bound[i] <- max(0, min(1, mu[i]))
log(mu[i]) <-   b0   + b1*x[i]
}
b0 ~ dnorm( -1.049822, 18.8 )

b1 ~ dnorm( -0.2876821 , 34)   #logRR

}
# ... end BUGS model specification
" # close quote for modelstring
# Write model to a file:
writeLines(modelstring,con="modelOptim.txt")
modelCheck( "modelOptim.txt" )  #checking model 

#------------------------------------------------------------------------------
# Model 2
# SKEPTICAL PRIOR

# modelstring = "
# # BUGS model specification begins here...
# model {
# for( i in 1 : nData ) {
# y[i] ~ dbern (p.bound[i])
# p.bound[i] <- max(0, min(1, mu[i]))
# log(mu[i]) <-   b0   + b1*x[i]
# }
# b0 ~ dnorm( -2.302585 , 400 )
# b1 ~ dnorm( 0 , 3.18)   
# 
# }
# # ... end BUGS model specification
# " # close quote for modelstring
# # Write model to a file:
# writeLines(modelstring,con="modelSkep.txt")
# modelCheck( "modelSkep.txt" )  #checking model 

treatment<-rep(c(1,0),each=ngroup)

niter<-5000

sig.prob<-matrix(NA,ncol=2,nrow=nsim)

for(i in 1:nsim){
  b0<-rnorm(1,log(pcontrol),sd=sdcontrol)
  b1<-rnorm(1,logRR,sd=sdlogRR)
  
  p.ij<-exp(b0+b1*treatment)
  
  y<-rbinom(ngroup*2,1,p.ij)
  
  datalist = list(
    x = treatment ,
    y = y, 
    nData = ngroup*2 
  )
  
  m1<-glm(y~x,family=binomial,data=datalist)
  inits1 <- list(list(b0=coef(m1)[1],b1=coef(m1)[2]))
  
  #run the chains
  M1<-bugs(data=datalist,inits1,model.file = "modelOptim.txt",para = c("b0", "b1"),n.chains=1,n.burnin=1000,debug=F ,n.iter=niter )
 # M2<-bugs(data=datalist,inits1,model.file = "modelSkep.txt",para = c("b0", "b1"),n.chains=1,n.burnin=1000,debug=F ,n.iter=niter )
  
# calculate posterior probabilities of benefit under both priors
sig.prob[i,1]<-mean(M1$sims.matrix[,2]<0)
  sig.prob[i,2]<-mean(M1$sims.matrix[,2]< ESclinimport)
}

## CALCULATING POWER UNDER EACH PRIOR
mean(sig.prob[,1]>=confsig )
mean(sig.prob[,2]>=confimport )


