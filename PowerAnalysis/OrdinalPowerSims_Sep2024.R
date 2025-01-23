#############
# POWER ANALYSES FOR SEGA TRIAL PROPOSAL
# PI:  R CHEN
# JULY 17, 2017

# COMPARES GENERAL ANESTHESIA (GA) VS CONSCIOUS SEDATION (CS)
# HYPOTHESIS: GA WILL HAVE LARGER % GOOD OUTCOME
# FOR ORDINAL OUTCOME, WE USE CONTROL (CS) FREQUENCIES FROM 3 TRIALS
# AND WE ASSUME OR=1.5, ESTIMATED FROM THE TRIALS
# WE ASSUME N=250
# HERE, WE LOOK AT SAMPLE SIZE ACCOUNTING FOR LOSS TO FOLLOWUP
# AND FOR POTENTIAL CROSS-OVER OF CS ARM PATIENTS TO GA
# HERE, WE MODIFIED THE WEIGHTING FOR ANSTROKE TRIAL-->50% OF ORIGINAL WEIGHTING
###############

# CODE MODIFIED FROM
# http://www.statsblogs.com/2015/05/22/simulation-based-power-analysis-using-proportional-odds-logistic-regression/
## load MASS for polr()
library(MASS)
library(dplyr)
library(flextable)
# library(rms)

rm(list=ls())

set.seed(508329)

# prbs<-c(.12,.035,.375,.47)
# 
# p1<-c(.125,.04,.36,.475)
# 
# 
# resp <- factor(replicate(1000, sample(1:4, 1, prob=prbs)),
#                ordered=TRUE, levels=1:4)
# ## fit POCL model; extract intercepts (zeta here)
# alph <- polr(resp~1)$zeta

OR<-function(p1,p2) (p1/(1-p1))/(p2/(1-p2))
## convenience functions
logit <- function(p) log(1/(1/p-1))
expit <- function(x) 1/(1/exp(x) + 1)

## block randomization
## n - number of randomizations
## m - block size
## levs - levels of treatment
block_rand <- function(n, m, levs=LETTERS[1:m]) {
  if(m %% length(levs) != 0)
    stop("length(levs) must be a factor of 'm'")
  k <- if(n%%m > 0) n%/%m + 1 else n%/%m
  l <- m %/% length(levs)
  factor(c(replicate(k, sample(rep(levs,l),
                               length(levs)*l, replace=FALSE))),levels=levs)
}

## simulate from POCL model
## n - sample size
## a - alpha
## b - beta
## levs - levels of outcome
pocl_simulate <- function(n, a, b, levs=0:length(a)) {
  dat <- data.frame(Treatment=block_rand(n,2,LETTERS[1:2])) 
  des <- model.matrix(~ 0 + Treatment, data=dat)
  nlev <- length(a) + 1
  yalp <- c(-Inf, a, Inf)
  xbet <- matrix(c(rep(0, nrow(des)),
                   rep(des %*% b , nlev-1),
                   rep(0, nrow(des))), nrow(des), nlev+1)
  prbs <- sapply(1:nlev, function(lev) {
    yunc <- rep(lev, nrow(des))
    expit(yalp[yunc+1] - xbet[cbind(1:nrow(des),yunc+1)]) - 
      expit(yalp[yunc]   - xbet[cbind(1:nrow(des),yunc)])
  })
  colnames(prbs) <- levs
  dat$y <- apply(prbs, 1, function(p) sample(levs, 1, prob=p))
  dat$y <- unname(factor(dat$y, levels=levs, ordered=TRUE))
  return(dat)
}



## Likelihood ratio test with 0.05 p-value threshold
## block randomization in blocks of size four to one
## of two treatment groups
## dat - data from pocl_simulate
pocl_test <- function(dat,alpha) {
  fit <- polr(y~Treatment, data=dat)
  anova(fit, update(fit, ~.-Treatment))$"Pr(Chi)"[2] < alpha
}

## power: n=50, OR=0.25
# mean(replicate(500, pocl_test(pocl_simulate(120, a=alph, b=c(0, log(2))))))

# d1<-pocl_simulate(40000, a=alph, b=c(0, log(2.3)))
# table(d1)[1,]/20000
# table(d1)[2,]/20000
# #table(d1)[3,]/20000
# 
# alph<-c(logit(.4),logit(.55))
# alph<-c(logit(.45),logit(.6))

# 10%, 14%, 11%, 19%, 18%, and 28% for mRS outcome categories (0, 1, 2, 3, 4, and 5+6)
probs<-c(.1,.14,.11,.19,.18,.28)
alph<-logit(cumsum(probs))[-length(probs)]

mean(replicate(1000, pocl_test(
  pocl_simulate(250, a=alph, b=c(0, log(1.53))),alpha=0.05)))

## Computing approximation of Bayesian posterior probability of OR>1 (GA vs CS)
# NOTE THAT ESTIMATED COEF IS LOG ODDS OF HIGHER (BAD) OUTCOMES, THEREFORE WE CALCULATE
# PROBABILITY OF BETA<0 (REVERSED)
## dat - data from pocl_simulate
pocl_Bayes_test <- function(dat,priormu,priorvar) {
  nlev<-length(levels(dat$y))
  fit <- polr(y~Treatment, data=dat,Hess = T)
  coefs<-summary(fit)$coef
  # fit <- lrm(y~Treatment, data=dat)
  # 
  # obsmu<-coefs["TreatmentB",1]
  # obsvar<-coefs["TreatmentB",2]^2
  
  # priormu1<-0
  # priorvar1<-0.7^2
  beta<-summary(fit)$coef[,1]
  Sig<-vcov(fit)
  # pval<- anova(fit, update(fit, ~.-Treatment))$"Pr(Chi)"[2]
  
  Sig_inv<-solve(Sig)
  
  probs<-matrix(NA,nrow=1,ncol=length(priormu))
  
  for(i in 1:length(priormu)){
  Dinv<-diag(c(1/priorvar[i],rep(1/10,nlev-1)))
  # CHANGE TO MATRIX MULTIPLICATION HERE 
  
  Binv<-Sig_inv+Dinv
  B<-solve(Binv)
  
  Bb<-B%*%(Sig_inv%*%beta+Dinv%*%c(priormu[i],rep(0,nlev-1)))
  
  
  probs[1,i]<-pnorm(0,mean=Bb[1],sd=sqrt(B[1,1]),lower.tail = F)
 # probs[2,i]<-pnorm(0,mean=Bb[2],sd=sqrt(B[2,2]),lower.tail = F)
  }
  # postvar1<-1/(1/priorvar1+1/obsvar)
  # postmu1<-(priormu1/priorvar1+obsmu/obsvar)*postvar1
  # 
  # postvar2<-1/(1/priorvar2+1/obsvar)
  # postmu2<-(priormu2/priorvar2+obsmu/obsvar)*postvar2
  # 
  # p1<-pnorm(0,mean=postmu1,sd=sqrt(postvar1),lower.tail = F)
  # p2<-pnorm(0,mean=postmu2,sd=sqrt(postvar2 ),lower.tail = F)
  # return(c(p1,p2))
  return(probs)
}

# BAYESIAN POWER -----------------------------

## sample 1000 observations with probabilities prbs
# rates in control group
mean_vec_gt<-function(a,probs){
  res<-NULL
  for(i in 1:length(probs)) res<-append(res,mean(a>probs[i],na.rm=T))
  return(res)
}

# p1<-c(.125,.04,.36,.475)
# 
# prbs<-c(.12,.035,.375,.47)
# prbs<-p1
# 
# resp <- factor(replicate(100000, sample(1:6, 1, prob=probs)),
#                ordered=TRUE, levels=1:6)
# ## fit POCL model; extract intercepts (zeta here)
# alph <- polr(resp~1)$zeta
# 
# probs<-c()
# logit(cumsum(probs))

# pocl_Bayes_test(d1,
#   priormu = c(0, log(1.2)),
#   priorvar = c(.7 ^ 2, .35 ^ 2))
  
# d1<-pocl_simulate(40000, a=alph, b=c(0, log(1.87)))
# table(d1)[1,]/20000
# table(d1)[2,]/20000

# list to hold results
res<-list()

#Effect size
OR<-1.8
logOR<-log(OR)  #log(1.78) for 0.7, log(1.56) for 0.75, log(1.5) for 0.80

# d1<-pocl_simulate(100, a=alph, b=c(0, logOR))
# dim(d1)
nsamp<-c(100,200,250)

for(i in 1:length(nsamp)){
pow1 <-
  replicate(1000,
            pocl_Bayes_test(
              pocl_simulate(nsamp[i], a = alph, b = c(0, logOR)),
              priormu = 0,
              priorvar = .7))
# dim(pow1)
mean(pow1>.95)

mean(pow1>.9)
mean(pow1>.85)

mean(pow1>.8)
mean(pow1>.75)

#Neutral Prior
# mean(pow1[1,1,]>.8)
# mean(pow1[1,1,]>.9)

res[[paste(nsamp[i],OR)]]<-c(nsamp[i],OR,(mean_vec_gt(pow1,seq(.75,.95,by=.05))))
}

res.all<-do.call(rbind,res)

res.all<-round(res.all,2)
res.data<-data.frame(res.all)

names(res.data)<-c("Total sample size","True OR",paste("Pr(OR>1)>",seq(.75,.95,by=.05)))

t.res<-res.data %>% flextable()%>%set_caption(caption = "Bayesian power for ordinal outcome under a neutral prior.")%>%padding(padding.top = 2,padding.bottom = 2)
t.res

save_as_docx(t.res,path=paste0("C:/Users/cpedroza/OneDrive - UTHealth Houston/Advising/Sharivari/PowerOrdinalmRS_",Sys.Date(),".docx"))

#Data prior
# c(mean_vec_gt(pow1[1,2,],c(.8,.9)))
mean(pow1[1,2,]>.8 )
mean(pow1[1,2,]>.9)


# Ordinal with 5 categories ---------
p1<-c(.125,.04,.12,.24,.475)

prbs<-c(.12,.035,.125,.25,.47)
prbs<-p1
resp <- factor(replicate(100000, sample(1:5, 1, prob=prbs)),
               ordered=TRUE, levels=1:5)
## fit POCL model; extract intercepts (zeta here)
alph <- polr(resp~1)$zeta

d1<-pocl_simulate(180, a=alph, b=c(0, log(1.5),log(1.5)))
table(d1)[1,]/20000
table(d1)[2,]/20000
table(d1)[3,]/20000
# pocl_Bayes_test(d1,
#   priormu = c(0, log(1.2)),
#   priorvar = c(.7 ^ 2, .35 ^ 2))

#Effect size
logOR<-log(1.78) #log(1.78) for 0.7, log(1.56) for 0.75, log(1.5) for 0.80


pow1 <-
  replicate(3000,
            pocl_Bayes_test(
              pocl_simulate(180, a = alph, b = c(0, logOR, logOR)),
              priormu = c(0, log(1.2)),
              priorvar = c(.7 ^ 2, .35 ^ 2)
            ))

#Neutral Prior
mean(pow1[1,1,]>.8 | pow1[2,1,]>.8)
mean(pow1[1,1,]>.9 | pow1[2,1,]>.9)

# c(mean_vec_gt(pow1[1,1,],c(.8,.9)))

#Data prior
# c(mean_vec_gt(pow1[1,2,],c(.8,.9)))
mean(pow1[1,2,]>.8 | pow1[2,2,]>.8)
mean(pow1[1,2,]>.9 | pow1[2,2,]>.9)
# 
# # Calculate the win ratio
# d1<-pocl_simulate(180, a=alph, b=c(0, log(1.5),log(1.5)))
# 
# d1$id<-c(1:nrow(d1))
# d1$fu<-10
# d1$out<-as.numeric(d1$y)
# 
# d1.1<-d1[d1$Treatment!="B",]
# fit <- polr(y~Treatment, data=d1.1,Hess=T)
# anova(fit, update(fit, ~.-Treatment))
# 
# wr <- winratio(id = "id", trt = "Treatment", active = "C", 
#                outcomes = list(outc3 = c("out", "c", "<")), fu="fu",
#                data = d1.1)
# summary(wr)
# 

