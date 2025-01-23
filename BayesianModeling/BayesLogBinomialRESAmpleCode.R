# SAMPLE CODE Bayesian analysis for binary primary outcome 
# Using log binomial with one random effect 
# June 26, 2018
# 

# BRMS PACKAGE ALLOWS FOR EXPLICIT DEFINITION FOR 
# PRIOR FOR RANDOM EFFECT VARIANCE PARAMETER
# BUT CANNOT USE LOG LINK WITH BINOMIAL

# RSTANARM PACKAGE ALLOWS LOG LINK WITH BINOMIAL
# BUT CANNOT DIRECTLY DECLARE PRIOR FOR RE VARIANCE 
library(geepack)
#library(brms)
library(rstan)
library(rstanarm)

rm(list=ls())

# EXAMPLE 1 DATASET USED IN PEDROZA & TRUONG (2017) TRIALS PAPER
nperson.center<-c(36,37,20,32,19,19,16,17,17,12,11,10,5,9,6,7)
outcome.1<-c(11,10,16,22,14,7,2,1,6,0,1,0,1,1,4,6)
outcome.0<-nperson.center-outcome.1

y.fav<-rep(1,length=sum(outcome.1))
treat1<-rep(rep(c(1,0),8),outcome.1)
y.notfav<-rep(0,length=sum(outcome.0))
treat2<-rep(rep(c(1,0),8),outcome.0)
center1<-rep(1:8,c(21,16+22,14+7,3,6,1,2,10))
center2<-rep(1:8,c(25+27,14,17,14+16,11+12,20,12,3))

y<-c(y.fav,y.notfav)
treat<-c(treat1,treat2)
center<-c(center1,center2)

df1<-as.data.frame(cbind(y,treat,center))

names(df1)<-c("y","treatment","center")
df1<-df1[order(df1$center),]

# FREQUENTIST ROBUST POISSON ------
m1<-geese(y~treatment,id=center,family="poisson",corstr="exch",data=df1)
summary(m1)


# BAYESIAN LOG BINOMIAL WITH RE FOR CENTER -------

s1<-stan_glmer(y~treatment+(1|center),family=binomial(link = "log"),
               data=df1,
        prior =normal(0,1),
        prior_intercept=normal(0,10),
        warmup = 2000, 
        iter = 10000,chains=3)

prior_summary(s1)
# Priors for model 's1' 
# ------
#   Intercept (after predictors centered)
# ~ normal(location = 0, scale = 10)
# 
# Coefficients
# ~ normal(location = 0, scale = 1)
# 
# Covariance
# ~ decov(reg. = 1, conc. = 1, shape = 1, scale = 1)
# ------
#   See help('prior_summary.stanreg') for more details

summary(s1,digits=4)

# 
# Model Info:
#   
#   function:     stan_glmer
# family:       binomial [log]
# formula:      y ~ treatment + (1 | center)
# algorithm:    sampling
# priors:       see help('prior_summary')
# sample:       24000 (posterior sample size)
# observations: 273
# groups:       center (8)
# 
# Estimates:
#   mean   sd     2.5%
# (Intercept)                             -1.5    0.4   -2.4
# treatment                                0.2    0.1    0.0
# b[(Intercept) center:1]                  0.1    0.4   -0.7
# b[(Intercept) center:2]                  1.0    0.4    0.3
# b[(Intercept) center:3]                  0.7    0.4   -0.1
# b[(Intercept) center:4]                 -0.9    0.5   -2.0
# b[(Intercept) center:5]                 -0.3    0.5   -1.2
# b[(Intercept) center:6]                 -1.1    0.7   -2.5
# b[(Intercept) center:7]                 -0.5    0.6   -1.8
# b[(Intercept) center:8]                  0.9    0.4    0.1
# Sigma[center:(Intercept),(Intercept)]    1.1    0.9    0.2
# mean_PPD                                 0.4    0.0    0.3
# log-posterior                         -164.2    3.1 -171.3
# 25%    50%    75% 
# (Intercept)                             -1.7   -1.5   -1.2
# treatment                                0.2    0.2    0.3
# b[(Intercept) center:1]                 -0.2    0.1    0.3
# b[(Intercept) center:2]                  0.7    1.0    1.2
# b[(Intercept) center:3]                  0.5    0.7    1.0
# b[(Intercept) center:4]                 -1.2   -0.8   -0.5
# b[(Intercept) center:5]                 -0.6   -0.3    0.0
# b[(Intercept) center:6]                 -1.5   -1.0   -0.6
# b[(Intercept) center:7]                 -0.8   -0.5   -0.1
# b[(Intercept) center:8]                  0.6    0.9    1.2
# Sigma[center:(Intercept),(Intercept)]    0.5    0.8    1.3
# mean_PPD                                 0.3    0.4    0.4
# log-posterior                         -166.1 -163.8 -162.0
# 97.5%
# (Intercept)                             -0.7 
# treatment                                0.5 
# b[(Intercept) center:1]                  1.0 
# b[(Intercept) center:2]                  1.9 
# b[(Intercept) center:3]                  1.6 
# b[(Intercept) center:4]                  0.1 
# b[(Intercept) center:5]                  0.7 
# b[(Intercept) center:6]                  0.0 
# b[(Intercept) center:7]                  0.6 
# b[(Intercept) center:8]                  1.8 
# Sigma[center:(Intercept),(Intercept)]    3.4 
# mean_PPD                                 0.4 
# log-posterior                         -159.3 
# 
# Diagnostics:
#   mcse Rhat n_eff
# (Intercept)                           0.0  1.0   6521
# treatment                             0.0  1.0  14954
# b[(Intercept) center:1]               0.0  1.0   7017
# b[(Intercept) center:2]               0.0  1.0   6507
# b[(Intercept) center:3]               0.0  1.0   6882
# b[(Intercept) center:4]               0.0  1.0  10274
# b[(Intercept) center:5]               0.0  1.0   8842
# b[(Intercept) center:6]               0.0  1.0  11331
# b[(Intercept) center:7]               0.0  1.0  11365
# b[(Intercept) center:8]               0.0  1.0   6844
# Sigma[center:(Intercept),(Intercept)] 0.0  1.0   5700
# mean_PPD                              0.0  1.0  24000
# log-posterior                         0.0  1.0   5086
# 
# For each parameter, mcse is Monte Carlo standard error, n_eff is a crude measure of effective sample size, and Rhat is the potential scale reduction factor on split chains (at convergence Rhat=1).

#MATCHES REPORT RR IN PAPER
betas <- extract(s1$stanfit, pars="beta",permuted = TRUE)
quantile(exp(betas$beta[,1]),c(.5,.025,.975))
# 50%      2.5%     97.5% 
# 1.2718988 0.9975039 1.6539170 

#Probability of favorable response with treatment
mean(betas$beta[,1]>0)
#[1] 0.974

