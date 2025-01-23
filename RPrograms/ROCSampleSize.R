library(pROC)

power.roc.test(ncases=60,ncontrols=60,
               sig.level = 0.05,
               power = 0.9)
power.roc.test(auc=.78,
               power = 0.9,kappa=1)

# Computing sample size for comparing two AUCs
# always set B1=B2=1
# A1 and A2 should be set to correspond with wanted AUCs
# for each curve
# delta is difference in AUC to detect

# Calculating AUC with different values of A1
# AUC=pnorm(A1/sqrt(1+B^2))
pnorm(c(1.85,1.35)/sqrt(1+1^2))

ob.params <- list(A1=1.85, B1=1, A2=1.35, B2=1, 
                  rn=0.6, ra=0.6, delta=0.15)
power.roc.test(ob.params, power=0.8, sig.level=0.05)

# Additional parameters to calculate samp size 
# based on partial AUC
ob.params <- list(A1=2.6, B1=1, A2=1.9, B2=1, 
                  rn=0.6, ra=0.6, delta=0.037,
                  FPR11=0,
                  FPR12=0.2, FPR21=0, FPR22=0.2)

# Sample size per group to ensure that AUC is no lower than theta0
zbeta<-qnorm(.8,lower=F)
zalpha<-qnorm(.025)
theta1<-.6
theta0<-.55

(zbeta*sqrt(theta1*(1-theta1))+zalpha*sqrt(theta0*(1-theta0)))^2/(theta1-theta0)^2

# Sample size – confidence interval for AUROC
# theta=expected AUC
# W = width of confidence interval
# P=proportion of sample having the disease
theta<-.9
W<-.12
P<-.5
Q1 = theta/(2-theta) 
Q2 = 2*theta^2/(1+theta) 
#The standard normal deviate for α = Zα = 1.9600
se.goal = abs(W/(2*zalpha) )
a = P*(1-P)*(se.goal)^2 
b = -(P*(Q1-Q2) + Q2 - theta^2) 
c1 = Q1 + Q2 - theta-theta^2
