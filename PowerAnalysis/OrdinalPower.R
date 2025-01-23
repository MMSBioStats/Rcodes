library(Hmisc)
OR.cal<-function(p1,p2){
  (p1/(1-p1))/(p2/(1-p2))
}


odds<-OR*p2/(1-p2)
p1<-odds/(1+odds)
p1<-p1/sum(p1)

x<-1:4
p1<-c(.125,.04,.36,.475)
X <- rep(1 : 4, 10000 * p1)
c(mean=mean(X), median=median(X))

pomodm(x,p1,1)
p1<-c(.125,.04,.36,.475)

# No BPD:47.5%
# BPD grade 1: 24%
# BPD 2: 12%
# Grade 3: 4%
# Death: 12.5%
p2<-c(.125,.04,.12,.24,.475)

prbs<-c(.12,.035,.375,.47)

prbs<-c(.12,.035,.125,.25,.47)

# POWER --
popower(prbs,c(2.45),n=180,alpha=.05)
popower(p2,c(2.1),n=120,alpha=.2)

paki<-c(20,55,11,14)/100

probs<-c(50,26,6,5,7)
probs<-probs/sum(probs)

ors<-1/c(1.2,1.5,1.7,2,2.5,3,3.8)
popower(probs,ors,n=150)

popower(c(.2,.55,.11,.14),c(1.5,1.7,2,2.5,3),n=150)

popower(c(.55,.30,.07,.08),ors,n=150)
pcontrol<-c(.55,.30,.07,.08)

popower(c(.2,.55,.11,.14),ors,n=150)

popower(c(.2,.55,.11,.14),2.5,n=150)

posamsize(c(.2,.55,.11,.14),c(1.5,1.7,2.5))

# Compute the mean and median x after shifting the probability
# distribution by an odds ratio under the proportional odds model
x <- 1 : 4
p <- c(.2, .55, .11, .14)
# For comparison make up a sample that looks like this
X <- rep(1 : 4, 20 * p)
c(mean=mean(X), median=median(X))
pomodm(x, p, odds.ratio=1)  # still have to figure out the right median
pomodm(x, p, odds.ratio=0.5)

#-------------
pdoor<-c(.26,.07,.3,.20,.17)
sum(pdoor)


popower(pdoor,c(1.4),n=1000)
