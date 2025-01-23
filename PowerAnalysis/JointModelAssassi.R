library(JMdesign)

## Input elements of Sigma_theta in forumula 4.6;
SigmaTheta <- matrix(c(1.2,0.0,0.0,0.0,0.7,0.0,0.0,0.0,0.8),nrow=3,ncol=3)
SigmaTheta <- matrix(c(441,13.5,13.5,3),nrow=2,ncol=2)

N        <-  140; # Total sample size;
nevents  <-  20; # Number of events;
tmedian  <-  1.8; # median survival;
meantf   <-  1.2; # mean follow-up time;
beta     <-  0.05; # Effect of the trajectory;
alpha    <-  0.05/6;# Type-I Error (2-sided);
sigmae_2 <-  225; # measurement error;

## schedule of measurement;
t <- c(.6, 1,  2) ; # maximum 2 year follow-up;

## Input estimated proportion subjects with 2,3,4,5,6 measurements;
## This is \xi in formula 4.6;
## The data is obtained from the simulated data for the calculation in table 2;
p <- c(0.7, 0.2, 0.1);

## Input the order of trajectories
ordtraj <- 1 ## linear trajectories

## Call function
## Linear Trajectories
pLSl <- powerLongSurv(N, nevents, tmedian, meantf, p, t, SigmaTheta,
                      sigmae_2, ordtraj, beta, alpha=0.05/62)
pLSl
show(pLSl)
unclass(pLSl)
