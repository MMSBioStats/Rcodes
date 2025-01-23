library(JMdesign)

SigmaTheta <- matrix(c(1.2,0.0,0.0,0.0,0.7,0.0,0.0,0.0,0.8),nrow=3,ncol=3)

N        <-  40; # Total sample size;
nevents  <-  20; # Number of events;
tmedian  <-  3.5; # median survival;
meantf   <-  1.4; # mean follow-up time;
beta     <-  0.8; # Effect of the trajectory;
alpha    <-  0.05;# Type-I Error (2-sided);
sigmae_2 <-  .5; # measurement error;

## schedule of measurement;
t <- c(1,3,5,7,9) ; # maximum 2 year follow-up;

## Input estimated proportion subjects with 2,3,4,5,6 measurements;
## This is \xi in formula 4.6;
## The data is obtained from the simulated data for the calculation in table 2;
p <- c(0,0.5, 0, 0.1, 0.4 );

## Input the order of trajectories
ordtraj <- 1 ## linear trajectories

## Call function
## Linear Trajectories
pLSl <- powerLongSurv(N, nevents, tmedian, meantf, p, t, SigmaTheta,
                      sigmae_2, ordtraj, beta, 
                      alpha=0.0025)
pLSl
