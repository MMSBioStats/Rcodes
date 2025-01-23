
# PLOTTING NORMAL CURVES WITH SHADED AREA UNDER CURVE

colorArea <- function(from, to, density, ..., col="blue", dens=NULL){
  y_seq <- seq(from, to, length.out=500)
  d <- c(0, density(y_seq, ...), 0)
  polygon(c(from, y_seq, to), d, col=col, density=dens)
}

par(mfrow=c(2,2))
curve(dnorm(x,mean=4.54,sd=2.471),-3,12,ylab="Posterior Probability",xlab="Bayley Score Group Difference")
colorArea(from=5, to=12, dnorm, mean=4.54, sd=2.471, col=2)

curve(dnorm(x,mean=4.54,sd=2.471),-3,12,ylab="Posterior Probability",xlab="Bayley Score Group Difference")
colorArea(from=7.5, to=12, dnorm, mean=4.54, sd=2.471, col=3)

curve(dnorm(x,mean=4.54,sd=2.471),-3,12,ylab="Posterior Probability",xlab="Bayley Score Group Difference")
colorArea(from=10, to=12, dnorm, mean=4.54, sd=2.471, col=1)

curve(dnorm(x,mean=4.54,sd=2.471),-3,12,ylab="Posterior Probability",xlab="Bayley Score Group Difference")
colorArea(from=00, to=12, dnorm, mean=4.54, sd=2.471, col=4)