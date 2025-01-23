#code to create normal curves EXPONENTIATED, IE RR SCALE
mu<-(-.3) #posterior mean log scale
sd.diff<-.26 #posterior sd log scale

#no box around graph
curve((dlnorm(x, meanlog = mu, sdlog = sd.diff, log = FALSE)),0,3,ylab=" ",
      xlab="Relative Risk",bty="n")

#mean=0, sd=0.35
curve((dlnorm(x, meanlog = 0, sdlog = 0.35, log = FALSE)),0,3,ylab=" ",
      xlab="Relative Risk",bty="n")

#mean=0, sd=1000
curve((dlnorm(x, meanlog = 0, sdlog = 10, log = FALSE)),0,3,ylab=" ",
      xlab="Relative Risk",bty="n")

#if you want box around
curve(dnorm(x,mean=mu, sd=sd.diff),-1.5,.8,ylab=" ",
      xlab="log Relative Risk")

colorArea <- function(from, to, density, ..., col="blue", dens=NULL){
  y_seq <- seq(from, to, length.out=500)
  d <- c(0, density(y_seq, ...), 0)
  polygon(c(from, y_seq, to), d, col=col, density=dens)
}

#if you want shaded area
colorArea(from=-1.5, to=0, dnorm, mean=mu, sd=sd.diff, col=2)
#rest of curve shaded different color
colorArea(from=0, to=.8, dnorm, mean=mu, sd=sd.diff, col="gray")

#add another curve, eg prior distribution
curve(dnorm(x,mean=0, sd=0.35),-1.5,.8,ylab=" ",
      xlab="log Relative Risk",add=T,lwd=3,col="blue")
