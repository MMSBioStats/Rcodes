#code to create normal curves
mu<-(-.3) #posterior mean log scale
sd.diff<-.26 #posterior sd log scale

#no box around graph
curve(dnorm(x,mean=mu, sd=sd.diff),-1.5,.8,ylab=" ",
      xlab="log Relative Risk",bty="n")
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

#-------------
colorArea <- function(from, to, density, ..., col="blue", dens=NULL){
  y_seq <- seq(from, to, length.out=500)
  d <- c(0, density(y_seq, ...), 0)
  polygon(c(from, y_seq, to), d, col=col, density=dens)
}

#no box around graph
mu<-(-.028)
sd.diff<-0.03
curve(dnorm(x,mu,sd.diff),-.15,.1,ylab=" ",
      xlab="Risk Difference (Treatment - Control)",bty="n",axes=F)
axis(1,at=c(-.15,-.1,-.05,0,.05,.1),
     labels = c(-.15,-.10,-.05,0,.05,.10))

#if you want shaded area
colorArea(from=.05, to=.15, dnorm, mean=mu,
          sd=sd.diff, col="red")
colorArea(from=-.15, to=.05, dnorm, mean=mu,
          sd=sd.diff, col="steelblue1")
abline(v=.05,lty=3,lwd=3)
text(x=-.1,11,
     paste0("Pr(RD < 0.05)=",round(pnorm(.05,mu,sd.diff,lower=T),3)))
text(x=-.1,12,"RD 0.028, (95% CrI: -0.09, 0.03)")
text(x=.067,13,"NI Margin")
     
mu<-(-.028)
sd.diff<-0.03
curve(dnorm(x,mu,sd.diff),-.15,.1,ylab=" ",
      xlab="Risk Difference (Treatment - Control)",bty="n",axes=F)
axis(1,at=c(-.15,-.1,-.05,0,.05,.1),
     labels = c(-.15,-.10,-.05,0,.05,.10))

#if you want shaded area
colorArea(from=.02, to=.15, dnorm, mean=mu,
          sd=sd.diff, col="red")
colorArea(from=-.15, to=.02, dnorm, mean=mu,
          sd=sd.diff, col="steelblue1")
abline(v=.02,lty=3,lwd=3)
text(x=-.1,11,
     paste0("Pr(RD < 0.02)=",round(pnorm(.02,mu,sd.diff,lower=T),2)))
text(x=-.1,12,"RD 0.028, (95% CrI: -0.09, 0.03)")

text(x=.037,13,"NI Margin")

#-------------
#no box around graph
curve(dnorm(x,25,2.4),18,32,ylab=" ",
      xlab="Standard Normal Curve",bty="n",axes=F)
axis(1)


#if you want shaded area
colorArea(from=19, to=26, dnorm, mean=25, sd=2.4, col="skyblue2")

# Standard Normal with shaded areas -------
colorArea2 <- function(from, to, density, ..., col="blue", dens=NULL){
  y_seq <- seq(from, to, length.out=500)
  d <- c(0, density(y_seq, ...), 0)
  polygon(c(from, y_seq, to), d, col=col, density=dens,border = "white")
}

png(filename = "G:/My Drive/BioStatsIntro/2020/Week1/StdNormal.png",
    width = 7,height = 4.7,units="in",res=300)
# pdf(file = "G:/My Drive/BioStatsIntro/2020/Week1/StdNormal.pdf",
#     width = 6,height = 9)
par(oma=c(0,0,0,0))
curve(dnorm(x,0,1),-3.5,3.5,ylab=" ",
      xlab="Z score (x/SD)",bty="n",axes=F,main="Standard Normal Curve")
axis(1)


colorArea(from=-1, to=1, dnorm, mean=0, sd=1, col="steelblue4")
colorArea(from=-2, to=-1, dnorm, mean=0, sd=1, col="steelblue3")
colorArea(from=1, to=2, dnorm, mean=0, sd=1, col="steelblue3")
colorArea(from=-3, to=-2, dnorm, mean=0, sd=1, col="steelblue1")
colorArea(from=2, to=3, dnorm, mean=0, sd=1, col="steelblue1")
colorArea(from=-3, to=-3.5, dnorm, mean=0, sd=1, col="lightblue1")
colorArea(from=3, to=3.5, dnorm, mean=0, sd=1, col="lightblue1")

colorArea2(from=-.01, to=.01, dnorm, mean=0, sd=1, col="white")
colorArea2(from=-1.01, to=-.99, dnorm, mean=0, sd=1, col="white")
colorArea2(from=.99, to=1.01, dnorm, mean=0, sd=1, col="white")
colorArea2(from=-2.01, to=-1.99, dnorm, mean=0, sd=1, col="white")
colorArea2(from=1.99, to=2.01, dnorm, mean=0, sd=1, col="white")

curve(dnorm(x,0,1),-3.5,3.5,ylab=" ",
      bty="n",add=T)

text(-.5,.2,"34.1%",col="white")
text(.5,.2,"34.1%",col="white")
text(-1.5,.06,"13.6%",col="white")
text(1.5,.06,"13.6%",col="white")
text(-2.3,.012,"2.1%",col="white",cex=.75)
text(2.32,.012,"2.1%",col="white",cex=.75)
text(-3.3,.015,"0.1%",cex=.75)
text(3.3,.015,"0.1%",cex=.75)
#arrows(-3.3,.018,-3.3,.001,length=.07)
dev.off()

png(filename = "G:/My Drive/BioStatsIntro/2020/Week1/StdNormal1.png",
    width = 4,height = 2.7,units="in",res=300)
par(oma=c(0,0,0,0),mar=c(4,0,0,0))
curve(dnorm(x,0,1),-3.5,3.5,ylab=" ",
      xlab="Standard Normal Distribution",bty="n",axes=F,main="")
axis(1)
dev.off()

png(filename = "G:/My Drive/BioStatsIntro/2020/Week1/StdNormal2.png",
    width = 4,height = 2.7,units="in",res=300)
par(oma=c(0,0,0,0),mar=c(4,0,0,0))
curve(dnorm(x,0,1),-3.5,3.5,ylab=" ",
      xlab="Standard Normal Distribution",bty="n",axes=F,main="")
axis(1)

colorArea2(from=-1, to=1, dnorm, mean=0, sd=1, col="salmon")
curve(dnorm(x,0,1),-3.5,3.5,ylab=" ",
      add=T)

text(0,.2,"68% of data",col="white")

#arrows(-3.3,.018,-3.3,.001,length=.07)
dev.off()

png(filename = "G:/My Drive/BioStatsIntro/2020/Week1/StdNormal3.png",
    width = 4,height = 2.7,units="in",res=300)
par(oma=c(0,0,0,0),mar=c(4,0,0,0))
curve(dnorm(x,0,1),-3.5,3.5,ylab=" ",
      xlab="Standard Normal Distribution",bty="n",axes=F,main="")
axis(1)

colorArea2(from=-2, to=2, dnorm, mean=0, sd=1, col="steelblue2")
curve(dnorm(x,0,1),-3.5,3.5,ylab=" ",
      add=T)

text(0,.1,"95% of data",col="white",cex=1.2)

#arrows(-3.3,.018,-3.3,.001,length=.07)
dev.off()

png(filename = "G:/My Drive/BioStatsIntro/2020/Week1/StdNormal4.png",
    width = 4,height = 2.7,units="in",res=300)
par(oma=c(0,0,0,0),mar=c(4,0,0,0))
curve(dnorm(x,0,1),-3.5,3.5,ylab=" ",
      xlab="Standard Normal Distribution",bty="n",axes=F,main="")
axis(1)

colorArea2(from=-3, to=3, dnorm, mean=0, sd=1, col="springgreen1")
curve(dnorm(x,0,1),-3.5,3.5,ylab=" ",
      add=T)

text(0,.03,"99.7% of data",col="grey14",cex=1.2)

#arrows(-3.3,.018,-3.3,.001,length=.07)
dev.off()

