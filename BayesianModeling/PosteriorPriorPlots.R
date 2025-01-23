#code to create normal curves
colorArea <- function(from, to, density, ..., col="blue", dens=NULL){
  y_seq <- seq(from, to, length.out=500)
  d <- c(0, density(y_seq, ...), 0)
  polygon(c(from, y_seq, to), d, col=col, density=dens)
}

# PLOTTING POSTERIOR & PRIOR  ------------------
#-----------------------------
###NEUTRAL PRIOR w/ 95% CrI: 0.5-2.0
#-----------------------------
r1<-0.75
r2<-1.02
obsmu<-log(r1)
obssd<-(log(r2)-log(r1))/1.96

exp(obsmu+1.96*obssd)
exp(obsmu-1.96*obssd)

priormu1<-log(1)
priorsd1<-log(2)/(1.96)
#priorsd1<-0.57

postvar1<-1/(1/priorsd1^2+1/obssd^2)
postmu1<-(priormu1/(priorsd1^2)+obsmu/(obssd^2))*postvar1

exp(qnorm(c(0.5,.025,.975),mean=postmu1, sd=sqrt(postvar1)))

pnorm(0,mean=postmu1,sd=sqrt(postvar1),lower=t)
pnorm(log(.9),mean=postmu1,sd=sqrt(postvar1))

pdf(file="PosteriorEx.pdf",height=6,width=8)

#no box around graph
x<-seq(-.7,.7,length=4000)
y<-exp(dnorm(x,mean=postmu1, sd=sqrt(postvar1)))
y2<-exp(dnorm(x,mean=priormu1, sd=priorsd1))

png("G:/My Drive/ClinicalTrialsMedSchool/2021/plot1.png",height = 350)
plot(exp(x),y,ylab=" ",yaxt='n',
     xlab="Hazard Ratio",bty="n",lwd=2,cex=1.2,type="n" )
lines(exp(x),y2,lty=3,lwd=3)
dev.off()

png("G:/My Drive/ClinicalTrialsMedSchool/2021/plot2.png",height = 300,width = 500)
plot(exp(x),y,ylab=" ",yaxt='n',
     xlab="Hazard Ratio",bty="n",lwd=2,cex=1.2 )
lines(exp(x),y2,lty=3,lwd=3)
dev.off()

#abline(v=0)

#if you want shaded area
colorArea(from=-.6, to=0, dnorm, mean=postmu1, sd=sqrt(postvar1), col="lightskyblue")
#rest of curve shaded different color
colorArea(from=0, to=.4, dnorm, mean=postmu1, sd=sqrt(postvar1), col="orangered")
text(0,.05,"Treatment \nBenefit ",cex=1,adj = c(1,0),font=2)
text(0.15,.06,"Treatment \nHarm ",cex=1,adj = c(0,0),font=2)

curve(dnorm(x,mean=priormu1, sd=priorsd1),-.6,.4,ylab=" ",yaxt='n',
      xlab="",bty="n",add=T,col="darkorchid3",lwd=3)
text(0.2,1.2,"Neutral prior",cex=1.1,font=2,col="darkorchid3")

dev.off()
