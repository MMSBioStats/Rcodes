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
pnorm(log(.95),mean=postmu1,sd=sqrt(postvar1))

pdf(file="PosteriorEx.pdf",height=6,width=8)

#no box around graph
# x<-seq(-.7,.7,length=4000)
# y<-exp(dnorm(x,mean=postmu1, sd=sqrt(postvar1)))
# y2<-exp(dnorm(x,mean=priormu1, sd=priorsd1))
# 
# png("G:/My Drive/AdvStudyDesign/2022/plot1.png",height = 400,width = 500)
# plot(exp(x),y,ylab=" ",yaxt='n',
#      xlab="Hazard Ratio",bty="n",lwd=2,cex=1.2,type="n" )
# lines(exp(x),y2,lty=3,lwd=3)
# dev.off()
# 
# png("G:/My Drive/ClinicalTrialsMedSchool/2021/plot2.png",height = 400,width = 500)
# plot(exp(x),y,ylab=" ",yaxt='n',
#      xlab="Hazard Ratio",bty="n",lwd=2,cex=1.2 )
# lines(exp(x),y2,lty=3,lwd=3)
# dev.off()
# 
#abline(v=0)

#if you want shaded area
x<-seq(-.7,.7,length=4000)
y<-(dnorm(x,mean=postmu1, sd=sqrt(postvar1)))
y2<-(dnorm(x,mean=priormu1, sd=priorsd1))

png("G:/My Drive/AdvStudyDesign/2022/plot3.png",height = 600,width = 750)

plot((x),y,ylab=" ",yaxt='n',xaxt='n',
     xlab="Hazard Ratio",bty="n",lwd=1.5,cex=1.2 )
# lines((x),y2,lty=3,lwd=3)
axis(1, at=log(c(.5,1,1.5,2)),labels=c(.5,1,1.5,2))

colorArea(from=log(.5), to=0, (dnorm), mean=postmu1, sd=sqrt(postvar1), col="lightskyblue")
# dev.off()

#rest of curve shaded different color
colorArea(from=0, to=.5, dnorm, mean=postmu1, sd=sqrt(postvar1), col="orangered")
text(0,.05,"Treatment \nBenefit ",cex=1,adj = c(1,0),font=2)
text(0.02,.06,"Harm ",cex=1,adj = c(0,0),font=2)

curve(dnorm(x,mean=priormu1, sd=priorsd1),-.7,.7,ylab=" ",yaxt='n',
      xlab="",bty="n",add=T,col="darkorchid3",lwd=2)
text(0.2,1.2,"Neutral prior",cex=1.1,font=2,col="darkorchid3")

dev.off()

png("G:/My Drive/AdvStudyDesign/2022/plot4.png",height = 550,width = 750)

plot((x),y,ylab=" ",yaxt='n',xaxt='n',
     xlab="Hazard Ratio",bty="n",lwd=1.5,cex=1.2 )
# lines((x),y2,lty=3,lwd=3)
axis(1, at=log(c(.5,1,1.5,2)),labels=c(.5,1,1.5,2))

qs<-qnorm(c(.025,.975),mean=postmu1, sd=sqrt(postvar1))
colorArea(from=qs[1], to=qs[2], (dnorm), mean=postmu1, sd=sqrt(postvar1), col="turquoise")
# dev.off()
dev.off()

png("G:/My Drive/AdvStudyDesign/2022/plot5.png",height = 500,width = 700)

plot((x),y,ylab=" ",yaxt='n',xaxt='n',
     xlab="Hazard Ratio",bty="n",lwd=1.5,cex=1.2 )
# lines((x),y2,lty=3,lwd=3)
axis(1, at=log(c(.5,.9,1,1.5,2)),labels=c(.5,.9,1,1.5,2))

colorArea(from=-.7, to=log(.9), (dnorm), mean=postmu1, sd=sqrt(postvar1), col="purple")
# dev.off()
dev.off()