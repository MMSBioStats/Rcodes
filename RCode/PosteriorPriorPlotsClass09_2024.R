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
r1<-1.54
r2<-2.76
obsmu<-log(r1)
obssd<-(log(r2)-log(r1))/1.96

exp(obsmu+1.96*obssd)
exp(obsmu-1.96*obssd)

priormu1<-log(1)
# priorsd1<-log(2)/(1.96)
priorsd1<-0.57

postvar1<-1/(1/priorsd1^2+1/obssd^2)
postmu1<-(priormu1/(priorsd1^2)+obsmu/(obssd^2))*postvar1

exp(qnorm(c(0.5,.025,.975),mean=postmu1, sd=sqrt(postvar1)))

pnorm(0,mean=postmu1,sd=sqrt(postvar1),lower=F)
pnorm(log(1.1),mean=postmu1,sd=sqrt(postvar1),lower=F)

# pdf(file="PosteriorEx.pdf",height=6,width=8)

# PRIOR PLOT -------
#no box around graph
x<-seq(-2,2,length=4000)
# y<-exp(dnorm(x,mean=postmu1, sd=sqrt(postvar1)))
y2<-(dnorm(x,mean=priormu1, sd=priorsd1))
# 
png("C:/Users/cpedroza/OneDrive - UTHealth Houston/AdvStudyDesign/2024/Week2/plot1.png",height = 4,width = 6.5,units="in",res=300)
plot((x),y2,ylab=" ",yaxt='n',
     xlab="log Relative Risk",bty="n",lwd=2,cex=1.1,type="n",main="Prior Distribution" )
# lines(exp(x),y2,lty=3,lwd=3)
colorArea(from=0, to=2, dnorm, mean=priormu1, sd=priorsd1, col="skyblue")
colorArea(from=-2, to=0, dnorm, mean=priormu1, sd=priorsd1, col="red")
text(-.04,.05,"Normothermia \nBenefit ",cex=.8,adj = c(1,0),font=2)
text(0.05,.05,"Hypothermia \nBenefit ",cex=.8,adj = c(0,0),font=2)
text(1.3,.5,"RR 1.0 \n95% CrI: 0.33-3.0 ",cex=.7,adj = c(0,0),font=1)

dev.off()


# 
# png("G:/My Drive/ClinicalTrialsMedSchool/2021/plot2.png",height = 400,width = 500)
# plot(exp(x),y,ylab=" ",yaxt='n',
#      xlab="Hazard Ratio",bty="n",lwd=2,cex=1.2 )
# lines(exp(x),y2,lty=3,lwd=3)
# dev.off()
# 
#abline(v=0)

x<-seq(-2,2,length=4000)
y<-(dnorm(x,mean=postmu1, sd=sqrt(postvar1)))
y2<-(dnorm(x,mean=priormu1, sd=priorsd1))

png("C:/Users/cpedroza/OneDrive - UTHealth Houston/AdvStudyDesign/2024/Week2/plot3.png",height = 5.5,width = 7.5,units="in",res=300)

plot((x),y,ylab=" ",yaxt='n',xaxt='n',
     xlab="Relative Risk",bty="n",type="l",lwd=3)
# lines((x),y2,lty=3,lwd=3)
axis(1, at=log(c(.25,.33,.5,1,1.5,2,3,3.5)),labels=c(.25,0.33,.5,1,1.5,2,3,3.5))

colorArea(from=0, to=2, (dnorm), mean=postmu1, sd=sqrt(postvar1), col="lightskyblue")
# dev.off()

#rest of curve shaded different color
colorArea(from=-2, to=0, dnorm, mean=postmu1, sd=sqrt(postvar1), col="orangered")
text(0.05,.05,"Hypothermia \nBenefit ",cex=1,adj = c(0,0),font=2)
# text(0.02,.06,"Harm ",cex=1,adj = c(0,0),font=2)

curve(dnorm(x,mean=priormu1, sd=priorsd1),-2,2,ylab=" ",yaxt='n',
      xlab="",bty="n",add=T,col="darkorchid3",lwd=2)
text(-0.5,.7,"Neutral prior",cex=1.1,font=1,col="darkorchid3")

dev.off()

png("C:/Users/cpedroza/OneDrive - UTHealth Houston/AdvStudyDesign/2024/Week2/plot4.png",height = 5.5,width = 7.50,units="in",res=300)

plot((x),y,ylab=" ",yaxt='n',xaxt='n',
     xlab="Relative Risk",bty="n",type="l",lwd=3,xlim=c(log(.5),log(2.5)))
# lines((x),y2,lty=3,lwd=3)
axis(1, at=log(c(.5,1,1.5,2.5)),labels=c(.5,1,1.5,2.5))


qs<-qnorm(c(.025,.975),mean=postmu1, sd=sqrt(postvar1))
colorArea(from=qs[1], to=qs[2], (dnorm), mean=postmu1, sd=sqrt(postvar1), col="turquoise")
# dev.off()
dev.off()

png("C:/Users/cpedroza/OneDrive - UTHealth Houston/AdvStudyDesign/2024/Week2/plot5.png",height = 5,width = 7,units="in",res=300)

plot((x),y,ylab=" ",yaxt='n',xaxt='n',
     xlab="Relative Risk",bty="n",type="l",lwd=3,xlim=c(log(.5),log(3.5)))
# lines((x),y2,lty=3,lwd=3)
axis(1, at=log(c(.5,1,1.5,3.5)),labels=c(.5,1,1.5,3.5))

colorArea(from=log(1.1), to=log(3.5), (dnorm), mean=postmu1, sd=sqrt(postvar1), col="purple")
# dev.off()
dev.off()