# Stopping rules simulations
priormu1<-0
priorsd1<-log(2)/(1.96)
# priorsd1<-.7
#priorsd1<-10000
ntreat<-ncontrol<-850/2
pcontrol<-.22
ptreat<-round(pcontrol*.76,3)
#Harm
ptreat<-round(pcontrol*1.15,3)

# If you want second prior ----
#second prior.4/.1
prior2="yes"
if(prior2=="yes"){priormu2<-log(.88)
priorsd2<-(log(1.8)-priormu2)/(1.96)
}

nsim<-8000
res<-matrix(NA,nrow=nsim,ncol=4)
for(i in 1:nsim){
# exp(qnorm(c(.5,.025,.975),priormu2,priorsd2))
ytreat <- rbinom(1, ntreat, ptreat)
ycontrol <- rbinom(1, ncontrol, pcontrol)

logRR<-log((ytreat/ntreat)/(ycontrol/ncontrol))

##OBSERVED RATES
obsmu<-logRR
obssd<-sqrt(1/ytreat-1/ntreat+1/ycontrol-1/ncontrol)


postvar1<-1/(1/priorsd1^2+1/obssd^2)
postmu1<-(priormu1/(priorsd1^2)+obsmu/(obssd^2))*postvar1
sig<-pnorm(0,mean=postmu1,sd=sqrt(postvar1),lower=(ptreat<pcontrol))
# sig4<-pnorm(log(mic),mean=postmu1,sd=sqrt(postvar1),lower=(ptreat<pcontrol))

if(prior2=="yes"){postvar2<-1/(1/priorsd2^2+1/obssd^2)
postmu2<-(priormu2/(priorsd2^2)+obsmu/(obssd^2))*postvar2
sig2<-pnorm(0,mean=postmu2,sd=sqrt(postvar2),lower=(ptreat<pcontrol))
# sig3<-pnorm(log(mic),mean=postmu2,sd=sqrt(postvar2),lower=(ptreat[k]<pcontrol))
}
res[i,]<-c((ytreat/ntreat)/(ycontrol/ncontrol),sig,sig2,ytreat/ntreat-ycontrol/ncontrol)
}
plot(res[,2],res[,3],pch=20,xlab="Prob(RR<1) Neutral Prior",
     ylab="Prob(RR<1) Data Prior")
abline(a=0,b=1)

png(filename = paste("G:/My Drive/CycledPT/NRNProposal/StoppingRules",pcontrol,".png",sep=""),
    width = 8,height = 7,units="in",res=600)

#Informative Prior
plot(res[,1],res[,3],pch=20,xlab="Observed RR",ylab="Prob(RR<1)",
     main=paste("Mortality rates of ", pcontrol*100,"% and ",
                ptreat*100, "%, continuous vs cycled PT",sep=""))

abline(h=.95,lwd=3,col="blue",lty=3)
abline(h=.85,lwd=3,col="springgreen3")

abline(v=1,lwd=3,col="red")
# points(res[,1],res[,2],pch=20,col="gray")
# abline(h=0.5,lwd=3,col="gray")

abline(v=round(max(res[res[,3]>.85,1]),2),lwd=3,col="springgreen3")
abline(v=round(max(res[res[,3]>.95,1]),2),lwd=3,col="blue",lty=3)
text(min(res[,1])+.15,.3,paste("85%: Continue Trial for RR >",round(max(res[res[,3]>.85,1]),2)),
     cex=.85,col="springgreen3")
text(min(res[,1])+.15,.35,paste("95%: Continue Trial for RR >",round(max(res[res[,3]>.95,1]),2)),
     cex=.85,col="blue")

dev.off()

#------
max(res[res[,3]>.85,1])
max(res[res[,3]>.95,1])

max(res[res[,2]>.85,1])
max(res[res[,2]>.95,1])

min(res[res[,2]>.95,1])

plot(res[,4],res[,3],pch=20,xlab="Observed RD",ylab="Prob(RR<1)")
abline(h=.95,lwd=3,col="blue",lty=3)
abline(h=.85,lwd=3,col="blue")
max(res[res[,3]>.85,4])
max(res[res[,3]>.95,4])

# Harm scenarios
#Informative Prior
d.col<-cut(res[,2], breaks = c(-Inf,  .95, Inf), labels = c("black", "red"))
d.col<-as.character(d.col)
plot(res[,1],res[,2],pch=20,col=d.col,xlab="Observed RR",ylab="Prob(RR>1)",
     main=paste("Mortality rates of ", pcontrol*100,"% and ",ptreat*100, "%, continuous vs cycled PT",sep=""))
abline(h=.95,lwd=3,col="red")
abline(v=1,lwd=3,col="gray")
abline(v=round(min(res[res[,2]>.95,1]),2),lwd=3,col="red")
text(1.7,.35,paste("95%: Continue Trial for RR <",round(min(res[res[,2]>.95,1]),2)),
     cex=.9)


#Neutral Prior
plot(res[,1],res[,2],pch=20)
abline(h=.85,lwd=3,col="blue")
abline(v=round(min(res[res[,2]>.95,1]),2),lwd=3,col="red")

sig
sig2
