##FUNCTION BELOW PLOTS THE PRIOR DIST., LIKELIHOOD, AND POSTERIOR DENSITY FOR BETA-BINOMIAL MODEL FROM .005 TO .995
##IT TAKES AS INPUTS THE ALPHA AND BETA PARAMETERS FROM THE PRIOR DIST. AND THE # OF SUCC AND FAIL FROM OBSERVED DATA
#plot.beta<-function(alpha,beta,nsuc,nfail){ 
postscript(file="/Users/cpedroza/Desktop/2012/Prior1.eps", paper="letter",horizontal=T)

par( mar=c(4,2,0.5,0.5),oma=c(0,0,0,0),mfrow=c(2,2),cex=1.4)

alpha<-3
beta<-4.5
nsuc<-15
nfail<-5
theta <- seq(0.005, 0.995, by = 0.001)
parms<-c(alpha,beta)
results<-c(nsuc,nfail)
	toplim1 <- max(dbeta(theta, parms[1] + results[1], parms[2] + results[
		2]))
	toplim2 <- max(dbeta(theta, parms[1], parms[2]))
	toplim3 <- max(dbeta(theta, results[1] + 1, results[2] + 1))
	toplim <- max(c(toplim1, toplim2, toplim3))
	print(c(toplim1, toplim2, toplim3, toplim))


	plot(theta, dbeta(theta, parms[1] + results[1], parms[2] + results[2]), type =  ##THIS PLOTS POSTERIOR
		"l",  ylim = c(0, 1.1 * toplim), xlab = "Drug Response Rate", ylab= "",yaxt="n",bty="n",lwd=3)
	lines(theta, dbeta(theta, results[1] + 1, results[2] + 1), type = "l", col="red",lwd=3) ##THIS PLOTS LIKELIHOOD
	lines(theta, dbeta(theta, parms[1], parms[2]), type = "l", col="blue",lwd=3 )             ##THIS PLOTS PRIOR
	legend(-0.1, 4.6, c("Prior (95% CrI: 0.11-0.74)", "Likelihood (15/20)", "Posterior (0.47-0.82)"), text.col= c("blue", 
                                          "red","black"),bt="n", cex=.75)

alpha<-9.2
beta<-13.8
nsuc<-15
nfail<-5
theta <- seq(0.005, 0.995, by = 0.001)
parms<-c(alpha,beta)
results<-c(nsuc,nfail)
toplim1 <- max(dbeta(theta, parms[1] + results[1], parms[2] + results[
  2]))
toplim2 <- max(dbeta(theta, parms[1], parms[2]))
toplim3 <- max(dbeta(theta, results[1] + 1, results[2] + 1))
toplim <- max(c(toplim1, toplim2, toplim3))
print(c(toplim1, toplim2, toplim3, toplim))

plot(theta, dbeta(theta, parms[1] + results[1], parms[2] + results[2]), type =  ##THIS PLOTS POSTERIOR
  "l",  ylim = c(0, 1.1 * toplim), xlab = "Drug Response Rate", ylab= "",yaxt="n",bty="n",lwd=3)
lines(theta, dbeta(theta, results[1] + 1, results[2] + 1), type = "l", col="red",lwd=3) ##THIS PLOTS LIKELIHOOD
lines(theta, dbeta(theta, parms[1], parms[2]), type = "l", col="blue",lwd=3 )             ##THIS PLOTS PRIOR
legend(-0.11, 5.5, c("Prior (95% CrI: 0.21-0.60)", "Likelihood (15/20)", "Posterior (0.41-0.71)"), text.col= c("blue", 
                                                                                        "red","black"),bt="n", cex=.75)
alpha<-3
beta<-4.5
nsuc<-375
nfail<-500-375
theta <- seq(0.005, 0.995, by = 0.001)
parms<-c(alpha,beta)
results<-c(nsuc,nfail)
toplim1 <- max(dbeta(theta, parms[1] + results[1], parms[2] + results[
  2]))
toplim2 <- max(dbeta(theta, parms[1], parms[2]))
toplim3 <- max(dbeta(theta, results[1] + 1, results[2] + 1))
toplim <- max(c(toplim1, toplim2, toplim3))
print(c(toplim1, toplim2, toplim3, toplim))

plot(theta, dbeta(theta, parms[1] + results[1], parms[2] + results[2]), type =  ##THIS PLOTS POSTERIOR
  "l",  ylim = c(0, 1.1 * toplim), xlab = "Drug Response Rate", ylab= "",yaxt="n",bty="n",lwd=3)
lines(theta, dbeta(theta, results[1] + 1, results[2] + 1), type = "l", col="red",lwd=3) ##THIS PLOTS LIKELIHOOD
lines(theta, dbeta(theta, parms[1], parms[2]), type = "l", col="blue",lwd=3 )             ##THIS PLOTS PRIOR
legend(-0.1, 20, c("Prior", "Likelihood (375/500)", "Posterior (0.71-0.78)"), text.col= c("blue", "red","black"),bt="n",cex=.75)

alpha<-9.2
beta<-13.8
nsuc<-375
nfail<-500-375
theta <- seq(0.005, 0.995, by = 0.001)
parms<-c(alpha,beta)
results<-c(nsuc,nfail)
toplim1 <- max(dbeta(theta, parms[1] + results[1], parms[2] + results[
  2]))
toplim2 <- max(dbeta(theta, parms[1], parms[2]))
toplim3 <- max(dbeta(theta, results[1] + 1, results[2] + 1))
toplim <- max(c(toplim1, toplim2, toplim3))
print(c(toplim1, toplim2, toplim3, toplim))

plot(theta, dbeta(theta, parms[1] + results[1], parms[2] + results[2]), type =  ##THIS PLOTS POSTERIOR
  "l",  ylim = c(0, 1.1 * toplim), xlab = "Drug Response Rate", ylab= "",yaxt="n",bty="n",lwd=3)
lines(theta, dbeta(theta, results[1] + 1, results[2] + 1), type = "l", col="red",lwd=3) ##THIS PLOTS LIKELIHOOD
lines(theta, dbeta(theta, parms[1], parms[2]), type = "l", col="blue",lwd=3 )             ##THIS PLOTS PRIOR
legend(-0.1, 20, c("Prior", "Likelihood (375/500)", "Posterior (0.70-0.77)"), text.col= c("blue", "red","black"),bt="n",cex=.75)

dev.off()
#}

##THIS FUNCTION IS SAME AS ABOVE EXCEPT IT PLOTS FROM 0.2 TO 0.8
plot.beta2<-function(alpha,beta,nsuc,nfail){
theta <- seq(0.2, 0.8, by = 0.01)
parms<-c(alpha,beta)
results<-c(nsuc,nfail)
	toplim1 <- max(dbeta(theta, parms[1] + results[1], parms[2] + results[
		2]))
	toplim2 <- max(dbeta(theta, parms[1], parms[2]))
	toplim3 <- max(dbeta(theta, results[1] + 1, results[2] + 1))
	toplim <- max(c(toplim1, toplim2, toplim3))
	print(c(toplim1, toplim2, toplim3, toplim))
	plot(theta, dbeta(theta, parms[1] + results[1], parms[2] + results[2]), type = 
		"l", lty = 3, ylim = c(0, 1.1 * toplim), xlab = "Value", ylab
		 = "Density", main = paste("Beta density,\nalpha = ", parms[
		1], ", beta = ", parms[2], " \n  Successes", results[1], 
		"Failures ", results[2]))
	lines(theta, dbeta(theta, results[1] + 1, results[2] + 1), type = "l", lty = 2)
	lines(theta, dbeta(theta, parms[1], parms[2]), type = "l", lty = 1)
	legend(0.6, toplim-2, c("Prior", "Likelihood", "Posterior"), lty = c(1, 2,
		3))
}

##PLOT PLACENTA PREVIA EXAMPLE USING BOTH FUNCTIONS:
plot.beta(1,1,437,543)
plot.beta2(1,1,437,543)

##PLOTTING A BETA DISTRIBUTION (TRY CHANGING PARAMETERS TO SEE HOW DENSITY CHANGES):
a<-1
b<-1
plot(theta,dbeta(theta,a,b),type="l")

##SAMPLING FROM A BETA DISTRIBUTION AND CREATING A HISTOGRAM AND PLOTTING THE DENSITY ON TOP OF IT:
theta.samp=rbeta(10000,a,b)
hist(theta.samp,freq=F)
lines(theta,dbeta(theta,a,b),lty=1)

##GETTING EMPIRICAL QUANTILES AND EXACT QUANTILES:
quantile(theta.samp,c(.025,.975))
qbeta(c(.025,.975),a,b)

