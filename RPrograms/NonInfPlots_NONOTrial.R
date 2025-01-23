ncontrol<-300
ntreat<-300

pcontrol<-.35
ptreat<-.32
# out_control<-300*.35
# out_treat<-300*.35

y1<-rbern(ncontrol,pcontrol)
p1<-mean(y1)
y2<-rbern(ntreat,ptreat)
p2<-mean(y2)

# p1<-out_treat/ntreat
# p2<-out_control/ncontrol
# pdiff<-rnorm(10000,p2,sqrt(p2*(1-p2)/ncontrol))-rnorm(10000,p1,sqrt(p1*(1-p1)/ntreat))
# plot(density(pdiff))



##OBSERVED DIFF IN RATES

obsmu<-p2-p1
obssd<-sqrt(p2*(1-p2)/ncontrol+p1*(1-p1)/ntreat)

priormu1<-log(1)
priorsd1<-5
(qnorm(c(0.5,.025,.975),mean=priormu1, sd=priorsd1))

# POSTERIOR DIST OF PDIFF
postvar1<-1/(1/priorsd1^2+1/obssd^2)
postmu1<-(priormu1/(priorsd1^2)+obsmu/(obssd^2))*postvar1

# Bayesian results w/ NEUTRAL PRIOR
(qnorm(c(0.5,.025,.975),mean=postmu1, sd=sqrt(postvar1)))

x<-seq(-.15,.15,length.out=100)
plot(x,dnorm(x,postmu1,sqrt(postvar1)),type="l")

# Summary stat: mean & the 2.5th percentile and 97.5th percentile (quantiles)
sum_stat <- matrix(NA, ncol = 4, nrow = 7)
for (i in 1:7){
  sum_stat[i,1:3] <- (qnorm(c(0.5,.025,.975),mean=postmu1, sd=sqrt(postvar1)))

  sum_stat[i,2] <- mean(x[,i])
  sum_stat[i,3] <- quantile(x[,i], probs=.975)
}
# str(sum_stat)
sum_stat <- data.frame(sum_stat)
# sum_stat[,4] <- colnames(x)
colnames(sum_stat) <- c("left", "mean", "right", "variable")
sum_stat



p9 <- ggplot(data.frame(x = c(-.15, .15)), aes(x = x)) +
  stat_function(fun = dnorm, args = list(mean=postmu1,sd=sqrt(postvar1)),size=1.3)
p9+theme_bw()+
geom_point(inherit.aes = FALSE, 
           data = sum_stat, 
           aes(x = mean, y = variable, alpha=0.4), 
           show.legend = FALSE) +
  
  geom_errorbarh(inherit.aes = FALSE, 
                 data = sum_stat, 
                 aes(xmin = left, xmax = right, y = variable, alpha=0.4), 
                 height = 0.1, show.legend = FALSE) +
  
  

