z.alpha<-qnorm(.025,lower.tail = F)
z.beta<-qnorm(.2,lower.tail = F)

sd.c<-20000 # SD for cost
sd.q<-.2  #SD for outcome
Q<-(-.02)   #difference in effectiveness
C<-10000     #difference in cost
W<-20000   #willingness to pay
rho<-(.1)    #correlation btw eff and cost

num<-2*(z.alpha+z.beta)^2*(sd.c^2+(W*sd.q)^2-(2*W*rho*sd.c*sd.q))
den<-(W*Q-C)^2
n<-num/den

#Sample size per group
cat(paste("Sample size per group: ",round(n)))


l1<-340*(100000*.05-250)^2
l2<-2*(5000^2+(100000*.2)^2-(2*100000*(-.1)*5000*.2))

pnorm(sqrt(l1/l2)-z.alpha)

#-------
sd.c<-20000
sd.q<-.43
Q<-(.02)
C<-5000
W<-1000
rho<-(.1)

num<-2*(z.alpha+z.beta)^2*(sd.c^2+(W*sd.q)^2-(2*W*rho*sd.c*sd.q))
den<-(W*Q-C)^2
n<-num/den
n
