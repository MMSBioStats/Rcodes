library(MASS)
n1<-n2<-15
mu1<-.09
s1<-.07^2
s2<-.13^2
s12<-.25*s2

s12<-.5*s2

y1<-mvrnorm(15,c(mu1,1.56*mu1),Sigma=matrix(c(s1,s12,s12,s2),nrow=2))
y2<-mvrnorm(15,c(mu1,2.22*mu1),Sigma=matrix(c(s1,s12,s12,s2),nrow=2))
 
fake.data<-data.frame(c(y1[,1],y1[,2],y2[,1],y2[,2]),c(rep(1,n1),rep(90,n1),rep(1,n1),
                                                       rep(90,n1)),
                      c(rep(1,n1+n2),rep(0,n1+n2)),c(1:15,1:15,16:30,16:30))

names(fake.data)<-c("y","time","group","id")

m1<-lmer(y~time*group+(1|id),data=fake.data)

d1<-y1[,1]-y1[,2]
d2<-y2[,1]-y2[,2]

sd(d1)
sd(d2)
sd(d1-d2)
t.test(d1,d2)

signif<-rep(NA,1000)
n1<-n2<-15            #15
mu1<-.09
sd1<-.11
nsim<-1000

for(s in 1:nsim){
  y1<-mvrnorm(15,c(mu1,.05+mu1),Sigma=matrix(c(s1,s12,s12,s2),nrow=2))
  y2<-mvrnorm(15,c(mu1,.13+mu1),Sigma=matrix(c(s1,s12,s12,s2),nrow=2))
  
  fake.data<-data.frame(c(y1[,1],y1[,2],y2[,1],y2[,2]),c(rep(1,n1),rep(10,n1),rep(1,n1),
                                           rep(10,n1)),
                        c(rep(1,n1+n2),rep(0,n1+n2)),c(1:15,1:15,16:30,16:30))
  
  names(fake.data)<-c("y","time","group","id")
  
  model1fake<-lmer(y~time*group+(1|id),data=fake.data)
  
  theta.hat<-fixef(model1fake)["time:group"]
  theta.se<-se.fixef(model1fake)["time:group"]
  signif[s]<-(theta.hat+2*theta.se)<0
  
}
mean(signif)