mu1<-.414
s2<-.248^2
s12<-.25*s2

s12<-.5*s2

y1<-mvrnorm(15,c(mu1,.6*mu1),Sigma=matrix(c(s2,s12,s12,s2),nrow=2))
y2<-mvrnorm(15,c(mu1,mu1),Sigma=matrix(c(s2,s12,s12,s2),nrow=2))
 
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
n1<-n2<-1000             #15
mu1<-.414
sd1<-.248
nsim<-1000

for(s in 1:nsim){
  b0<-rnorm(n1+n2,mu1,sd=)
  y1<-rnorm(n1,mu1,sd=sd1)
  y2<-rnorm(n2,mu1,sd=sd1)
  
  y15<-rnorm(n1,.5*y1-.6,sd=sd1)
  y25<-rnorm(n2,.5*y2,sd=sd1)
  
  fake.data<-data.frame(c(y1[,1],y1[,2],y2[,1],y2[,2]),c(rep(1,n1),rep(90,n1),rep(1,n1),
                                           rep(90,n1)),
                        c(rep(1,n1+n2),rep(0,n1+n2)),c(1:15,1:15,16:30,16:30))
  
  names(fake.data)<-c("y","time","group","id")
  
  model1fake<-lmer(y~time*group+(1|id),data=fake.data)
  
  theta.hat<-fixef(model1fake)["year:group"]
  theta.se<-se.fixef(model1fake)["year:group"]
  signif[s]<-(theta.hat-2*theta.se)>0
  
}
mean(signif)