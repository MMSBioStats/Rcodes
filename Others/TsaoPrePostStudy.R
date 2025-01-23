
a1<-rbeta()

pi<-.1

pi<-.10
cv<- .4              #.205
k<-(1-pi)/pi


alpha1<-(k/(cv^2)-1)/(1+k)
beta1<-alpha1*k
qbeta(c(.025,.975),alpha1,beta1)


pi<-.24
cv<- .4                #.205
k<-(1-pi)/pi


alpha2<-(k/(cv^2)-1)/(1+k)
beta2<-alpha2*k
qbeta(c(.025,.975),alpha1,beta1)

y1<-11
n1<-64

y2<-69
n2<-304

p1<-rbeta(10000,alpha1+y1,n1-y1+beta1)

p2<-rbeta(10000,alpha2+y2,n2-y2+beta2)

alpha3<-.23*250
beta3<-250-alpha3

p1<-rbeta(10000,alpha3+y1,n1-y1+beta3)
p2<-rbeta(10000,alpha3+y2,n2-y2+beta3)

n1<-100
y1<-17

p1<-rbeta(10000,1+y1,n1-y1+1)
p2<-rbeta(10000,1+y2,n2-y2+1)




mean(p1<p2)


