yhi<-3
ymid<-2
ypla<-4

nhi<-15
nmid<-21
npla<-36

a1<-b1<-1.5

phi<-rbeta(10000,a1+yhi,b1+nhi-yhi)
pmid<-rbeta(10000,a1+ymid,b1+nmid-ymid)
ppla<-rbeta(10000,a1+ypla,b1+npla-ypla)

mean(phi>=.15)
mean(pmid>=.15)
mean(ppla>=.15)

mean((phi-ppla)>0)
mean((phi-pmid)>0)

quantile(phi-ppla,c(.5,.025,.975))
quantile(phi,c(.025,.975))
quantile(pmid,c(.025,.975))
quantile(ppla,c(.025,.975))