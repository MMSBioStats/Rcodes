n<-120
# p_con<-c(.125,.04,.36,.475)
# p_trt<-c(.125,.03,.29,.555)

p_con<-c(.125,.04,.12,.24,.475)
p_trt<-c(.125,.028,.084,.168,.595)

pocl_simulate2 <- function(n, p_con,p_trt, levs=1:length(p_con)){
treatment<-rep(c("A","B","C"),each=n/3)



y1<-sample(levs, n/3,r=T, prob=p_con)
y2<-sample(levs, n/3, r=T,prob=p_trt)
y3<-sample(levs, n/3, r=T,prob=p_trt)

dat<-data.frame(y=as.factor(c(y1,y2,y3)),Treatment=treatment)
return(dat[dat$y!=1,])
}
d1<-pocl_simulate2(60000,p_con,p_trt)

fit<-polr(y~Treatment,data=d1[d1$y!=1,])

table(d1)[,1]/20000
table(d1)[,2]/20000
table(d1)[,3]/20000


n<-120
p_con<-c(.125,.04,.36,.475)
p_trt<-c(.125,.03,.29,.555)
#RR 0.7
p_trt<-c(.125,.028,.25,.6)

pow1 <-
  replicate(1000,
            pocl_Bayes_test(
              pocl_simulate2(n, p_con,p_trt),
              priormu = c(0, log(1.2)),
              priorvar = c(.7 ^ 2, .35 ^ 2)
            ))

#Neutral Prior
mean(pow1[1,1,]>.8 | pow1[2,1,]>.8)
mean(pow1[1,1,]>.9 | pow1[2,1,]>.9)
