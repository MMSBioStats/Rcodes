#these programs estimate bias conditional on  stopping relative to continuing to the #end ( see Freidlin and Korn CT 2009 for details) # function bias_fs2 simulates a trial with one interim analysis # sample calls
#bias_fs2(179,20000,c(64.5,129),c(2.9626,1.9686),.75) - OBF boundary
#bias_fs2(179,100000,c(64.5,129),c(3.0902,1.965),1) -  H-P boundary # function bias_fs3 simulates a trial with two interim analyses # sample calls
#bias_fs3(179,20000,c(43,86,129),c(3.7103,2.5114,1.993),.75)  - OBF boundary
#bias_fs3(179,20000,c(43,86,129),c(3.0902,3.0902,1.971),.75) -  HP boundary # function bias_fs4 simulates a trial with three interim analyses # sample calls
#bias_fs4(179,10000,c(32.5,65,97.5,130),c(4.3326,2.9631,2.3590,2.0141),.75)
#bias_fs4(179,10000,c(32.5,65,97.5,130),c(3.0902,3.0902,3.0902,1.98),.75)

bias_fs2<-function(seed,nsim,inf,bnd,hra)
{
  #seed - seed for random variable generator #nsim - number of simulations #inf - information vector, contains information at the first and the final analyses #bnd - interim monitoring bounds (on Z scale) #hra - true hazard ratio
  
  set.seed(seed)
  lhra<-log(hra)
  print(date())
  zva<-0
  pva<-0
  pvfa<-0
  is<-0
  is2<-0
  zva2<-0
  umle<-0
  ufmle<-0
  zv3a<-0
  for (isim in 1:nsim)
  {
    ifm<-rbind(inf[1],inf[2])
    
    #information structure for the interim analysis
    res<-sqrt(c(inf[1],inf[2]-inf[1]))*rnorm(2,0,1)
    
    res<-res+rbind(inf[1]*lhra,(inf[2]-inf[1])*lhra)
    
    zv<-matrix(cumsum(res)/sqrt(ifm),1,2)
    umle<-umle+(zv[1]/sqrt(inf[1]))
    if (zv[2]<(-1.96))  ufmle<-ufmle+(zv[2]/sqrt(inf[2]))
    
    pv<-(zv<(-bnd))
    #pvf<-(zv[2]<(-1.96))
    zva<-rbind(zva,zv)
    
    #if (pv[1]>0) { zva<-rbind(zva,zv); is<-is+1} #if ((pv[1]==0) & (pv[2]>0)) { zva2<-rbind(zva2,zv); is2<-is2+1}
    
    pva<-rbind(pva,pv)
  }
  zva<-zva[2:(isim+1),]
  
  pva<-pva[2:(isim+1),]
  
  zvfo<-zva[,2]
  zvfo<-zvfo[order(zvfo)]
  
  is1<-sum(pva[,1])
  
  if (is1>0)  mc1<-exp(sum((zva[,1]*(pva[,1]==1))/sqrt(ifm[1]))/is1)
  
  print("Power for fixed sample size trial") print(sum(zva[,2]<(-1.96))/nsim); print("Overall power") print(sum((pva[,1]+pva[,2])>0)/nsim);
  print("prob of stop at first look")
  print(is1/nsim);
  print("unconditional mle at the last look") print( exp(sum(zva[,2]/sqrt(ifm[2]))/nsim)  )
  print("")
  print("mle at the last look conditional on positive trial (the third approach, no interim looks)") print( exp(sum((zva[,2]*(zva[,2]<(-1.96)))/sqrt(ifm[2]))/sum(zva[,2]<(-1.96)))  )
  print("")
  print("conditional mle at first look")
  print(mc1)
  print(c("final mean of the best",is1/nsim))
  print(exp(sum((zvfo[1:is1]/sqrt(ifm[2])))/is1))
  print("")
}


bias_fs3<-function(seed,nsim,inf,bnd,hra)
{
  #seed - seed for random variable generator #nsim - number of simulations #inf - information vector, contains information at the first and the final analyses #bnd - interim monitoring bounds (on Z scale) #hra - true hazard ratio
  
  set.seed(seed)
  lhra<-log(hra)
  
  print(date())
  
  zva<-0
  pva<-0
  pvfa<-0
  is<-0
  is2<-0
  zva2<-0
  umle<-0
  ufmle<-0
  zv3a<-0
  for (isim in 1:nsim)
  {
    ifm<-rbind(inf[1],inf[2],inf[3])
    
    #information structure for the interim analysis
    res<-sqrt(c(inf[1],inf[2]-inf[1],inf[3]-inf[2]))*rnorm(3,0,1)
    
    res<-res+rbind(inf[1]*lhra,(inf[2]-inf[1])*lhra,(inf[3]-inf[2])*lhra)
    
    zv<-matrix(cumsum(res)/sqrt(ifm),1,3)
    umle<-umle+(zv[1]/sqrt(inf[1]))
    if (zv[3]<(-1.96))  ufmle<-ufmle+(zv[3]/sqrt(inf[3]))
    
    pv<-(zv<(-bnd))
    zva<-rbind(zva,zv)
    pva<-rbind(pva,pv)
  }
  zva<-zva[2:(isim+1),]
  
  pva<-pva[2:(isim+1),]
  
  zvfo<-zva[,3]
  zvfo<-zvfo[order(zvfo)]
  
  is1<-sum(pva[,1])
  is2<-sum((pva[,1]==0) & (pva[,2]>0))
  
  if (is1>0)  mc1<-exp(sum((zva[,1]*(pva[,1]==1))/sqrt(ifm[1]))/is1)
  if (is2>0)  mc2<-exp(sum((zva[,2]*((pva[,1]==0) & (pva[,2]>0)))/sqrt(ifm[2]))/is2)
  
  print("Power for fixed sample size")
  print(sum(zva[,3]<(-1.96))/nsim);
  print("Overal power")
  print(sum((pva[,1]+pva[,2]+pva[,3])>0)/nsim);
  print("prob stop at first look")
  print(is1/nsim);
  print("prob stop at second look")
  print(is2/nsim);
  print("unconditional mle at the last look") print( exp(sum(zva[,3]/sqrt(ifm[3]))/nsim)  )
  print("")
  print((sum(zva[,3]<(-3.7103))/nsim))
  print("mle at the last look conditional on positive(3.7103) trial (the third approach, no interim looks)") print( exp(sum((zva[,3]*(zva[,3]<(-3.7103)))/sqrt(ifm[3]))/sum(zva[,3]<(-3.7103)))  )
  print("")
  print("conditional mle at first look")
  print(mc1)
  print(c("final mean of the best",is1/nsim))
  print(exp(sum((zvfo[1:is1]/sqrt(ifm[3])))/is1))
  print("")
  print("conditional mle at second look")
  print(mc2)
  print(c("final mean of the NEXT best",is2/nsim))
  print(exp(sum((zvfo[(is1+1):(is1+is2)]/sqrt(ifm[3])))/is2))
  print("")
  print(date())
}

bias_fs4<-function(seed,nsim,inf,bnd,hra)
{
  set.seed(seed)
  lhra<-log(hra)
  
  print(date())
  zva<-0
  pva<-0
  pvfa<-0
  is<-0
  is2<-0
  zva2<-0
  umle<-0
  ufmle<-0
  zv3a<-0
  for (isim in 1:nsim)
  {
    ifm<-rbind(inf[1],inf[2],inf[3],inf[4])
    
    #information structure for the interim analysis
    res<-sqrt(c(inf[1],inf[2]-inf[1],inf[3]-inf[2],inf[4]-inf[3]))*rnorm(4,0,1)
    
    res<-res+rbind(inf[1]*lhra,(inf[2]-inf[1])*lhra,(inf[3]-inf[2])*lhra,(inf[4]-inf[3])*lhra)
    
    zv<-matrix(cumsum(res)/sqrt(ifm),1,4)
    umle<-umle+(zv[1]/sqrt(inf[1]))
    if (zv[4]<(-1.96))  ufmle<-ufmle+(zv[4]/sqrt(inf[4]))
    
    pv<-(zv<(-bnd))
    
    zva<-rbind(zva,zv)
    
    pva<-rbind(pva,pv)
  }
  zva<-zva[2:(isim+1),]
  
  pva<-pva[2:(isim+1),]
  #pvfa<-pvfa[2:(isim+1),]
  
  zvfo<-zva[,4]
  zvfo<-zvfo[order(zvfo)]
  
  
  is1<-sum(pva[,1])
  is2<-sum((pva[,1]==0) & (pva[,2]>0))
  is3<-sum((pva[,1]==0) & (pva[,2]==0) & (pva[,3]>0))
  
  if (is1>0)  mc1<-exp(sum((zva[,1]*(pva[,1]==1))/sqrt(ifm[1]))/is1)
  if (is2>0)  mc2<-exp(sum((zva[,2]*((pva[,1]==0) & (pva[,2]>0)))/sqrt(ifm[2]))/is2) if (is3>0)  mc3<-exp(sum((zva[,3]*((pva[,1]==0) & (pva[,2]==0) & (pva[,3]>0)))/sqrt(ifm[3]))/is3)
  
  
  print("Power for fixed sample size")
  print(sum(zva[,4]<(-1.96))/nsim);
  print("Overal power")
  print(sum((pva[,1]+pva[,2]+pva[,3]+pva[,4])>0)/nsim);
  print("prob stop at first look")
  print(is1/nsim);
  print("prob stop at second look")
  print(is2/nsim);
  print("prob stop at third look")
  print(is3/nsim);
  print("unconditional mle at the last look") print( exp(sum(zva[,4]/sqrt(ifm[4]))/nsim)  )
  print("")
  print("mle at the last look conditional on positive trial (no interim looks)") print( exp(sum((zva[,4]*(zva[,4]<(-1.96)))/sqrt(ifm[4]))/sum(zva[,4]<(-1.96)))  )
  print("")
  print("conditional mle at first look")
  print(mc1)
  print(c("final mean of the best",is1/nsim))
  print(exp(sum((zvfo[1:is1]/sqrt(ifm[4])))/is1))
  print("")
  print("conditional mle at second look")
  print(mc2)
  print(c("final mean of the NEXT best",is2/nsim))
  print(exp(sum((zvfo[(is1+1):(is2+is1)]/sqrt(ifm[4])))/is2))
  print("")
  print("conditional mle at third look")
  print(mc3)
  print(c("final mean of the NEXT best",is3/nsim))
  print(exp(sum((zvfo[(is1+is2+1):(is3+is2+is1)]/sqrt(ifm[4])))/is3))
  print("")
  
}
