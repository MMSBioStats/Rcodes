###### FUNCTION TO CALCULATE BETA PARAMETERS IMPLIED BY 95% CI OF 0.5-2 FOR RR
beta.params<-function(pc,upCI){
  qc<-1-pc
  s2<-(log(upCI)/1.96)^2
  if(pc<.5){a1<-2*qc*pc/(s2*pc)*1.05   
            b1<-a1*qc/pc}
  else if(pc>=0.5 & pc<.6){a1<-2*qc*pc/(s2*pc)*1.065   
                           b1<-a1*qc/pc}
  else if(pc>=0.6 & pc<.7){a1<-2*qc*pc/(s2*pc)*1.12  
                           b1<-a1*qc/pc}
  else if(pc>=0.7 & pc<.8){a1<-2*qc*pc/(s2*pc)*1.235  
                           b1<-a1*qc/pc}
  else if(pc>=0.8 & pc<.9){a1<-2*qc*pc/(s2*pc)*1.38  
                           b1<-a1*qc/pc}
  else {a1<-2*qc*pc/(s2*pc)*1.732  
        b1<-a1*qc/pc}
  return(c(a1,b1))
}

###### FUNCTION TO CALCULATE BETA PARAMETERS IMPLIED BY 95% CI OF 0.23-4.3 FOR RR

beta.params2<-function(pc,upCI){
  qc<-1-pc
  s2<-(log(upCI)/1.96)^2
  if(pc<.5){a1<-2*qc*pc/(s2*pc)*1.3
            b1<-a1*qc/pc}
  else if(pc>=0.5 & pc<.6){a1<-2*qc*pc/(s2*pc)*1.425
                           b1<-a1*qc/pc}
  else if(pc>=0.6 & pc<.7){a1<-2*qc*pc/(s2*pc)*1.6 
                           b1<-a1*qc/pc}
  else if(pc>=0.7 & pc<.8){a1<-2*qc*pc/(s2*pc)*1.8 
                           b1<-a1*qc/pc}
  else if(pc>=0.8 & pc<.9){a1<-2*qc*pc/(s2*pc)*2.2
                           b1<-a1*qc/pc}
  else {a1<-2*qc*pc/(s2*pc)*2.7 
        b1<-a1*qc/pc}
  return(c(a1,b1))
}
