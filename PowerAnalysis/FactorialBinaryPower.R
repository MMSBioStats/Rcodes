
# 2x2 factorial design
# assumes no MULTIPLICATIVE interaction between two factors 

.54*c(1.3,1.2,1.3*1.2)
round(.4*c(1.3,1.2,1.3*1.2),2)

# here we assume we know rates of response
mydat <- data.frame( v1 = rep( 0:1, each=2),
                     v2 = rep( 0:1, 2 ), 
                     # resp=c(0.54,.70,.65,.84) )
resp=c(0.40,.52,.48,.62) )

#calculating betas for simulations
fit0 <- glm( resp ~ v1*v2, data=mydat,
             weight=rep(100000,4), family=binomial(link="log"))
b0 <- coef(fit0)

# function uses GEE poisson to ensure 
simfunc2 <- function( beta=b0, n=10000 ) {
  w <- sample(1:4, n, replace=TRUE)
  mydat2 <- mydat[w, 1:2]
  eta <- with(mydat2,  cbind( 1, v1, 
                              v2 ) %*% beta )
  p <- exp(eta)
  mydat2$resp <- rbinom(n, 1, p)
  mydat2$id<-c(1:n)
  
  fit1 <- geese( resp ~ v1+v2,id=id, data=mydat2,
               family=poisson,corstr="exch")
  summary(fit1)$mean[2:3,4]
}


out <- replicate(1000, simfunc2(b0[1:3], 940))
mean( out[1,] <= 0.05 )
mean( out[2,] <= 0.05 )



# Using logit link
simfunc <- function( beta=b0, n=10000 ) {
  w <- sample(1:4, n, replace=TRUE)
  mydat2 <- mydat[w, 1:2]
  eta <- with(mydat2,  cbind( 1, v1, 
                              v2 ) %*% beta )
  p <- exp(eta)/(1+exp(eta))
  mydat2$resp <- rbinom(n, 1, p)
  
  fit1 <- glm( resp ~ v1*v2, data=mydat2,
               family=binomial)
  summary(fit1)$coef[2:3,4]
}

out <- replicate(1000, simfunc(b0, 1500))
mean( out[1,] <= 0.05 )
mean( out[2,] <= 0.05 )