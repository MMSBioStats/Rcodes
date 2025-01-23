library(survival)
library(dplyr)

lung <- 
  lung %>% 
  mutate(
    status = recode(status, `1` = 0, `2` = 1)
  )
dim(lung)
names(lung)

m1<-coxph(Surv(time, status) ~ sex+age+meal.cal+tt(age), tt = function(x,t,...) x*t, data = lung)
coef(m1)
vcov(m1)

# Linear combination to calculate HR for age at time=5 

A<-matrix(c(0,1,0,5), nrow=1)
b<-matrix(coef(m1),ncol=1)
V<-vcov(m1)

lincom <- as.vector(A %*% b)
v_lincom <- A %*% V %*% t(A)
sds <- sqrt(diag(v_lincom))

# HR and 95% CI
exp(lincom+1.96*c(0,-1,1)*sds)
