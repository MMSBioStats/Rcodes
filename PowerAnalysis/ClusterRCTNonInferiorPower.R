library(clusterPower)

cpa.normal(nsubjects = 35, nclusters=15,d = 4, ICC = .09, vart = 121,alpha=.025)

library(simstudy)
library(parallel)
clusteredData <- function(k, m, d1, d2) {
  
  dc <- genData(k, d1)
  dc <- trtAssign(dc, grpName = "rx")
  
  di <- genCluster(dc, "site", m, level1ID = "id")
  di <- addColumns(d2, di)
  di[]
  
}
defC <- defData(varname = "ceff", formula = 0, 
                variance = 10.8, id = "site", dist = "normal")

defI2 <- defDataAdd(varname = "y", formula = "ceff + 0 * rx", 
                    variance = 121, dist = "normal")
dx <- clusteredData(k = 30, m = 35, defC, defI2)

m1<-lmer(y~rx+(1|site),data=dx)
# summary(m1)
confint(m1,parm = "rx")[1]>(-4)

genEstFromClust <- function(k, m, d1, d2) {
  dx <- clusteredData(k, m, d1, d2)
  confint(lmer(y~rx+(1|site),data=dx),parm="rx",method="Wald")[1]>(-4) #NI margin
  
  # summary(lmerTest::lmer(y ~ rx + (1|site), data = dx))$coef["rx", 5]
}

niters<-10000
resCest <- unlist(mclapply(1:niters, 
                           function(x) genEstFromClust(k=30, m = 28, defC, defI2)))

mean(resCest) # power
## [1] 0.805