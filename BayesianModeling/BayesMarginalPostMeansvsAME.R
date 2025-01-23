library(tidyverse) 
library(haven) 
library(brms) #for Bayesian (multilevel) generalised linear modelling
library(modelr) #for data manipulation
library(emmeans) 

# d1 <- read_sav("https://github.com/MultiLevelAnalysis/Datasets-third-edition-Multilevel-book/blob/master/chapter%206/Thaieduc/thaieduc.sav?raw=true")
# 
# d2 <- d1 %>%
#   mutate(SCHOOLID = factor(SCHOOLID),
#          SEX = if_else(SEX == 0, "girl", "boy"),
#          SEX = factor(SEX, levels = c("girl", "boy")),
#          PPED = if_else(PPED == 0, "no", "yes"),
#          PPED = factor(PPED, levels = c("no", "yes")))
# # head(d2)
# 
# d2 <- d2 %>%
#   filter(!is.na(MSESC))
# 
# d2 %>%
#   group_by(PPED) %>%
#   summarise(REPEAT = sum(REPEAT))
# names(d2)
# 
# d3<- d2 %>% slice_sample(n=500)
# d3 %>%
#   group_by(PPED) %>%
#   summarise(REPEAT = sum(REPEAT),n=n())
# 
# # running model ----
# m1 <- brm(formula = REPEAT ~ SEX + PPED,  
#                           data=d3, 
#                           family = bernoulli(link = "logit"),
#                           warmup = 500, 
#                           iter = 2000, 
#                           chains = 2, 
#                           cores=2,
#                           seed = 123)
# plot(m1, 
#      type = "trace")
# 
# summary(m1)
# save(d3,m1,file="G:/My Drive/RPrograms/BayesMarginalMeansExample.RData")

load("BayesMarginalMeansExample.RData")

draws<-as.matrix(m1)
draws<-draws[,-4]

r1<-ref_grid(m1)@ grid

# ref levels:"girl", "no"
X2<-matrix(c(1,0,0,
             1,0,1,
             1,1,0,
             1,1,1),ncol=3,byrow=T)

inv.logit<-function(x){
  exp(x)/(1+exp(x))
}

p.pred2<-inv.logit(X2%*%t(draws))

# Equal weighing across cells -----
p_no<-c(p.pred2[1,]*.5+p.pred2[3,]*.5)
c(mean(p_no),quantile(p_no,c(.025,.975)))
  #             2.5%     97.5% 
  # 0.1560619 0.1155091 0.2024053

p_yes<-c(p.pred2[2,]*.5+p.pred2[4,]*.5)
c(mean(p_yes),quantile(p_yes,c(.025,.975)))
  #               2.5%      97.5% 
  # 0.07032355 0.04069118 0.10759654 

# now with emmeans ---------
e1<-emmeans(m1,  ~PPED,transform="response")
summary(e1,point="mean")
p1<-matrix(unlist(as.mcmc((e1))[[1]]),ncol=2,byrow=F)
p2<-matrix(unlist(as.mcmc((e1))[[2]]),ncol=2,byrow=F)

pp<-rbind(p1,p2)
apply(pp,2,function(x){ c(mean(x),quantile(x,c(.025,.975)))})
#         [,1]       [,2]
#       0.1560619 0.07032355
# 2.5%  0.1155091 0.04069118
# 97.5% 0.2024053 0.10759654

# Weighted by the prop in each cell ----------
p.no<-(p.pred2[1,]*r1$.wgt.[r1$SEX=="girl" & r1$PPED=="no"] /sum(r1$.wgt.[r1$PPED=="no"])+
         p.pred2[3,]*r1$.wgt.[r1$SEX=="boy" & r1$PPED=="no"]/sum(r1$.wgt.[r1$PPED=="no"] ))
c(mean(p.no),quantile(p.no,c(.025,.975)))
#             2.5%     97.5% 
#   0.1565825 0.1158459 0.2029333 

p.yes<-(p.pred2[2,]*r1$.wgt.[r1$SEX=="girl" & r1$PPED=="yes"] /sum(r1$.wgt.[r1$PPED=="yes"])+
          p.pred2[4,]*r1$.wgt.[r1$SEX=="boy" & r1$PPED=="yes"] /sum(r1$.wgt.[r1$PPED=="yes"]))
c(mean(p.yes),quantile(p.yes,c(.025,.975)))
  #             2.5%      97.5% 
  # 0.06982640 0.04044542 0.10678513 
# Weighted emmeans -------
e1.1<-emmeans(m1,  ~PPED,transform="response",weights="cells")
# summary(e1.1,point="mean")
p1<-matrix(unlist(as.mcmc((e1.1))[[1]]),ncol=2,byrow=F)
p2<-matrix(unlist(as.mcmc((e1.1))[[2]]),ncol=2,byrow=F)

pp<-rbind(p1,p2)
apply(pp,2,function(x){ c(mean(x),quantile(x,c(.025,.975)))})
#         [,1]       [,2]
#       0.1565825 0.06982640
# 2.5%  0.1158459 0.04044542
# 97.5% 0.2029333 0.10678513

# This is what I actually do; I think they call them Average Marginal Effects;  ---------
# although mainly it's because i'm interested in the adjusted risk difference or RR derived from the model
nd<-d3
nd$PPED<-"yes"
post_yes<-posterior_epred(m1,nd)
c(mean(post_yes),quantile(post_yes,c(.025,.975)))
# 2.5%      97.5% 
#   0.07023167 0.02678980 0.13610736 

nd<-d3
nd$PPED<-"no"
post_no<-posterior_epred(m1,nd)
c(mean(post_no),
quantile(post_no,c(.025,.975)))
# 2.5%      97.5% 
#   0.15587517 0.07274441 0.25776004 

