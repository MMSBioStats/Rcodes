library(tidyverse) 
library(haven) 
library(brms) #for Bayesian (multilevel) generalised linear modelling
library(modelr) #for data manipulation
library(tidybayes) #for analysis of posterior draws of a Bayesian model

d1 <- read_sav("https://github.com/MultiLevelAnalysis/Datasets-third-edition-Multilevel-book/blob/master/chapter%206/Thaieduc/thaieduc.sav?raw=true")

d2 <- d1 %>%
  mutate(SCHOOLID = factor(SCHOOLID),
         SEX = if_else(SEX == 0, "girl", "boy"),
         SEX = factor(SEX, levels = c("girl", "boy")),
         PPED = if_else(PPED == 0, "no", "yes"),
         PPED = factor(PPED, levels = c("no", "yes")))
head(d2)

d2 <- d2 %>%
  filter(!is.na(MSESC))

d2 %>%
  group_by(PPED) %>%
  summarise(REPEAT = sum(REPEAT))
names(d2)

d3<- d2 %>% slice_sample(n=500)
d3 %>%
  group_by(PPED) %>%
  summarise(REPEAT = sum(REPEAT),n=n())

m1.1 <- glm(REPEAT ~ SEX + PPED,  
          data=d3, 
          family = binomial)
X1<-model.matrix(m1.1)

m1.2 <- brm(formula = REPEAT ~ SEX + PPED+MSESC,  
          data=d3, 
          family = bernoulli(link = "logit"),
          warmup = 500, 
          iter = 2000, 
          chains = 2, 
          cores=2,
          seed = 123)

# running model ----
m1 <- brm(formula = REPEAT ~ SEX + PPED,  
                          data=d3, 
                          family = bernoulli(link = "logit"),
                          warmup = 500, 
                          iter = 2000, 
                          chains = 2, 
                          cores=2,
                          seed = 123)
plot(m1, 
     type = "trace")

summary(m1)

draws<-as.matrix(m1)
draws<-draws[,-4]
zeta<-X1%*%t(draws)
p.pred<-inv.logit(zeta)

r1<-ref_grid(m1)@ grid

# ref levels:"girl", "no"
X2<-matrix(c(1,0,0,
             1,0,1,
             1,1,0,
             1,1,1),ncol=3,byrow=T)
omega<-X2%*%t(draws)


p.pred2<-inv.logit(omega)

# Equal weighing across cells -----
p_no<-c(p.pred2[1,]*.5+p.pred2[3,]*.5)
c(mean(p_no),quantile(p_no,c(.025,.975)))

p_yes<-c(p.pred2[2,]*.5+p.pred2[4,]*.5)
c(mean(p_yes),quantile(p_yes,c(.025,.975)))

# now with emmeans ---------
e1<-emmeans(m1,  ~PPED,transform="response")
summary(e1,point="mean")
p1<-matrix(unlist(as.mcmc((e1))[[1]]),ncol=2,byrow=F)
p2<-matrix(unlist(as.mcmc((e1))[[2]]),ncol=2,byrow=F)

pp<-rbind(p1,p2)
apply(pp,2,function(x){ c(mean(x),quantile(x,c(.025,.975)))})

# Weighted by the prop in each cell ----------
p.no<-(p.pred2[1,]*r1$.wgt.[r1$SEX=="girl" & r1$PPED=="no"] /sum(r1$.wgt.[r1$PPED=="no"])+
         p.pred2[3,]*r1$.wgt.[r1$SEX=="boy" & r1$PPED=="no"]/sum(r1$.wgt.[r1$PPED=="no"] ))
c(mean(p.no),quantile(p.no,c(.025,.975)))

p.yes<-(p.pred2[2,]*r1$.wgt.[r1$SEX=="girl" & r1$PPED=="yes"] /sum(r1$.wgt.[r1$PPED=="yes"])+
          p.pred2[4,]*r1$.wgt.[r1$SEX=="boy" & r1$PPED=="yes"] /sum(r1$.wgt.[r1$PPED=="yes"]))
c(mean(p.yes),quantile(p.yes,c(.025,.975)))

# Weighted emmeans -------
e1.1<-emmeans(m1,  ~PPED,transform="response",weights="cells")
# summary(e1.1,point="mean")
p1<-matrix(unlist(as.mcmc((e1.1))[[1]]),ncol=2,byrow=F)
p2<-matrix(unlist(as.mcmc((e1.1))[[2]]),ncol=2,byrow=F)

pp<-rbind(p1,p2)
apply(pp,2,function(x){ c(mean(x),quantile(x,c(.025,.975)))})

# This is what I actually do ---------

nd<-d3
nd$PPED<-"yes"
post_yes<-posterior_epred(m1,nd)
mean(post_yes)
quantile(post_yes,c(.025,.975))

nd<-d3
nd$PPED<-"no"
post_no<-posterior_epred(m1,nd)
mean(post_no)
quantile(post_no,c(.025,.975))


# Creating draws from the marginal posterior using posterior_linpred through tidybayes -------- 
p.draws<-d3 %>%
  mutate(PPED="yes") %>%
  add_linpred_draws(m1,transform=T) 

p.draws2<-d3 %>%
  add_linpred_draws(m1,transform=T) 

p.draws3<-d3 %>%
  modelr::data_grid(SEX,PPED) %>%
  add_linpred_draws(m1,transform=T) 

# p.draws%>%
#   ggplot(aes(x = .linpred, y = interaction(SEX, PPED))) +
#   stat_pointinterval(.width = c(.68, .95)) +
#   coord_flip() +
#   xlab("predicted probability") +
#   scale_x_continuous(breaks = seq(0, 0.24, 0.02))

p.draws3 %>% group_by(PPED)%>%
  summarise(pp = mean(.linpred),lb=quantile(.linpred,c(.025)),ub=quantile(.linpred,c(.975)))

p.draws2 %>% group_by(PPED)%>%
  summarise(pp = mean(.linpred),lb=quantile(.linpred,c(.025)),ub=quantile(.linpred,c(.975)))

p.draws %>% group_by(PPED)%>%
  summarise(pp = mean(.linpred),lb=quantile(.linpred,c(.025)),ub=quantile(.linpred,c(.975)))
# `summarise()` has grouped output by 'SEX'. You can override using the `.groups` argument.
# # A tibble: 4 x 5
# # Groups:   SEX [2]
# SEX   PPED     pp     lb    ub
# <fct> <fct> <dbl>  <dbl> <dbl>
# 1 girl  no    0.173 0.122  0.235
# 2 girl  yes   0.108 0.0675 0.156
# 3 boy   no    0.196 0.138  0.262
# 4 boy   yes   0.122 0.0792 0.175

# Checking manually for one group ---------------
nd<-d3
# nd$SEX<-"girl"
nd$PPED<-"no"
lp0 <- posterior_linpred(m1, newdata = nd,transform = T)
c(mean(lp0),quantile(lp0,c(.025,.975)))
#               2.5%     97.5% 
#   0.1734749 0.1217139 0.2351575 

# Now using emmeans ----------
e1<-emmeans(m1,  ~PPED,weights = "proportional",type="response")
e1.1<-emmeans(m1,  ~PPED,transform="response",weights="cells")

g1<-ggemmeans(m1,  terms=c("PPED"))
g2<-ggpredict(m1,  terms=c("PPED"))

summary(e1.1,point="mean")

inv.logit<-function(x){
exp(x)/(1+exp(x))
}

# taking results from emmeans and summarizing to get posterior mean and 95% quantiles of marginal probabilities
p1<-matrix(unlist(as.mcmc((e1.1))[[1]]),ncol=2,byrow=F)
p2<-matrix(unlist(as.mcmc((e1.1))[[2]]),ncol=2,byrow=F)

pp<-rbind(p1,p2)
pp<-inv.logit(rbind(p1,p2))
res<-matrix(NA,nrow=4,ncol=3)

res[,1]<-apply(pp,2,mean)
res[,2:3]<-t(apply(pp,2,quantile,c(.025,.975)))
round(res,3)
# These estimates are same as the ones from posterior_linpred on line 58-61
#       [,1]  [,2]  [,3]
# [1,] 0.173 0.122 0.235
# [2,] 0.108 0.068 0.156
# [3,] 0.196 0.138 0.262
# [4,] 0.122 0.079 0.175
# 

pred.0<-exp(draws$`(Intercept)` + 0*draws$stratify + 0*draws$R34grp)
