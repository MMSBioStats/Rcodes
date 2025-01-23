# Example taken from here
# https://towardsdatascience.com/implementing-and-interpreting-ordinal-logistic-regression-1ee699274cf5
# 
library(MASS)
library(carData)
library(bayesplot)

data(WVS) 

d1<-WVS[sample(1:nrow(WVS),500,replace = F),]

model_fit <- polr(poverty~country, data = d1, Hess = TRUE)
summary(model_fit)
# Call:
#   polr(formula = poverty ~ country, data = d1, Hess = TRUE)
# 
# Coefficients:
#   Value Std. Error t value
# countryNorway -0.4718     0.2287  -2.063
# countrySweden -0.7726     0.2542  -3.039
# countryUSA     0.8305     0.2411   3.445
# 
# Intercepts:
#   Value   Std. Error t value
# Too Little|About Right -0.0155  0.1430    -0.1086
# About Right|Too Much    1.7830  0.1719    10.3719
# 
# Residual Deviance: 943.3611 
# AIC: 953.3611 

m1<-brm(poverty~country,family=cumulative,data=d1,
        prior =set_prior("normal(0,1)"),warmup = 2000, iter = 20000,chains=3,seed=39483,cores = 3)
summary(m1,digits=4)

# Family: cumulative 
# Links: mu = logit; disc = identity 
# Formula: poverty ~ country 
# Data: d1 (Number of observations: 500) 
# Draws: 3 chains, each with iter = 20000; warmup = 2000; thin = 1;
# total post-warmup draws = 54000
# 
# Population-Level Effects: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat
# Intercept[1]     -0.01      0.14    -0.28     0.26 1.00
# Intercept[2]      1.79      0.17     1.47     2.13 1.00
# countryNorway    -0.45      0.22    -0.89    -0.02 1.00
# countrySweden    -0.74      0.24    -1.22    -0.27 1.00
# countryUSA        0.81      0.23     0.35     1.27 1.00
# Bulk_ESS Tail_ESS
# Intercept[1]     52873    38714
# Intercept[2]     59037    42935
# countryNorway    54976    43349
# countrySweden    55729    42154
# countryUSA       55830    39644
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
# disc     1.00      0.00     1.00     1.00   NA       NA
# Tail_ESS
# disc       NA
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).


# inspect trace plots 
plot(m1)

#Extracting beta draws
betas <- as_draws_df(m1, variable = "^b_country", regex = TRUE)

#calculating probability of each country having a positive  perception about government's efforts to reduce poverty
#
mean(betas$b_countryUSA > 0)
# [1] 0.9998148
mean(betas$b_countryNorway > 0)
# [1] 0.01924074
mean(betas$b_countrySweden > 0)
# [1] 0.001111111
