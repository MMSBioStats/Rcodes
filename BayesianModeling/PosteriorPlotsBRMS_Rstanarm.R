#Creating pretty plots from rstanarm or brms fitted models
# taken from
# https://cran.r-project.org/web/packages/tidybayes/vignettes/tidy-brms.html

library(rstanarm)
library(tidybayes)
library(ggplot2)
library(dplyr)
library(ggdist)
library(cowplot)

theme_set(theme_tidybayes() + panel_border())

### Logistic regression
data(wells)
head(wells)
wells$dist100 <- wells$dist / 100
fit2 <- stan_glm(
  switch ~ dist100 + arsenic, 
  data = wells, 
  family = binomial(link = "logit"), 
  prior_intercept = normal(0, 10),
  QR = TRUE,
  refresh = 0,
  # for speed of example only
  chains = 3, iter = 5000 
)
print(fit2)
prior_summary(fit2)
get_variables(fit2)

# plotting distribution of OR for variable arsenic
# different color for area under OR of 1.5; change to whatever you want or just leave out "fill" argument 

fit2 %>%
  spread_draws(arsenic) %>%
  mutate(OR = exp(arsenic)) %>%
  ggplot(aes(x = OR,fill = stat(abs(x) < 1.5))) + stat_slab()+ylab("")+
  xlab("Odds Ratio for Arsenic")+theme(axis.ticks.y = element_blank(),
                                       axis.text.y = element_blank())+
  theme(legend.position="none")
