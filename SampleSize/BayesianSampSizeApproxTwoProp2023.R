# BAYESIAN SAMPLE SIZE APPROX FOR TWO PROPORTIONS
# USING NEUTRAL INFORMATIVE PRIOR FOR RR, 0.5-2


library(flextable)
library(dplyr)
library(officer)


# Function for calculating power ----
mean_vec_gt<-function(a,probs){
  res<-NULL
  for(i in 1:length(probs)) res<-append(res,mean(a>probs[i],na.rm=T))
  return(res)
}


# effect.post= what effect we're calculating power for; eg, any difference or <10% reduction, can be vector

bayes_bin_prop<-function(ptreat,pcontrol,ntreat,ncontrol,effect.post){
  ytreat <- rbinom(1, ntreat, ptreat)
  ycontrol <- rbinom(1, ncontrol, pcontrol)
  
  logRR <- log((ytreat / ntreat) / (ycontrol / ncontrol))
  
  ##OBSERVED RATES
  obsmu <- logRR
  obssd <- sqrt(1 / ytreat - 1 / ntreat + 1 / ycontrol - 1 / ncontrol)
  
  postvar1 <- 1 / (1 / priorsd1 ^ 2 + 1 / obssd ^ 2)
  postmu1 <- (priormu1 / (priorsd1 ^ 2) + obsmu / (obssd ^ 2)) * postvar1
  
   return(round(pnorm(
      effect.post,
      mean = postmu1,
      sd = sqrt(postvar1),
      lower = (ptreat < pcontrol)
    ),4))
  
}


# set seed
set.seed(23171)

# post prob thresholds to declare success
prob.thresh<-c(.95,.9,.85,.8,.75,.7)

prob.thresh<-sort(prob.thresh)

# expected rates in each group
pcontrol<-0.17
ptreat<-0.11
# ptreat<-pcontrol*.7

# can use frequentist samp size estimate to help guide starting sample size for Bayesian power

# pw1<-power.prop.test(p1 = pcontrol, p2 =ptreat, sig.level=.2, power=0.8)
# print(paste("Total Sample size =", ceiling(pw1$n/1)*2))
# 
# ntreat<-ncontrol<-round((pw1$n)*.8)



# Setting prior mean and sd in the log scale --------
#priormu1<-log(.67)
priormu1<-log(1)

priorsd1<-log(3)/(1.96)
# priorsd1<-1


# matrix to store results
res.all<-NULL

results2<-results<-matrix(NA, nrow=length(ptreat),ncol=length(prob.thresh))

# Number of simulations to use
nsim<-20

# Sample size in each group; can be a vector for more than one scenario
n.samp<-c(135,200,230)
res.all<-list()

# log RR of clinically important effect sizes
eff.size<-c(0,log(.95),log(.9))

for(i in 1:length(n.samp)){
  ntreat<-ncontrol<-n.samp[i]
  sims<-replicate(nsim,bayes_bin_prop(ptreat,pcontrol,ntreat,ncontrol,effect.post = eff.size))
  
  
  
  #calculating power for different post probability thresholds
  # change this if you want different thresholds
  results<-matrix(NA,nrow=nrow(sims),ncol=length(prob.thresh))
  
  for(k in 1:nrow(sims)){
    results[k,]<-mean_vec_gt(sims[k,],prob.thresh)
  }
  
# Turning power results into dataframe and adding column names
res<-data.frame(p.control=pcontrol,p.treat=ptreat,prior.mean=priormu1,prior.sd=priorsd1,n.group=ntreat,round(results,2))

names(res)[6:(5+length(prob.thresh))]<-paste0(prob.thresh*100,"%")
# res$size.ben<-c("Any","5%","10%")

res.all[[i]]<-res
}

res.all

# Collapsing over effect size of interest
all_res<-dplyr::bind_rows(res.all)

all_res$size.ben<-rep(round(eff.size,2)*(-100),length(n.samp))

# Create flextable -----------
ft <- all_res%>%filter(size.ben==0)%>%
  select(p.control,p.treat,n.group,
         # size.ben,
         "70%","75%","80%","85%","90%","95%")%>%
  flextable() %>% set_header_labels(p.control = "", 
                        p.treat = "",  n.group = ""
                        # size.ben=""
                       ) %>% 
 set_table_properties( layout = "autofit", width = .8)
ft<-ft %>% add_header_row(values = c("Control Rate", "Treatment Rate", 
                                       "Num of patients/group",
                                       # "RR Reduction",
 "Bayesian power for different levels of evidence*"),
  colwidths = c(1,1,1,ncol_keys(ft)-3)) %>% 
  add_footer_lines( values = "*Power to declare efficacy when Prob(RR < 1 ) > specified threshold")
ft<-ft <- set_caption(ft,
                      caption = "Bayesian power to declare efficacy depending on level of evidence (probability threshold)")

ft<-ft %>% align(align = "center",part="body") %>% align(align = "center",part="header") %>% autofit()

prop1<-prop_section(
  page_size = page_size(orient = "portrait"), type = "continuous"
)

save_as_docx(ft,
             path=paste0("BayesianPower_",
                         Sys.Date(),".docx"),
             pr_section = prop1)


#---------------------------------------------

