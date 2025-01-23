# BAYESIAN SAMPLE SIZE APPROX FOR TWO PROPORTIONS
# USING NEUTRAL INFORMATIVE PRIOR FOR RR, 0.5-2

#For saving results
setwd("G:/My Drive/ObGyn/")

library(flextable)
rm(list=ls())

mean_vec_gt<-function(a,probs){
  res<-NULL
  for(i in 1:length(probs)) res<-append(res,mean(a>probs[i],na.rm=T))
  return(res)
}
# Number of simulations to use
nsim<-2000
set.seed(231715)
# post prob thresholds to declare success
prob.thresh<-c(.95,.9,.85,.8,.75,.7)

prob.thresh<-sort(prob.thresh)
# sample size in each group

# expected rates in each group
pcontrol<-0.11
ptreat<-0.2
# ptreat<-pcontrol*.7

pw1<-power.prop.test(p1 = pcontrol, p2 =ptreat, sig.level=.2, power=0.8)
print(paste("Total Sample size =", ceiling(pw1$n/1)*2))

ntreat<-ncontrol<-round((pw1$n)*.8)

ntreat<-70
ncontrol<-277
# ntreat<-ncontrol<-352

# you can specify one or more rates in the treatment group 
# code will loop through them
# if(pcontrol==.7) ptreat<-c(.8,.85,.9,.95) else ptreat<-c(.16)    
# 
# #minimum clinically important effect
# mic<-0.85

#-----------------------------
###NEUTRAL PRIOR w/ 95% CrI: 0.25-4
#-----------------------------

#priormu1<-log(.67)
priormu1<-0
priorsd1<-log(2)/(1.96)
 priorsd1<-.7
#priorsd1<-10000

# If you want second prior ----
#second prior.4/.1
prior2="no"
if(prior2=="yes"){priormu2<-0
priorsd2<-.7
}

# matrix to store results
res.all<-NULL

results2<-results<-matrix(NA, nrow=length(ptreat),ncol=length(prob.thresh))

# Looping through num of treat rates and sims -----
for(k in 1:length(ptreat)){
  sig4<-sig3<-sig2<-sig<-rep(NA,nsim)
for(i in 1:nsim){
  ytreat <- rbinom(1, ntreat, ptreat[k])
  ycontrol <- rbinom(1, ncontrol, pcontrol)
  
  logRR<-log((ytreat/ntreat)/(ycontrol/ncontrol))
  
  ##OBSERVED RATES
  obsmu<-logRR
  obssd<-sqrt(1/ytreat-1/ntreat+1/ycontrol-1/ncontrol)
  

postvar1<-1/(1/priorsd1^2+1/obssd^2)
postmu1<-(priormu1/(priorsd1^2)+obsmu/(obssd^2))*postvar1
sig[i]<-pnorm(0,mean=postmu1,sd=sqrt(postvar1),lower=(ptreat[k]<pcontrol))
# sig4[i]<-pnorm(log(mic),mean=postmu1,sd=sqrt(postvar1),lower=(ptreat[k]<pcontrol))

if(prior2=="yes"){postvar2<-1/(1/priorsd2^2+1/obssd^2)
postmu2<-(priormu2/(priorsd2^2)+obsmu/(obssd^2))*postvar2
sig2[i]<-pnorm(0,mean=postmu2,sd=sqrt(postvar2),lower=(ptreat[k]<pcontrol))
# sig3[i]<-pnorm(log(mic),mean=postmu2,sd=sqrt(postvar2),lower=(ptreat[k]<pcontrol))
}
}

  #calculating power for different post probability thresholds
  # change this if you want different thresholds

results[k,]<-mean_vec_gt(sig,prob.thresh)

if(prior2=="yes") results2[k,]<-mean_vec_gt(sig2,prob.thresh)
}

# Turning power results into dataframe and adding column names
res<-as.data.frame(round(results,2))

names(res)<-paste0(prob.thresh*100,"%")
res$p.control<-pcontrol
res$p.treat<-ptreat
res$prior.mean<-priormu1
res$prior.sd<-priorsd1
res$n.group<-ntreat

res.all<-res[,c((length(prob.thresh)+1):(length(prob.thresh)+5),1:length(prob.thresh))]
res.all

if(prior2=="yes"){res2<-as.data.frame(round(results2,2))

names(res2)<-paste0(prob.thresh*100,"%")
res2$p.control<-pcontrol
res2$p.treat<-ptreat
res2$prior.mean<-priormu2
res2$prior.sd<-priorsd2
res2$n.group<-ntreat

res2<-res2[,c((length(prob.thresh)+1):(length(prob.thresh)+5),1:length(prob.thresh))]
res2

res.all<-rbind(res.all,res2)
}
res.all

n.res1<-res.all
n.res2<-res.all
n.res3<-res.all

all_res<-rbind(n.res1,n.res2,n.res3)
all_res<-all_res[all_res$prior.sd==1,]
all_res<-all_res[order(all_res$n.group),]
# mean_vec_gt(sig4,.75)
# 
# mean_vec_gt(sig3,.75)

# Write results to word tables -----------
ft <- flextable(all_res[,-c(3,4)]) 
ft <- set_header_labels(ft, p.control = "", 
                        p.treat = "",  n.group = ""
                       )
ft <- set_table_properties(ft, layout = "autofit", width = .8)
ft
ft <- ft %>% add_header_row(values = c("Control Rate", "Treatment Rate", 
                                       "Num of patients/group",
 "Bayesian power for different levels of evidence"),
  colwidths = c(1,1,1, 5)) %>% 
  add_footer_lines( values = "*Power to declare efficacy when Prob(RR<1.0) > specified threshold")
ft<-ft <- set_caption(ft,
                      caption = "Bayesian power to declare efficacy depending on level of evidence (probability threshold)")

ft<-ft %>% align(align = "center",part="body") %>% align(align = "center",part="header")

# if(ncontrol==100) ft1<-ft else ft2<-ft

save_as_docx(ft,
             path=paste0("G:/My Drive/ObGyn/Sabai/BayesianPower_",Sys.Date(),".docx"))

# Write results to file --------
# write.csv(res.all,file=
#             paste("BayesPowerN",2*ntreat,"pcontrol",pcontrol,"NeutPrior",
#                   ".csv",sep=""),row.names=F)
# if(prior2=="yes") write.csv(res2,file=
#                               paste("BayesPowerN",2*ntreat,"pcontrol",pcontrol,"EnthPrior",
#                                     ".csv",sep=""),row.names=F)


#---------------------------------------------

# Plots ---------------------------
temp = list.files(pattern="BayesPowerN*")
myfiles = lapply(temp, read.csv)
res.all<-do.call(rbind,myfiles)

res.all$teffect<-res.all$p.treat-res.all$p.control
names(res.all)[6:9]<-c("power_.75","power_.80","power_.85","power_.90")

res.all2<-reshape(res.all,varying = c(6:9), direction="long" ,sep="_",
                  timevar="threshold",times=c("75%","80%","85%","90%"))

res.all2<-res.all2[res.all2$teffect>.11,]

res.all2.1<-res.all2[res.all2$prior.mean==0 & res.all2$threshold>.75,]

res.all2.1$p.control<-factor(res.all2.1$p.control,labels=c("pcontrol=60%","pcontrol=70%"))
res.all2.1$n.group<-factor(res.all2.1$n.group,labels=c("N/group=20","N/group=30"))

f<-ggplot(data=res.all2.1, aes(x=teffect, y=power,color=factor(threshold)))+ 
  geom_hline(yintercept = .8,color="darkgray")+
  geom_line(aes(color=factor(threshold)),size=.6,show.legend = FALSE) +
  theme_bw()+
  geom_point(aes(fill=factor(threshold)),size=2,stroke=0)+ 
  coord_cartesian(xlim = range(res.all2$teffect), ylim = c(0,1)) + 
  scale_y_continuous(breaks=c(0,.2,.4,.6,.8,1))+
  scale_x_continuous(breaks=c(.15,.2,.25,.3))+
  scale_color_discrete("Probability Threshold",
                       labels = c("80% ","85% ", "90%"))+  
 
  theme(legend.text=element_text(size=8,margin = margin(r = 3, unit = "pt")),
        legend.title = element_text(size=8),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-8,-5,0,-5),
        legend.key.size =  unit(0.1, "in"))+
  theme(axis.title=element_text(size=10),
        axis.text.x = element_text(size=7),
        plot.title=element_text(size=10,face="bold"))+
  theme(legend.position="bottom", legend.box = "horizontal") +
  # theme(plot.margin = unit(c(0,0,0,0), "cm"))+ #top, right, bottom, left
  labs(x="Absolute Risk Difference (Feto - Conventional)",y="Power",
       title="Neutral Prior")+
  guides(fill = FALSE)+  facet_wrap(vars(p.control,n.group),strip.position="top",ncol=4)


res.all2.2<-res.all2[res.all2$prior.mean>0 & res.all2$threshold>.75,]
res.all2.2$p.control<-factor(res.all2.2$p.control,labels=c("pcontrol=60%","pcontrol=70%"))
res.all2.2$n.group<-factor(res.all2.2$n.group,labels=c("N/group=20","N/group=30"))

f2<-ggplot(data=res.all2.2, aes(x=teffect, y=power,color=factor(threshold)))+ 
  geom_hline(yintercept = .8,color="darkgray")+
  geom_line(aes(color=factor(threshold)),size=.6,show.legend = FALSE) +
  theme_bw()+
  geom_point(aes(fill=factor(threshold)),size=2,stroke=0)+ 
  coord_cartesian(xlim = range(res.all2$teffect), ylim = c(0,1)) + 
  scale_y_continuous(breaks=c(0,.2,.4,.6,.8,1))+
  scale_x_continuous(breaks=c(.15,.2,.25,.3))+
  
  scale_color_discrete("Probability Threshold",
                       labels = c("80% ","85% ", "90%"))+
  # scale_color_manual("Probability Threshold",
  #                    labels = c("0.95 ","0.90 "),
  #                    values=c("05"="magenta2","1"="dodgerblue"))+
  theme(legend.text=element_text(size=8,margin = margin(r = 3, unit = "pt")),
        legend.title = element_text(size=8),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-8,-5,0,-5),
        legend.key.size =  unit(0.1, "in"))+
  theme(axis.title=element_text(size=10),
        axis.text.x = element_text(size=7),
        plot.title=element_text(size=10,face="bold"))+
  theme(legend.position="bottom", legend.box = "horizontal") +
  # theme(plot.margin = unit(c(0,0,0,0), "cm"))+ #top, right, bottom, left
  labs(x="Absolute Risk Difference (Feto - Conventional)",y="Power",
       title="TOTAL Trial Prior")+
  guides(fill = FALSE)+  facet_wrap(vars(p.control,n.group),strip.position="top",ncol=4)

pdf(file=paste("PowerFigs.pdf",sep=""),height = 8)
grid.arrange(f,f2,ncol=1)
dev.off()
