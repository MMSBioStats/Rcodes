#Updated 11/30/22
# Updated 1/3/2023
# # Updated 2/23/2023


#Load Hmisc library
library(Hmisc)
# library(doBy)
#Packages to create tables
library(arsenal)
library(gtsummary)
library(lubridate)
library(readxl)
library(dplyr)
library(flextable)
library(labelled)

# source("G:/My Drive/Blakely/HIP Trial/FinalAnalysis/BaselineTable.R")
# source("G:/My Drive/Blakely/HIP Trial/FinalAnalysis/Readmits.R")

rm(list=ls())

load(paste0("./Data/BaselineDataset_2023-02-23.RData"))


# load(paste0("G:/My Drive/Blakely/HIP Trial/FinalAnalysis/Data/DatasetTables_",Sys.Date(),".RData"))
load(paste0("./Data/DatasetTables_2023-02-23.RData"))


# Creating Table 1----------------
baselinedata$ega_birth_weeks<-as.numeric(baselinedata$ega_birth_weeks)

var_label(baselinedata)<-list(ega_birth_weeks="EGA completed weeks",
                             birth_weight="Birthweight, grams",
                             singleton_mult="Singleton",
                             inborn_outborn="Inborn",
                             apgar_1="Apgar 1 min",apgar_5="Apgar 5 min",apgar_10="Apgar 10 min")
t1 <- baselinedata %>% select(early_late,age_enrol_days,gest_age_category,
                               ega_birth_weeks,
                pma_enrollment,gender,race,hispanic,birth_weight,
                singleton_mult,inborn_outborn,apgar_1,apgar_5,apgar_10,
                apnea_preenroll,apnea_pre_int,brady_preenroll,
                brady_pre_int,ivh_prior_enroll,ivh_34,
                incarceration_prior_enroll) %>% 
  tbl_summary(by=early_late,type = apgar_10~"continuous",
              value=list(singleton_mult~"Singleton",inborn_outborn~"Inborn"),
              missing_text="Missing") %>% 
  modify_caption(caption = "Table 1. Baseline characteristics") %>% as_flex_table()


# Creating Table 3----------------
ids.earlydeath<-data.tables$study_id[!is.na(data.tables$preih_death) & data.tables$preih_death==1]


var_label(data.tables)<-list(other_circum="Circumcision",
                  other_gtube="G tube",
                  umb_hernia="Umbilical hernia repair",
                  other_proc="Other",
                  list_other_procedure_same_ane___2="Orchiopexy",
                  reason_no_repair="Reason no IH repair",nicu_los="Initial NICU stay, days",
                  total_los_readmit="Readmission total LOS, days")

# t3<-data.tables %>% filter(study_id%nin%ids.earlydeath) %>%
t3<-data.tables %>% 
  select(early_late, ih_nonapparent,op_technique,gen_anesthesia,postih_los,
        
         total_los_readmit,
             repair_study_period,reason_no_repair,other_proc_same_ane,
              other_circum,other_gtube,umb_hernia,list_other_procedure_same_ane___2,
              other_proc,
              readmit,num_readmit) %>% 
  tbl_summary(by=early_late, type=list(all_continuous() ~ "continuous2",
                                       num_readmit~"continuous2"),
              digits = list(postih_los~1,total_los_readmit~1,num_readmit~1),
              
              missing_text="Missing",
              statistic = all_continuous() ~ c("{mean} ({sd})",
                                               "{median} ({p25}, {p75})", "{min}, {max}"),
  ) %>%
  modify_caption(caption = "Table 3. Secondary outcomes") %>% 
  modify_header(label ~ "Outcome") %>% as_flex_table()

t3.1<-data.tables %>% filter(study_id!="18560-20") %>%
  select(early_late,nicu_los,postih_los,
         
         total_los_readmit,total_los) %>% 
  tbl_summary(by=early_late, type=list(all_continuous() ~ "continuous2", total_los~"continuous2"),
              digits = list(nicu_los~1,postih_los~1,total_los~1),
              missing_text="Missing",
              statistic = all_continuous() ~ c("{mean} ({sd})",
                                               "{median} ({p25}, {p75})", "{min}, {max}"),
  ) %>%
  modify_caption(caption = "Table 3.1 Secondary outcomes; excludes infant with incorrect discharge date") %>% 
  modify_header(label ~ "Outcome") %>% as_flex_table()


# Creating Table 4----------------
var_label(data.tables)<-list(compcog="BSID cognitive score")
t4<-data.tables %>% filter(bsid_yn==1 | neuroyn==1) %>%
  select(early_late,compcog,complang,compmoto,cpyn,gmfl) %>% 
  mutate(compcog=as.numeric(compcog)) %>%
  tbl_summary(by=early_late,type=compcog~"continuous",
              label = compcog~"BSID cognitive score",
              
              missing_text="Missing") %>% 
  modify_caption(caption = "Table 4. Neurological outcomes") %>%
  modify_header(label ~ "Outcome") %>% as_flex_table()

data.tables %>% filter(bsid_yn==1 | neuroyn==1) %>%
  select(early_late,compcog)%>%
  tbl_summary(by=early_late,type=compcog~"continuous",
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits =compcog~ 1)

# Creating Table 5----------------
t5<-table5data %>% select(early_late,early_after_dc,late_but_early,reason_late_but_early___1,
              reason_late_but_early___2,
              reason_late_but_early___3,
              reason_late_but_early___4,
              reason_late_but_early___5,
              age_repair_days,
              pma_repair,wt_repair,
              surg_time,gen_anesthesia,reg_anesthesia,left_or_intubated,
              postop_dx) %>% tbl_summary(by=early_late,
                                         missing_text="Missing") %>%
  modify_caption(caption = "Table 5. Characteristics of subjects at time of IH repair; only includes infants who had a repair.") %>% as_flex_table()
            



t5.1<-d2 %>% select(!c(study_id,ihrep_readmit)) %>%
  tbl_summary(by=early_late,value = everything()~"1",
              missing_text="Missing") %>%
  modify_header(label ~ "") %>%
  modify_caption("Table 5.1 Adverse events during IH repair. Only includes infants who had a repair.") %>% as_flex_table()


t2<-data.tables %>% select(early_late,sae,sae_pre,sae_ihrep,sae_post,any_ae) %>%
  tbl_summary(by=early_late,
              label=list(sae="Infants with one or more SAEs during the study period",
                                       sae_pre="Infants with one or more SAEs before the IH repair",
                         sae_ihrep="Infants with one or more SAEs during IH repair",
                                       sae_post="Infants with one or more SAEs after IH repair",
                         any_ae="Infants with one or more AE during study period"),
              value=list("sae"~"1",
    "sae_pre"~"1","sae_ihrep"~"1","sae_post"~"1","any_ae"~"1"))  %>% 
  modify_header(label = "Outcome") %>% modify_caption(caption="Table 2. Infants with Serious Adverse Events")
t2<-t2 %>% as_flex_table()

t2_1<-data.tables %>% filter(sae==1) %>% 
  select(
    early_late,
    death,
        `Cardiac Arrest`,
    CPR,
    apnea,
    bradycardia,
    reganesthtox,
    stridor,
    pneum,
    `Prolonged Intubation`,
    `Unplanned Reintub`,
    IHIncarc,
    `IH Recurr`,
    `IH Reop`,
    `Injury Adjacent Structure`,
    `Wound Disrupt`,
    SSI,   
    `Other AE`
  ) %>%
  tbl_summary(by=early_late,
              label = list(death = "Death",
                           apnea="Apnea",bradycardia="Bradycardia",
                           stridor="Stridor",
                           pneum="Pneumonia",
                           IHIncarc="IH incarceration",`Other AE`="Other SAE",
                           `Wound Disrupt`="Wound disruption",
                           `IH Recurr`="IH recurrence",`IH Reop`="IH reoperation",
                           `Unplanned Reintub`="Unplanned reintubation",
                           `Prolonged Intubation`="Prolonged intubation",
                           reganesthtox="Regional anesthetic toxicity",
                           `Injury Adjacent Structure`="Injury to adjacent structure",
                           SSI="Surgical site infection"),
              value=list("death" ~ "1","apnea"~"1",
                         "bradycardia"~"1",
                         `Cardiac Arrest`~"1",
                         "CPR"~"1",
                         "pneum"~"1",
                         "stridor"~"1",
                         "IHIncarc"~"1",
                         `Prolonged Intubation`~"1",
                         `Other AE`~"1",
                         "SSI"~"1",
                         `Wound Disrupt`~"1",
                         `IH Recurr`~"1",
                         `IH Reop`~"1",
                         `Unplanned Reintub`~"1",
                         `Injury Adjacent Structure`~"1",
                         "reganesthtox"~"1")) %>%
  modify_header(label = "SAE") %>% 
  modify_caption(caption ="SAE specifics - this table only includes infants with at least one SAE")

t2_1<-t2_1 %>% as_flex_table()

save_as_docx(t1,t2,t2_1,t3,t3.1,t4,t5,t5.1,
             path=paste0("./Results/HIPTables_",Sys.Date(),".docx"))

save_as_docx(t3.1,
             path=paste0("./Results/HIP_LOSoutcomes_",Sys.Date(),".docx"))

# Primary outcome analysis -----------------
df1<-data.tables[order(data.tables$site),]
df1$sae<-as.numeric(df1$sae)-1
df1$ga<-as.numeric(df1$ega_birth_weeks+df1$ega_birth_days/7)

write.csv()
library(lmerTest)
library(geepack)
library(brms)

l2<-glm(sae~early_late+gest_age_category,family=binomial,
          data=df1)
summary(l2)
#
library(splines)
l1<-glmer(sae~early_late+gest_age_category+(1|site),family=binomial,
          data=df1)
summary(l1)

sjPlot::plot_model(l1,type="pred",terms=c("gest_age_category","early_late"))
sjPlot::plot_model(l1,type="pred",terms=c("ga [all]","early_late"))
res.freq<-matrix(NA,nrow=2,ncol=4)

l1<-geese(sae~early_late+gest_age_category,id=site,family=binomial(link="log"),
          corstr = "exchangeable",
          data=df1)
summary(l1)  
cc<-summary(l1)$mean

res.freq[1,]<-c(cc["early_lateLate Repair",1]+
                  c(0,-1,1)*1.96*cc["early_lateLate Repair",2],
                cc["early_lateLate Repair",4])
res.freq[1,1:3]<-exp(res.freq[1,1:3])

l2<-geese(sae~early_late+gest_age_category,id=site,family=binomial(link="identity"),
          corstr = "exchangeable",
          data=df1)
summary(l2)  
cc<-summary(l2)$mean

res.freq[2,]<-c(cc["early_lateLate Repair",1]+
                  c(0,-1,1)*1.96*cc["early_lateLate Repair",2],
                cc["early_lateLate Repair",4])
round(res.freq,2)  
# [,1]  [,2]  [,3] [,4]
# [1,]  0.67  0.47  0.95 0.02
# [2,] -0.08 -0.16 -0.01 0.02

# SAEs --
results<-matrix(NA,nrow=4,ncol=4)

get_prior(sae~early_late+gest_age_category+(1|site),
          family=bernoulli,data=df1)
m3<-brm(sae~early_late+gest_age_category+(1|site),
        family=bernoulli,
        prior = c(set_prior("normal(0,10)", class = "Intercept"),
                  set_prior("normal(0,1)", class = "b"),
                  set_prior("normal(0,.7)", class = "b",
                            coef="early_lateLateRepair"),
                  set_prior("normal(0,1)",class="sd")),
        cores = 3,control = list(adapt_delta = 0.99),
        chains = 3, warmup=2000,iter = 10000,data=df1)

#using vague priors
m3<-brm(sae~early_late+gest_age_category+(1|site),
        family=bernoulli,
        cores = 3,control = list(adapt_delta = 0.99),
        chains = 3, warmup=2000,iter = 10000,data=df1)


summary(m3,digits=4)
beta3<-as.matrix(m3,variable="b_early_lateLateRepair")
# 
nd<-df1
nd$early_late<-"Early Repair"
p0<-posterior_epred(m3,nd)
nd$early_late<-"Late Repair"
p1<-posterior_epred(m3,nd)
RR<-p1/p0

RD<-p1-p0

# # estimating posterior median and 95% credible interval for RR
results[3,]<-c(quantile(RR,c(.5,.025,.975)),mean(RR<1))

# # estimating posterior median and 95% credible interval for RD
results[4,]<-c(quantile(RD,c(.5,.025,.975)),mean(RD<0))

# # estimating posterior median and 95% credible interval for OR
# results[3,]<-c(quantile(exp(beta3),c(.5,.025,.975)),mean(beta3>0))
results[2,]<-c(quantile(exp(beta3),c(.5,.025,.975)),
mean(beta3<0))

res.data<-data.frame(results[-1,])
res.data$metric<-c("OR","RR","RD")
names(res.data)[1:4]<-c("median","95% L","95% U","Prob(RR<1)")
write.csv(res.data,"./Results/BayesianPrimOut.csv",row.names = F)

# l2<-glm.nb(total_los_end~group+age,data=d.sub)
# summary(l2)  
# l3<-glm(total_los_end~group+age,family=poisson,data=d.sub)
# summary(l3)  
# 
f1<-glm(total_los~early_late+gest_age_category,
        family=Gamma(link="log"), data=df1[df1$nicu_los>=0,])
summary(f1)

f2<-rq(total_los~early_late+gest_age_category,
        data=df1[df1$nicu_los>=0,])
summary(f2)

d.sub<-df1[df1$nicu_los>=0 & !is.na(df1$nicu_los),]
ggplot(d.sub,aes(x=nicu_los,y=total_los))+geom_point()+facet_grid(~early_late,drop=T)

