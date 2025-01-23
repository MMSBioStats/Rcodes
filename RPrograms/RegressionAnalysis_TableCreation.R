library(gtsummary)
library(labelled)
library(flextable)

# using dataset 'd2'
m.uni<-d2 |>
  tbl_uvregression(
    method = polr,
    y = gosee_dis,
    include =c(posmolality_base_sd,totalprotein_base_sd,stm_base_sd,syndecan_base_sd,hyaluronic_base_sd,heparan_base_sd,tpa_base_sd,albumin_base_sd),
    exponentiate = TRUE
    # pvalue_fun = label_style_pvalue(digits = 2)
  )%>%modify_header(label="**Predictor**")%>%as_flex_table()%>%set_caption(caption="Table 1. Univariate ordinal regression model of GOS-E at discharge including baseline (0-12 hr) predictors standardized (mean=0, SD=1). ORs for higher GOS-E categories",align_with_table = F)%>%
  flextable::padding(padding.top = 2, part = "all") %>%
  flextable::padding(padding.bottom = 2, part = "all")
m.uni


m.mul.96<-polr(gosee_dis~ posmolality_96_sd +stm_96_sd+ hyaluronic_96_sd +albumin_96_sd,
               data=d2)
summary(m.mul.96)
t.96.mul<-m.mul.96%>%tbl_regression(exponentiate = T)%>%add_n()%>%modify_header(label="**Predictor**")%>%as_flex_table()%>%set_caption(caption="Table 4. Multiple ordinal regression model of GOS-E at discharge including 96 hr predictors standardized (mean=0, SD=1). ORs for higher GOS-E categories",align_with_table = F)%>%
  flextable::padding(padding.top = 2, part = "all") %>%
  flextable::padding(padding.bottom = 2, part = "all")
t.96.mul

save_as_docx(m.uni,t.96.mul,path=paste0("../Results/LabPredictors_GOSE_",Sys.Date(),".docx"))

