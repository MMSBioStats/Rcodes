# ETM-Devices; PI: Mosquera
# CREATING ALLOCATION TABLES
# STRATIFIED BY age (<=2; >2), baseline risk (1=MV, 2=median, 3=low)
# TOTAL SAMPLE SIZE=~300
# randomizing 1:1 to control:intervention
# Updated May 31, 2022

install.packages("remotes")
remotes::install_github("nutterb/redcapAPI")

library(redcapAPI)
rcon <- redcapConnection(url="https://redcap.uth.tmc.edu/api/", 
                         token="9C36072DAFE3D2E617F46088842A0AA3")

Randomize <- allocationTable(rcon, random="group", 
                             strata=c("age_group", "baseline_risk"), 
                             replicates=108, 
                             # block.size = 6,
                             block.size = c(4),
                             # block.size.shift = c(0,.5),
                             weights=c(1,1),
                             seed.dev=5999, seed.prod=87535)
table(Randomize$dev_allocation$group,Randomize$dev_allocation$baseline_risk)
table(Randomize$dev_allocation$group,Randomize$dev_allocation$age_group)

table(Randomize$prod_allocation$group,Randomize$prod_allocation$baseline_risk)
table(Randomize$prod_allocation$group,Randomize$prod_allocation$age_group)

# Randomize <- allocationTable_offline(meta_data="G:/My Drive/ObGyn/Fishel/CGM/CGMScreeningOBGYN_DataDictionary_2022-02-28.csv", random="randomization1", 
#                              strata=c("bmi_rand", "clinic_site"), 
#                              replicates=150, 
#                              block.size=c(8, 4, 2), 
#                              block.size.shift = c(0, .48, .9),
#                              seed.dev=7234, seed.prod=747)

write.csv(Randomize$dev_allocation, file="G:/My Drive/Avritscher/EnhancedTM/AllocationDevelopment.csv",row.names = F)
write.csv(Randomize$prod_allocation, file="G:/My Drive/Avritscher/EnhancedTM/AllocationProduction.csv",row.names = F)
