# Example of creating randomization tables for RedCAP
# CREATING ALLOCATION TABLES
# STRATIFIED BY BMI (<40; >=40), SITE (1=UTP, 2=BELLAIRE, 3=MEMORIAL, 4=GH)
# TOTAL SAMPLE SIZE=~594

# if project has API set up you can use lines 7-17
library(redcapAPI)
# rcon <- redcapConnection(url="https://redcap.uth.tmc.edu/api/", 
#                          token="")
# 
# Randomize <- allocationTable(rcon, random="randomization1", 
#                              strata=c("bmi_rand", "clinic_site"), 
#                              replicates=150, 
#                              block.size=c(6, 4, 2), 
#                              block.size.shift = c(0, .48, .9),
#                              seed.dev=7234, seed.prod=747)

# If not API then download datadictionary from RedCAP

Randomize <- allocationTable_offline(meta_data="../DataDictionary_2022-02-28.csv", random="randomization1",
                             strata=c("bmi_rand", "clinic_site"),
                             replicates=150,
                             block.size=c(6, 4, 2),
                             block.size.shift = c(0, .48, .9),
                             seed.dev=7234, seed.prod=747)

write.csv(Randomize$dev_allocation, file="../AllocationDevelopment.csv",row.names = F)
write.csv(Randomize$prod_allocation, file="../AllocationProduction.csv",row.names = F)
