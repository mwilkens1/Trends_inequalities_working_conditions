library(foreign)

#Importing data
EWCS <- read.spss("R:/SpssStata/Mathijn/EWCS/Data/Step 2 - after_recodes_incl_JQI_2703.sav", to.data.frame=T)

save(EWCS,file="data/EWCS.Rda")