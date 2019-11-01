# Recoding relevant variables to prep them for analysis

# Getting data created in 'import_data.R'
load("data/EWCS.Rda")

#Subsetting only EU
EWCS_orig <-  subset(EWCS, EU_x==1)

# Year of the survey
EWCS <- subset(EWCS_orig, select=wave)
EWCS$wave <- ordered(EWCS_orig$wave)

# Job quality indexes
EWCS$physrsk <- EWCS_orig$envsec_slim
EWCS$intensity <- EWCS_orig$intens_slim
EWCS$skilsdis <- EWCS_orig$wq_slim
EWCS$timequality <- EWCS_orig$wlb_slim

q89b <- (5-as.numeric(EWCS_orig$y15_Q89b))/4
q89g <- (as.numeric(EWCS_orig$y15_Q89g)-1)/4
EWCS$prospects <- rowMeans(cbind(q89b,q89g), na.rm=TRUE) * 100

# Covariates
EWCS$country <- droplevels(EWCS_orig$countid)
EWCS$nace <- EWCS_orig$y15_nace_r1_lt_11
EWCS$isco <- EWCS_orig$y15_ISCO_88_1
EWCS$sex <- EWCS_orig$y15_Q2a
EWCS$age <- as.numeric(levels(EWCS_orig$y15_Q2b))[EWCS_orig$y15_Q2b]
EWCS$educ <- ordered(EWCS_orig$education)
EWCS$wp_size <- ordered(EWCS_orig$wp_size)
EWCS$emp_stat <- EWCS_orig$emp_stat_lt

levels(EWCS$isco) <- c("Armed forces","Managers","Professionals","Technicians","Clerks","Service and sales",
                      "Agricultural workers","Craft workers","Plant and machine operators","Elementary occupations")




EWCS$nace8[EWCS$nace=="A-B Agriculture, hunting, forestry, fishing"] <- "Agriculture"
EWCS$nace8[EWCS$nace=="C-D Mining, quarrying, Manufacturing"] <- "Industry"
EWCS$nace8[EWCS$nace=="E Electricity, gas, and water supply"] <- "Industry"
EWCS$nace8[EWCS$nace=="F Construction"] <- "Construction"
EWCS$nace8[EWCS$nace=="G Wholesale and retail trade; repair of motor vehicles and motorcycles"] <- "Commerce and hospitality"
EWCS$nace8[EWCS$nace=="H Hotels and restaurants"] <- "Commerce and hospitality"
EWCS$nace8[EWCS$nace=="I Transport, storage and communication"] <- "Transport"
EWCS$nace8[EWCS$nace=="J Financial intermediation"] <- "Financial services"
EWCS$nace8[EWCS$nace=="K Real estate activities"] <- "Financial services"
EWCS$nace8[EWCS$nace=="L Public administration and defence; compulsory social security" ] <- "Public administration"
EWCS$nace8[EWCS$nace=="M-N-O-P-Q Other services"  ] <- "Other services"

EWCS$nace8 <- as.factor(EWCS$nace8)


# Weight
EWCS$w_time <- EWCS_orig$w_time
EWCS$w4 <- EWCS_orig$w4

# Overview of what is available throughout the waves

df <- as.data.frame(table(EWCS$wave))
df <- df[-2]
colnames(df) <- 'wave'

for (y in colnames(EWCS)[-1]) {
  
  formula <- as.formula(paste0(y, " ~ wave"))
  agg <- aggregate(formula, data=EWCS, function(x) {sum(!is.na(x))})
  df <- merge(df,agg, all=TRUE)
  
}

# This shows that all variables are available from 2000, except education and prospects from 2005. 
# All are also availabe from 1995, except education, prospects, workplace size and employment status.
df

save(EWCS,file="data/EWCS_recoded.Rda")

haven::write_sav(EWCS, "data/ewcs_recoded.sav")

haven::write_sav(
  subset(EWCS, wave!="1995 - 2nd EWCS" & wave!="2000-2001 - 3rd EWCS"), "data/ewcs_recoded_3waves.sav")
