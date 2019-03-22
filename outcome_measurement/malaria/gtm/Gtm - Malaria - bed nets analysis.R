# --------------------------
# Author: Guillermo Ambrosio
# Date:  2019-03-05
#
# Preliminary analysis of bednets impact
# Also some analysis of other interventions such as breeding sites treatment
# --------------------------
library(data.table)
library(lme4)

codePath = "PCE/gf/"

# Requirements:
source(paste0(codePath, "core/GT_load_data.R"), encoding = "UTF-8")
source(paste0(codePath, "core/GT_helper_functions.R"), encoding = "UTF-8")

datmalaria = read.csv("PCE/Outcome Measurement Data/MALARIA/Gtm Malaria impact analysis data.csv",
                      row.names = 1)
breeds = read.csv("PCE/Outcome Measurement Data/MALARIA/SIGSA 6m - Malaria production by depto - 2014-2017.csv", row.names = 1)


# These departments should be included in the analysis given their amount of notifications
# Other departments have very low counts.
deptosGood = c(5, 6, 10,11,13, 14,16,17,18)

#datmalaria$Year_n = scale(datmalaria$Year) # (datmalaria$Year - mean(datmalaria$Year)) / var(datmalaria$Year)
#datmalaria$Notifs = datmalaria$Notifs-1
#model.gee = gee(Notifs ~ 1+ Year_n + factor(Semester) + 
#                (notifsLagSem_1 + notifsLagSem_2  ) + 
#                (bednetsLagSem_1 + bednetsLagSem_2 + bednetsLagSem_3 + bednetsLagSem_4), 
#            data = datmalaria[datmalaria$semindex > 10 , ], 
#            id= deptocode, family=poisson,corstr = "unstructured")
#summary(model.gee)

datmalaria$notifsLagYear_1 = datmalaria$notifsLagSem_1 + datmalaria$notifsLagSem_2
datmalaria$notifsLagYear_1_n = log(datmalaria$notifsLagYear_1+1)

datmalaria$notifsLagSem_1_n = log(datmalaria$notifsLagSem_1 + 1)  
datmalaria$notifsLagSem_2_n = log(datmalaria$notifsLagSem_2 + 1)  
datmalaria$notifsLagSem_3_n = log(datmalaria$notifsLagSem_3 + 1)  
datmalaria$notifsLagSem_4_n = log(datmalaria$notifsLagSem_4 + 1)  
datmalaria$bednetsLagSem_1_n = log(datmalaria$bednetsLagSem_1 + 1)  
datmalaria$bednetsLagSem_2_n = log(datmalaria$bednetsLagSem_2 + 1)  
datmalaria$bednetsLagSem_3_n = log(datmalaria$bednetsLagSem_3 + 1)  
datmalaria$bednetsLagSem_4_n = log(datmalaria$bednetsLagSem_4 + 1)  
datmalaria$Year_n = datmalaria$Year - 2015
datmalaria$notifsLagYear_1_n = datmalaria$notifsLagYear_1/1000
summary(datmalaria$notifsLagYear_1_n)
datmalaria$bednetsLagSem_1_n = datmalaria$bednetsLagSem_1/1000
summary(datmalaria$bednetsLagSem_1_n)
datmalaria$semindex_g = factor(datmalaria$semindex)

datmalaria$notifsLagSem_1_n_log10 = log10(datmalaria$notifsLagSem_1+1)
datmalaria$notifsLagSem_2_n_log10 = log10(datmalaria$notifsLagSem_2+1)
datmalaria$bednetsLagSem_1_n_log10 = log10(datmalaria$bednetsLagSem_1 + 1)  
datmalaria$bednetsLagSem_2_n_log10 = log10(datmalaria$bednetsLagSem_2 + 1)  

# Cum BN
datmalaria$cumBN_n = datmalaria$cumBN/10000  #  log10(datmalaria$cumBN+1)
datmalaria$cumBNLagSem_1_n =  datmalaria$cumBNLagSem_1/10000 # log10(datmalaria$cumBNLagSem_1+1)
datmalaria$cumBNLagSem_2_n =  datmalaria$cumBNLagSem_2/10000  # log10(datmalaria$cumBNLagSem_2+1)

datmalaria$cumBN_l10n = log10(datmalaria$cumBN+1)
datmalaria$cumBNLagSem_1_l10n = log10(datmalaria$cumBNLagSem_1+1)
datmalaria$cumBNLagSem_2_l10n = log10(datmalaria$cumBNLagSem_2+1)


model1 = glmer(Notifs ~ 1 + factor(Year) + factor(Semester) + 
                   bednetsLagSem_1_n+notifsLagYear_1_n + 
                   (1+bednetsLagSem_1_n+notifsLagYear_1_n|deptocode) # + (1|deptocode:semindex_g)
               , 
               data = datmalaria[(datmalaria$semindex %in% c(9,10,11,12)), ],
               family=poisson)
summary(model1)

#---------------- IHME ----------------
# I tried the previous model but it couldnt converge and recommended to rescale covariates.
model = glmer.nb(Notifs ~ 1 + factor(Year) + factor(Semester) + 
                     bednetsLagSem_1_n+notifsLagYear_1_n + 
                     (1+bednetsLagSem_1_n+notifsLagYear_1_n|deptocode) # + (1|deptocode:semindex_g)
                 , 
                 data = datmalaria[(datmalaria$semindex %in% c(9,10,11,12)), ])
summary(model)

# Transformed covs with log10 and removed lagged random fx. 
model.nb = glmer.nb(Notifs ~ 1  + factor(Year) + 
                     bednetsLagSem_1_n_log10 + 
                     (1 + factor(Year) + 
                          bednetsLagSem_1_n_log10 |deptocode) # + (1|deptocode:semindex_g)
                 , 
                 data = datmalaria[(datmalaria$semindex %in% c(9,10,11,12)), ])
summary(model.nb)
warnings()
#datmalaria[datmalaria$deptocode == 5, ]



model2 = glmer(Notifs ~ 1 + factor(Semester) + notifsLagYear_1_n +
                   cumBN_n +
                   (1 + notifsLagYear_1_n +  cumBN_n | deptocode)
               ,
               data = datmalaria[(datmalaria$deptocode %in% deptosGood) & 
                                 (datmalaria$semindex %in% c(7,8,9,10,11,12,13)), ],
               family=poisson,
               control=glmerControl(optimizer="bobyqa"))
summary(model2)

model2.1 = glmer(Notifs ~ 1 + factor(Semester) + notifsLagYear_1_n +
                   cumBN_n +
                   (1 + notifsLagYear_1_n | deptocode)
               ,
               data = datmalaria[(datmalaria$deptocode %in% deptosGood) & 
                                     (datmalaria$semindex %in% c(7,8,9,10,11,12,13)), ],
               family=poisson,
               control=glmerControl(optimizer="bobyqa"))
summary(model2.1)

model2.5 = glmer(Notifs ~ 1 + factor(Semester) + notifsLagYear_1_n +
                   bednetsLagSem_1_n_log10 + cumBN_n +
                   (1 + notifsLagYear_1_n + bednetsLagSem_1_n_log10 + cumBN_n | deptocode)
               ,
               data = datmalaria[(datmalaria$deptocode %in% deptosGood) & 
                                     (datmalaria$semindex %in% c(7,8,9,10,11,12,13)), ],
               family=poisson,
               control=glmerControl(optimizer="bobyqa"))
summary(model2.5)

model3 = glmer(Notifs ~ 1 + factor(Semester) + notifsLagYear_1_n +
                cumBNLagSem_1_n +
                (1 + notifsLagYear_1_n + cumBNLagSem_1_n | deptocode)
               ,
               data = datmalaria[(datmalaria$deptocode %in% deptosGood) & 
                                 (datmalaria$semindex %in% c(7,8,9,10,11,12,13)), ],
               family=poisson,
               control=glmerControl(optimizer="bobyqa"))
summary(model3)

# Random intercept and autoregressive term, fixed autoregressive and 
# fixed cumm bn term. No year categorical fixed effect since the tides 
# should be explained with autoreg term
model3.5 = glmer(Notifs ~ 1 + factor(Semester) + notifsLagYear_1_n +
                   cumBNLagSem_1_n +
                   (1 + notifsLagYear_1_n | deptocode)
               ,
               data = datmalaria[(datmalaria$deptocode %in% deptosGood) & 
                                     (datmalaria$semindex %in% c(7,8,9,10,11,12,13)), ],
               family=poisson,
               control=glmerControl(optimizer="bobyqa"))
summary(model3.5)
model3.6 = glmer(Notifs ~ 1 + factor(Year) + factor(Semester) + notifsLagYear_1_n +
                     cumBNLagSem_1_n +
                     (1 + notifsLagYear_1_n | deptocode)
                 ,
                 data = datmalaria[(datmalaria$deptocode %in% deptosGood) & 
                                       (datmalaria$semindex %in% c(7,8,9,10,11,12,13)), ],
                 family=poisson,
                 control=glmerControl(optimizer="bobyqa"))
summary(model3.6)

1-exp(-0.09899)
# compare with non cum intervention
model4 = glmer(Notifs ~ 1 + factor(Semester) + notifsLagYear_1_n +
                   bednetsLagSem_1_n_log10 + 
                   (1 + notifsLagYear_1_n +  bednetsLagSem_1_n_log10 | deptocode)
               ,
               data = datmalaria[(datmalaria$deptocode %in% deptosGood) & 
                                 (datmalaria$semindex %in% c(7,8,9,10,11,12,13)), ],
               family=poisson,
               control=glmerControl(optimizer="bobyqa"))
summary(model4)

model4.1 = glmer(Notifs ~ 1 + factor(Semester) + notifsLagYear_1_n +
                     bednetsLagSem_1_n_log10 + 
                   (1 + notifsLagYear_1_n | deptocode)
               ,
               data = datmalaria[(datmalaria$deptocode %in% deptosGood) & 
                                     (datmalaria$semindex %in% c(7,8,9,10,11,12,13)), ],
               family=poisson,
               control=glmerControl(optimizer="bobyqa"))
summary(model4.1)

model4.2 = glmer(Notifs ~ 1 + factor(Year) + factor(Semester) + notifsLagYear_1_n +
                     bednetsLagSem_1_n_log10 + 
                     (1 + notifsLagYear_1_n | deptocode)
                 ,
                 data = datmalaria[(datmalaria$deptocode %in% deptosGood) & 
                                       (datmalaria$semindex %in% c(7,8,9,10,11,12,13)), ],
                 family=poisson,
                 control=glmerControl(optimizer="bobyqa"))
summary(model4.2)

# with bednets, cumDose and autoreg term
model5 = glmer(Notifs ~ 1 + factor(Semester) + notifsLagYear_1_n +
                   bednetsLagSem_1_n_log10 + cumBNLagSem_1_n + 
                   (1 + notifsLagYear_1_n +  bednetsLagSem_1_n_log10 + cumBNLagSem_1_n | deptocode)
               ,
               data = datmalaria[(datmalaria$deptocode %in% deptosGood) & 
                                     (datmalaria$semindex %in% c(7,8,9,10,11,12,13)), ],
               family=poisson,
               control=glmerControl(optimizer="bobyqa"))
summary(model5)

model6 = glmer(Notifs ~ 1 + factor(Semester) + notifsLagYear_1_n +
                   bednetsLagSem_1_n_log10 + cumBNLagSem_1_n + 
                   (1 + notifsLagYear_1_n | deptocode)
               ,
               data = datmalaria[(datmalaria$deptocode %in% deptosGood) & 
                                     (datmalaria$semindex %in% c(7,8,9,10,11,12,13)), ],
               family=poisson,
               control=glmerControl(optimizer="bobyqa"))
summary(model6)

# Estimating an intervention effect for each department
model7 = glmer(Notifs ~ 1 + factor(Semester) + notifsLagYear_1_n +
                   cumBNLagSem_1_n:factor(deptocode) + 
                   (1 + notifsLagYear_1_n | deptocode)
               ,
               data = datmalaria[(datmalaria$deptocode %in% deptosGood) & 
                                     (datmalaria$semindex %in% c(7,8,9,10,11,12,13)), ],
               family=poisson,
               control=glmerControl(optimizer="bobyqa"))
summary(model7)


library(nlme)
# Check residuals vs predictor variables
plot(residuals(model2) ~ datmalaria[(datmalaria$deptocode %in% deptosGood) & 
                                        (datmalaria$semindex %in% c(7,8,9,10,11,12,13)), "cumBNLagSem_2_n"])


