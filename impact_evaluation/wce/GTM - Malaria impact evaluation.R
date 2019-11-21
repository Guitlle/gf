# --------------------------
# Author: Guillermo Ambrosio
# Date:  2019-03-22
#
# Preliminary analysis of bednets impact
# Also some analysis of other interventions such as breeding sites treatment
# --------------------------

library(data.table)
library(lme4)
library(ggplot2)
# Instead of hard coding this path, let's set it up as working dir before running the code.
codePath = "PCE/gf/"

# Requirements:
source(paste0(codePath, "core/GT_load_data.R"), encoding = "UTF-8")
source(paste0(codePath, "core/GT_helper_functions.R"), encoding = "UTF-8")


datmalaria = read.csv("PCE/Outcome Measurement Data/MALARIA/Gtm Malaria impact analysis data.csv",
                      row.names = 1)
breeds = read.csv("PCE/Outcome Measurement Data/MALARIA/SIGSA 6m - Malaria production by depto - 2014-2017.csv", row.names = 1)


datmalaria$notifsLagYear_1 = datmalaria$notifsLagSem_1 + datmalaria$notifsLagSem_2
datmalaria$notifsLagYear_1_n = log10(datmalaria$notifsLagYear_1+1)

# Cum BN
datmalaria$cumBN_n = datmalaria$cumBN/10000  #  log10(datmalaria$cumBN+1)
datmalaria$cumBNLagSem_1_n =  datmalaria$cumBNLagSem_1/10000 # log10(datmalaria$cumBNLagSem_1+1)
datmalaria$cumBNLagSem_2_n =  datmalaria$cumBNLagSem_2/10000  # log10(datmalaria$cumBNLagSem_2+1)

datmalaria$cumBN_l10n = log10(datmalaria$cumBN+1)
datmalaria$cumBNLagSem_1_l10n = log10(datmalaria$cumBNLagSem_1+1)
datmalaria$cumBNLagSem_2_l10n = log10(datmalaria$cumBNLagSem_2+1)

# These departments should be included in the analysis given their amount of notifications
# Other departments have very low counts.
deptosGood = c(5, 6, 10,11,13, 14,16,17,18, 15, 7)
# ------- With other covariates --------------
# Lag by 1 year
breeds$YearB = breeds$Year
breeds$Year = NULL
breeds$YearLag = breeds$YearB+1
breeds$txs_rociamiento = log10(1 + breeds$NRocdom)
breeds$txs_criads = log10(1 + breeds$NTxAplicados)
breeds$txs_crdcl = log10(1 + breeds$No..de.tratamientos.de.cura.radical)
breeds$txs_criads2 = log10(1 + breeds$txs_crdcl + breeds$NTxAplicados)
breeds$diag_gg = log10(1 + breeds$NGG)
breeds$criaderos = log10(breeds$NTemp + breeds$NPerm + 1)

breeds = data.table(breeds)

datmalaria2 = merge(datmalaria, breeds, by.x = c("deptocode", "Year"), 
                     by.y = c("Deptocode", "YearLag"))

table(datmalaria2$Year, datmalaria2$semindex)

datmalaria2 = data.table(datmalaria2)
# mean notifications by department
# Excluding departments with very low counts to avoid large influence in radical multiplicative change
datmalaria2[, mean(Notifs) ,by=.(deptocode)][order(V1)]
# Malaria testing by year:
dcast(breeds[, sum(NGG) ,by=.(YearB, Deptocode)], Deptocode ~ YearB)
# Notifs by year and depto
dcast(datmalaria2[, sum(Notifs) ,by=.(Year, deptocode)], deptocode ~ Year)
datmalaria2[, sum(Notifs) ,by=.(Year)]
# Cum Bednets distr
dcast(datmalaria2[, sum(cumBNLagSem_1) ,by=.(Year, deptocode)], deptocode ~ Year)
# Bednets distr
dcast(datmalaria2[, sum(bednetsLagSem_2) ,by=.(Year, deptocode)], deptocode ~ Year)
datmalaria2[, sum(bednetsLagSem_2) ,by=.(deptocode)]


model1 = glmer(Notifs ~ 
                   1 + notifsLagYear_1_n +
                   cumBNLagSem_1_l10n:factor(deptocode) + 
                   txs_rociamiento + 
                   txs_criads2 + 
                   (1 + factor(Semester) + notifsLagYear_1_n  + 
                        txs_rociamiento + 
                        txs_criads2 | deptocode)
               ,
               data = datmalaria2[(datmalaria2$deptocode %in% deptosGood) & 
                                      (datmalaria2$semindex %in% c(7,8,9,10,11,12,13,14)), ],
               family=poisson,
               control=glmerControl(optimizer= "bobyqa",
                                    optCtrl  = list(maxfun=2e5)
               ))
summary(model1)
ranef(model1)
AIC(model1)

model2 = glmer(Notifs ~ 
                   1 +
                   cumBNLagSem_1_n:factor(deptocode) + 
                   txs_rociamiento + 
                   txs_criads2 + 
                   (1 + factor(Semester) + notifsLagYear_1_n  + 
                        txs_rociamiento + 
                        txs_criads2 | deptocode)
               ,
               data = datmalaria2[(datmalaria2$deptocode %in% deptosGood) & 
                                      (datmalaria2$semindex %in% c(7,8,9,10,11,12,13,14)), ],
               family=poisson,
               control=glmerControl(optimizer= "bobyqa",
                                    optCtrl  = list(maxfun=2e5)
               ))
summary(model2)
ranef(model2)

model3 = glmer(Notifs ~ 
                   1 + factor(Semester) + notifsLagYear_1_n +
                   cumBNLagSem_1_n:factor(deptocode) + 
                   txs_rociamiento + 
                   txs_criads2 + 
                   (1 + factor(Semester)  + notifsLagYear_1_n  + 
                        txs_rociamiento + 
                        txs_criads2 | deptocode)
               ,
               data = datmalaria2[(datmalaria2$deptocode %in% deptosGood) & 
                                      (datmalaria2$semindex %in% c(7,8,9,10,11,12,13,14)), ],
               family=poisson,
               control=glmerControl(optimizer= "bobyqa",
                                    optCtrl  = list(maxfun=2e5)
               ))
summary(model3)
AIC(model3) # 1079
# Good alternative, but complains about singularity
ranef(model3)

model3b = glmer(Notifs ~ 
                   1 + factor(Semester) + notifsLagYear_1_n +
                    cumBNLagSem_1_n +
                    txs_rociamiento + 
                    txs_criads2 + 
                   (1 + factor(Semester)  + notifsLagYear_1_n  +
                        cumBNLagSem_1_n +
                        txs_rociamiento + 
                        txs_criads2 | deptocode)
               ,
               data = datmalaria2[(datmalaria2$deptocode %in% deptosGood) & 
                                      (datmalaria2$semindex %in% c(7,8,9,10,11,12,13,14)), ],
               family=poisson,
               control=glmerControl(optimizer= "bobyqa",
                                    optCtrl  = list(maxfun=2e5)
               ))
summary(model3b)
AIC(model3b) # 1100
# Correlation structure shows high relation between txs_criads2 and cumBednets lagged term
ranef(model3b)

model3c = glmer(Notifs ~ 
                    1 + factor(Semester) + notifsLagYear_1_n +
                    cumBNLagSem_1_n +
                    txs_rociamiento + 
                    txs_criads2 + 
                    (1 + factor(Semester)  + notifsLagYear_1_n|deptocode)  +
                    (-1 +
                         cumBNLagSem_1_n +
                         txs_rociamiento + 
                         txs_criads2 | deptocode)
                ,
                data = datmalaria2[(datmalaria2$deptocode %in% deptosGood) & 
                                       (datmalaria2$semindex %in% c(7,8,9,10,11,12,13,14)), ],
                family=poisson,
                control=glmerControl(optimizer= "bobyqa",
                                     optCtrl  = list(maxfun=2e5)
                ))
summary(model3c)
AIC(model3c) # 1124, better AIC, but no department level effect, not singular +1
# 1223 in second run
ranef(model3c)
# Effect is 10% decrease in notifications, in general, pero 1K bednets distributed
1-exp(-0.111)
# Predict what would happen in 2018 if no bednets were delivered:
datmalaria2[(datmalaria2$deptocode %in% deptosGood) & 
                (datmalaria2$semindex %in% c(13,14)), .(Notifs, deptocode)]
counterfactual18 = copy(datmalaria2[(datmalaria2$deptocode %in% deptosGood) & 
                (datmalaria2$semindex %in% c(13,14)), ])
counterfactual18[, Notifs_old := Notifs]
counterfactual18[, cumBNLagSem_1_n := 0.01]
predNotifs = predict(model3c,newdata=counterfactual18, type="link") 

# Attemting to get an estimation of confidence intervals with bootMer
predFun <- function(fit) {
    predict(fit,counterfactual18)
}
predNotifsCI <- bootMer(model3c,nsim=100,FUN=predFun,seed=1, verbose = T)
predsBS = apply(predNotifsCI$t, 2, function (x) { mean(x, na.rm=T) })
std.err <- apply(predNotifsCI$t, 2, function (x) { sd(x, na.rm=T) })
CI.lo <- predsBS - std.err*1.96
CI.hi <- predsBS + std.err*1.96
# Doesnt look good. Many errors are thrown because the model is not very flexible and does
# not converge when bootMer runs it repeated times.

counterfactual18[, Notifs_pred := round(exp(predNotifs), 1)] 
counterfactual18[, Notifs_pred_Boot := round(exp(predsBS), 1)] 
counterfactual18[,.(Notifs_old, Notifs_pred, deptocode)]

model3d = glmer(Notifs ~ 
                   1 + factor(Semester) + notifsLagYear_1_n +
                   cumBNLagSem_1_n:factor(deptocode) + 
                   txs_rociamiento + 
                   txs_criads2 + 
                   (1 + factor(Semester)  + notifsLagYear_1_n | deptocode)  + 
                    (-1 + 
                         txs_rociamiento + 
                         txs_criads2 | deptocode)
               ,
               data = datmalaria2[(datmalaria2$deptocode %in% deptosGood) & 
                                      (datmalaria2$semindex %in% c(7,8,9,10,11,12,13,14)), ],
               family=poisson,
               control=glmerControl(optimizer= "bobyqa",
                                    optCtrl  = list(maxfun=2e5)
               ))
summary(model3d)
AIC(model3d) # 1109 # Not singular :D 
# 1207 in other run
# Low correlations in random effects, lagged notifs effect makes sense (positive),
# only negative effects in bednets
ranef(model3d)
fixef(model3d)
counterfactual18 = copy(datmalaria2[(datmalaria2$deptocode %in% deptosGood) & 
                                    (datmalaria2$semindex %in% c(13,14)), ])
counterfactual18[, Notifs_old := Notifs]
counterfactual18[semindex %in% c(13,14), cumBNLagSem_1_n := 0.01]
predNotifs = predict(model3d,newdata=counterfactual18, type="link") 
counterfactual18[, Notifs_pred := round(exp(predNotifs), 1)] 
counterfactual18[, diff:= Notifs_pred - Notifs_old]

write.csv(counterfactual18[,.(sum(diff)), by=.(deptocode) ], "PCE/Outcome Measurement Data/MALARIA/Gtm Malaria impact eval results 2018.csv")

counterfactual18[, Notifs_old := Notifs]
counterfactual18[semindex %in% c(13,14), cumBNLagSem_1_n := 0.01]
predNotifs = predict(model3d,newdata=counterfactual18, type="response") 
counterfactual18[, Notifs_pred := round(predNotifs, 1)] 
counterfactual18[, diff:= Notifs_pred - Notifs_old]
counterfactual18[,.(round(sum(Notifs_old)), round(sum(Notifs_pred)), sum(diff)), by=.(deptocode) ]

model3e = glmer(Notifs ~ 
                    1 + factor(Semester) + notifsLagYear_1_n +
                    cumBNLagSem_1_n*txs_criads2 + 
                    txs_rociamiento + 
                    (1 + factor(Semester)  + notifsLagYear_1_n | deptocode)  + 
                    (-1 + cumBNLagSem_1_n*txs_criads2 + 
                         txs_rociamiento | deptocode)
                ,
                data = datmalaria2[(datmalaria2$deptocode %in% deptosGood) & 
                                       (datmalaria2$semindex %in% c(7,8,9,10,11,12,13,14)), ],
                family=poisson,
                control=glmerControl(optimizer= "bobyqa",
                                     optCtrl  = list(maxfun=2e5)
                ))
summary(model3e)
AIC(model3e) # 1086, but singular
ranef(model3e)
fixef(model3e)


model3f = glmer(Notifs ~ 
                    1 + factor(Semester) + notifsLagYear_1_n +
                    cumBN_n + cumBNLagSem_1_n + cumBNLagSem_2_n +
                    txs_rociamiento + 
                    txs_criads2 + 
                    (1 + factor(Semester)  + notifsLagYear_1_n|deptocode)  +
                    (-1 + txs_rociamiento + 
                         txs_criads2 | deptocode) +
                    (-1 + cumBN_n + cumBNLagSem_1_n + cumBNLagSem_2_n|deptocode)
                ,
                data = datmalaria2[(datmalaria2$deptocode %in% deptosGood) & 
                                       (datmalaria2$semindex %in% c(7,8,9,10,11,12,13,14)), ],
                family=poisson,
                control=glmerControl(optimizer= "bobyqa",
                                     optCtrl  = list(maxfun=2e5)
                ))
summary(model3f)
# Fixed effects:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        5.85041    1.85112   3.160 0.001575 ** 
# factor(Semester)2 -0.62467    0.18361  -3.402 0.000668 ***
# notifsLagYear_1_n  0.08912    0.10134   0.879 0.379143    
# cumBN_n            0.15866    0.14835   1.069 0.284862    
# cumBNLagSem_1_n   -0.28328    0.12805  -2.212 0.026953 *  
# cumBNLagSem_2_n    0.01290    0.07144   0.181 0.856664    
# txs_rociamiento   -0.50621    0.39759  -1.273 0.202942    
# txs_criads2       -0.05857    0.43130  -0.136 0.891984    

# notifsLagYear_1_n, criaderos and diag_gg are too correlated (>0.9)
# criaderos and txs_criads2 are also very correlated
# diag_gg and txs_criads2 are too correlated
# lagged notifications term is not strongly correlated with txs_criads2,
# so I have decided to keep it and leave diag and criaderos out
model4 = glmer(Notifs ~ 
                   1 + factor(Semester) + notifsLagYear_1_n + 
                   cumBNLagSem_1_l10n:factor(deptocode) + 
                   txs_rociamiento + 
                   txs_criads2 + 
                   (1 + factor(Semester) + notifsLagYear_1_n + 
                        txs_rociamiento + 
                        txs_criads2 | deptocode)
               ,
               data = datmalaria2[(datmalaria2$deptocode %in% deptosGood) & 
                                      (datmalaria2$semindex %in% c(7,8,9,10,11,12,13,14)), ],
               family=poisson,
               control=glmerControl(optimizer= "bobyqa",
                                    optCtrl  = list(maxfun=2e5)
               ))
AIC(model4) # 1211.3
# High correlations for txs_criads2
summary(model4)
ranef(model4)

model4b = glmer(Notifs ~ 
                   1 + factor(Semester) + notifsLagYear_1_n + 
                   cumBNLagSem_1_l10n:factor(deptocode) + 
                   txs_rociamiento + 
                   txs_criads2 + 
                   (1 + notifsLagYear_1_n +  
                        txs_rociamiento + 
                        txs_criads2 | deptocode)
                   + (-1 + cumBNLagSem_1_l10n| deptocode:semindex)
               ,
               data = datmalaria2[(datmalaria2$deptocode %in% deptosGood) & 
                                      (datmalaria2$semindex %in% c(7,8,9,10,11,12,13,14)), ],
               family=poisson,
               control=glmerControl(optimizer= "bobyqa",
                                    optCtrl  = list(maxfun=2e5)
               ))
summary(model4b)
ranef(model4b)
AIC(model4b)
# AIC 774
# Bad model: All random effects show |correlation| = 1 

model5 = glmer(Notifs ~ 
                   1 + factor(Semester) + notifsLagYear_1_n + 
                   cumBNLagSem_1_l10n + 
                   txs_rociamiento + 
                   txs_criads2 + 
                   (1 + factor(Semester) + notifsLagYear_1_n | deptocode) +
                   (cumBNLagSem_1_l10n - 1|Year:deptocode) +
                   (    cumBNLagSem_1_l10n + 
                        txs_rociamiento + 
                        txs_criads2 - 1|deptocode)
               ,
               data = datmalaria2[(datmalaria2$deptocode %in% deptosGood) & 
                                      (datmalaria2$semindex %in% c(7,8,9,10,11,12,13,14)), ],
               family=poisson,
               control=glmerControl(optimizer= "bobyqa",
                                    optCtrl  = list(maxfun=2e5)
               ))
ranef(model5)
summary(model5)
AIC(model5) # 1050
# Convergence problems


model5b = glmer(Notifs ~ 
                    1 + factor(Semester) + notifsLagYear_1_n + 
                    cumBNLagSem_1_l10n:factor(deptocode) + 
                    txs_rociamiento + 
                    txs_criads2 + 
                    (1 + factor(Semester) + notifsLagYear_1_n | deptocode) +
                    (txs_rociamiento + 
                         txs_criads2 + - 1|deptocode)
                ,
                data = datmalaria2[(datmalaria2$deptocode %in% deptosGood) & 
                                       (datmalaria2$semindex %in% c(7,8,9,10,11,12,13,14)), ],
                family=poisson,
                control=glmerControl(optimizer= "bobyqa",
                                     optCtrl  = list(maxfun=2e5)
                ))
vcov(model5b)
summary(model5b)
ranef(model5b)
# AIC 1237
# Testing modeling uncorrelated random treatments effects
# Seems ok, but lower AIC than model3. 

model5c = glmer(Notifs ~ 
                   1 + factor(Semester) + notifsLagYear_1_n + 
                   cumBNLagSem_1_l10n:factor(deptocode) + 
                   txs_rociamiento + 
                   txs_criads2 + 
                   (1 + factor(Semester) + notifsLagYear_1_n +
                        txs_rociamiento + 
                        txs_criads2 | deptocode) +
                   (cumBNLagSem_1_l10n - 1|Year:deptocode)
               ,
               data = datmalaria2[(datmalaria2$deptocode %in% deptosGood) & 
                                      (datmalaria2$semindex %in% c(7,8,9,10,11,12,13,14)), ],
               family=poisson,
               control=glmerControl(optimizer= "bobyqa",
                                    optCtrl  = list(maxfun=2e5)
               ))
summary(model5c)
ranef(model5c)
AIC(model5c) # 1026, better than model3
# Notifs lagged effect is negative, which does not match a priori assumption.
# Very high correlations in alternative txs with lagged effect. Seems wrong.

datmalaria2$is2018 = datmalaria2$Year == 2018
model6 = glmer(Notifs ~ 
                   1 + factor(Semester) + notifsLagYear_1_n + 
                   cumBNLagSem_1_n:factor(deptocode) + 
                   txs_rociamiento + 
                   txs_criads2 + 
                   (1 + factor(Semester) + notifsLagYear_1_n | deptocode) +
                   (cumBNLagSem_1_n - 1|Year:deptocode)
               ,
               data = datmalaria2[(datmalaria2$deptocode %in% deptosGood) & 
                                      (datmalaria2$semindex %in% c(7,8,9,10,11,12,13,14)), ],
               family=poisson,
               control=glmerControl(optimizer= "bobyqa",
                                    optCtrl  = list(maxfun=2e5)
               ))
summary(model6)
ranef(model6)

AIC(model6)
AIC(model4)
AIC(model5)
AIC(model5b)
AIC(model3)

# A profile of what interventions work best on each department:
summary(model5)
ranef(model5)

# What does bednet fixed effect estimation looks like, with p-values and all:
summary(model5b)
ranef(model5b)

# Model without other interventions
model1 = glmer(Notifs ~ 
                   1 + notifsLagYear_1_n +
                   cumBNLagSem_1_l10n:factor(deptocode) + 
                   (1 + factor(Semester) + notifsLagYear_1_n | deptocode)
               ,
               data = datmalaria2[(datmalaria2$deptocode %in% deptosGood) & 
                                      (datmalaria2$semindex %in% c(7,8,9,10,11,12,13,14)), ],
               family=poisson,
               control=glmerControl(optimizer= "bobyqa",
                                    optCtrl  = list(maxfun=2e5)
               ))
summary(model1)
ranef(model1)
AIC(model1)

modelF2 = glmer(Notifs ~ 
                   1 + notifsLagYear_1_n +
                   cumBNLagSem_1_l10n + 
                   (1 + factor(Semester) + notifsLagYear_1_n | deptocode)
               ,
               data = datmalaria2[(datmalaria2$deptocode %in% deptosGood) & 
                                      (datmalaria2$semindex %in% c(7,8,9,10,11,12,13,14)), ],
               family=poisson,
               control=glmerControl(optimizer= "bobyqa",
                                    optCtrl  = list(maxfun=2e5)
               ))
summary(modelF2)
1-exp(fixef(modelF2))


# ------------- 
# 2019/11/19
# Use a better counterfactual, rerun model with complete 2018 data
datmalaria2[(datmalaria2$deptocode %in% deptosGood) & 
              (datmalaria2$semindex %in% c(8,9,10,11,12,13,14)), notifsLagYear_1]
model0 = glmer(Notifs ~ 
                 1 +factor(Semester) + notifsLagYear_1_n +
                 cumBNLagSem_1_l10n + 
                 (1 + factor(Semester) + notifsLagYear_1_n + cumBNLagSem_1_l10n| deptocode)
               ,
               data = datmalaria2[(datmalaria2$deptocode %in% deptosGood) & 
                                    (datmalaria2$semindex %in% c(8,9,10,11,12,13,14)), ],
               family=poisson,
               control=glmerControl(optimizer= "bobyqa",
                                    optCtrl  = list(maxfun=2e5)
               ))
summary(model0)
# 1 semester lag is significantly contributing to diminishing notif rate
# 

model1 = glmer(Notifs ~ 
                  1 + factor(Semester) + notifsLagYear_1_n +
                 (cumBNLagSem_2_l10n + cumBNLagSem_1_l10n):factor(deptocode) + 
                  (1 +factor(Semester) + notifsLagYear_1_n | deptocode)
                ,
                data = datmalaria2[(datmalaria2$deptocode %in% deptosGood) & 
                                     (datmalaria2$semindex %in% c(8,9,10,11,12,13,14)), ],
                family=poisson,
                control=glmerControl(optimizer= "bobyqa",
                                     optCtrl  = list(maxfun=2e5)
                ))
summary(model1)
1-exp(fixef(model1))


model2 = glmer(Notifs ~ 
                 1 + factor(Semester) + notifsLagYear_1_n +
                 cumBNLagSem_1_l10n + 
                 (1 + factor(Semester) + notifsLagYear_1_n| deptocode)
               ,
               data = datmalaria2[(datmalaria2$deptocode %in% deptosGood) & 
                                    (datmalaria2$semindex %in% c(8,9,10,11,12,13,14)), ],
               family=poisson,
               control=glmerControl(optimizer= "bobyqa",
                                    optCtrl  = list(maxfun=2e5)
               ))
summary(model2)
1-exp(fixef(model2))

model3 = glmer(Notifs ~ 
                  1 + factor(Semester) + notifsLagYear_1_n +
                  cumBNLagSem_1_l10n:factor(deptocode) + 
                  txs_rociamiento + 
                  txs_criads + 
                (1 + notifsLagYear_1_n| deptocode)
                ,
                data = datmalaria2[(datmalaria2$deptocode %in% deptosGood) & 
                                     (datmalaria2$semindex %in% c(8,9,10,11,12,13,14)), ],
                family=poisson,
                control=glmerControl(optimizer= "bobyqa",
                                     optCtrl  = list(maxfun=2e5)
                ))
summary(model3)
vcov(model3)
AIC(model3)
ranef(model3)

# This is the official model:
datmalaria2[, Semester:=factor(Semester)]
model4 = glmer(Notifs ~ 
                 1 + Semester + notifsLagYear_1_n +
                 cumBNLagSem_1_l10n +  
                 txs_rociamiento + 
                 txs_criads + 
                 (1 + Semester + notifsLagYear_1_n|deptocode) + 
                 (-1 +  
                    txs_rociamiento + 
                    txs_criads|deptocode)
               ,
               data = datmalaria2[(datmalaria2$deptocode %in% deptosGood) & 
                                    (datmalaria2$semindex %in% c(8,9,10,11,12,13)), ],
               family=poisson,
               control=glmerControl(optimizer= "bobyqa",
                                    optCtrl  = list(maxfun=2e5)
               ))
summary(model4)
AIC(model4)
ranef(model4)
confint(model4, method="Wald")
#model4@beta[4] = -0.21771328
# cumBNLagSem_1_l10n -0.5902189 -0.21771328
# cumBNLagSem_1_l10n -0.403966   0.095029  -4.251 2.13e-05 ***


# Final model ^^^
# 


# Predict what would happen in bednets stop being distributed in 2015
counterfactual = datmalaria2[(datmalaria2$deptocode %in% deptosGood) & 
              (datmalaria2$semindex %in% c(7,8,9,10,11,12,13)), 
              .(cumBNLagSem_1_l10n = log10(cumBN_cf_LagSem_1+1), cumBNLagSem_2_l10n = log10(cumBN_cf_LagSem_2+1),
                cumBN_cf_l1 = cumBN_cf_LagSem_1, cumBN_l1 = cumBNLagSem_1, Notifs_old = Notifs, notifsLagYear_1_n, 
                notifsLagSem_2, txs_rociamiento, txs_criads, Semester, deptocode, semindex, cumBN_cf, cumBN, semindex, Semester)]
# Model explodes when estimations by department are made, models with overall effect are ok.
selmodel = model4
calcBootMerInts = T
counterfactual[semindex==7, predN := Notifs_old]
for (val in seq(8,13)) {
    print(paste("step", val))
    cfdata = copy(counterfactual[semindex==val])
    if (val>9) {
        cfdata[ , notifsLagYear_1_n := log10(1+counterfactual[semindex==val-2]$predN + counterfactual[semindex==val-1]$predN)]
    }
    if (val==9) {
        cfdata[, notifsLagYear_1_n := log10(1+counterfactual[semindex==val]$notifsLagSem_2 + counterfactual[semindex==val-1]$predN)]
    }
    counterfactual[semindex==val, predN := predict(selmodel, newdata=cfdata, type="response", re.form=NULL)]
    if (calcBootMerInts) {
        predict.fun <- function(model) {
          predict(model, newdata = cfdata, re.form = NULL)
        }
        counterfactual[semindex==val, bootMerPred:= exp(predict.fun(selmodel)) ]
        lmm.boots <- bootMer(selmodel, predict.fun, nsim = 100, ncpus=4, use.u=TRUE)
        confintscf <- confint(lmm.boots)
        counterfactual[semindex==val, bootMerPredCI1 := exp(confintscf[,colnames(confintscf)[1]]) ]
        counterfactual[semindex==val, bootMerPredCI2 := exp(confintscf[,colnames(confintscf)[2]]) ]
    }
}

if (calcBootMerInts) {
    counterfactual[,a := bootMerPred-Notifs_old]
    print("Estimación")
    print(sum(counterfactual[semindex > 7, a]))
    print(sum(counterfactual[semindex > 7, bootMerPredCI2-Notifs_old]))
    print(sum(counterfactual[semindex > 7, bootMerPredCI1-Notifs_old]))
    
    plotdata = counterfactual[,.(cfci1 = sum(bootMerPredCI1), cfci2 = sum(bootMerPredCI2), 
                             cf = sum(predN), f=sum(Notifs_old), bncf = sum(cumBN_cf), bnf = sum(cumBN) ),by=.(semindex)]
} else {
    counterfactual[,a := predN-Notifs_old]
    print("Estimación")
    print(sum(counterfactual$a))
    plotdata = counterfactual[,.(cf = sum(predN), f=sum(Notifs_old), bncf = sum(cumBN_cf), bnf = sum(cumBN) ),by=.(semindex)]
}
sum(counterfactual[,predN-Notifs_old])

plotdata = melt(plotdata, id.vars = ("semindex"))
ggplot(data = plotdata[variable %in% c("f", "cf")]) + geom_line(aes(x = semindex, y = value, color=variable))
if (calcBootMerInts) {
    ggplot(data = plotdata[variable %in% c("f", "cf", "cfci1", "cfci2")]) + geom_line(aes(x = semindex, y = value, color=variable))
}
ggplot(data = plotdata[variable %in% c("bncf")]) + geom_line(aes(x = semindex, y = value, color=variable))

ggplot() + geom_line(data = datmalaria[,.(value=sum(cumBN_cf)),by=semindex], aes(x = semindex, y = value))

write.csv(plotdata, "PCE/Impact Evaluation/wce_impact_eval_model4_counterfactual.csv")

