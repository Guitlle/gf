# --------------------------
# Author: Guillermo Ambrosio
# Date:  2019-03-22
#
# Preliminary analysis of bednets impact
# Also some analysis of other interventions such as breeding sites treatment
# --------------------------
install.packages("lme4")
library(data.table)
library(lme4)

codePath = "PCE/gf/"

# Requirements:
source(paste0(codePath, "core/GT_load_data.R"), encoding = "UTF-8")
source(paste0(codePath, "core/GT_helper_functions.R"), encoding = "UTF-8")

datmalaria = read.csv("PCE/Outcome Measurement Data/MALARIA/Gtm Malaria impact analysis data.csv",
                      row.names = 1)
breeds = read.csv("PCE/Outcome Measurement Data/MALARIA/SIGSA 6m - Malaria production by depto - 2014-2017.csv", row.names = 1)


datmalaria$notifsLagYear_1 = datmalaria$notifsLagSem_1 + datmalaria$notifsLagSem_2
datmalaria$notifsLagYear_1_n = log(datmalaria$notifsLagYear_1+1)

# Cum BN
datmalaria$cumBN_n = datmalaria$cumBN/10000  #  log10(datmalaria$cumBN+1)
datmalaria$cumBNLagSem_1_n =  datmalaria$cumBNLagSem_1/10000 # log10(datmalaria$cumBNLagSem_1+1)
datmalaria$cumBNLagSem_2_n =  datmalaria$cumBNLagSem_2/10000  # log10(datmalaria$cumBNLagSem_2+1)

datmalaria$cumBN_l10n = log10(datmalaria$cumBN+1)
datmalaria$cumBNLagSem_1_l10n = log10(datmalaria$cumBNLagSem_1+1)
datmalaria$cumBNLagSem_2_l10n = log10(datmalaria$cumBNLagSem_2+1)

# These departments should be included in the analysis given their amount of notifications
# Other departments have very low counts.
deptosGood = c(5, 6, 10,11,13, 14,16,17,18)
# ------- With other covariates --------------
# Lag by 1 year
breeds$YearLag = breeds$Year+1
breeds$txs_rociamiento = log10(1 + breeds$NRocdom)
breeds$txs_criads = log10(1 + breeds$NTxAplicados)
breeds$txs_crdcl = log10(1 + breeds$No..de.tratamientos.de.cura.radical)


breeds$criaderos = log10(breeds$NTemp + breeds$NPerm + 1)

datmalaria2 = merge(datmalaria, breeds, by.x = c("deptocode", "Year"), 
                     by.y = c("Deptocode", "YearLag"))

table(datmalaria2$Year, datmalaria2$semindex)

datmalaria2 = data.table(datmalaria2)
datmalaria2[ deptocode == 10]

model8 = glmer(Notifs ~ 
                   1 + factor(Semester) + notifsLagYear_1_n +
                   cumBNLagSem_1_n:factor(deptocode) + 
                   txs_rociamiento + 
                   txs_crdcl + 
                   txs_criads + 
                   (1 + notifsLagYear_1_n  + 
                        txs_rociamiento + 
                        txs_crdcl + 
                        txs_criads | deptocode)
               ,
               data = datmalaria2[(datmalaria2$deptocode %in% deptosGood) & 
                                      (datmalaria2$semindex %in% c(7,8,9,10,11,12)), ],
               family=poisson,
               control=glmerControl(optimizer= "bobyqa",
                                    optCtrl  = list(maxfun=2e5)
               ))
summary(model8)
ranef(model8)


require(RCurl)
afurl <- "https://raw.githubusercontent.com/lme4/lme4/2f57ca5359d98461ce98fbfa8679b8308596101e/R/allFit.R"
eval(parse(text=getURL(afurl)))

af8 <- allFit(model8, verbose=T)
