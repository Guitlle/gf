library("gee")

datmalaria = read.csv("/tmp/malaria_gee.csv")
#datmalaria$Notifs = datmalaria$Notifs-1
model = gee(Notifs ~ 1+ Year_n + factor(Semester) + 
                (notifsLagSem_1 + notifsLagSem_2  ) + 
                (bednetsLagSem_1 + bednetsLagSem_2 + bednetsLagSem_3 + bednetsLagSem_4), 
            data = datmalaria[datmalaria$semindex %in% c(5,6,7,8) , ], 
            id= deptocode, family=poisson,corstr = "unstructured")

summary(model)

library(lme4)

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

datmalaria$Notifs = datmalaria$Notifs - 1

datmalaria$notifsLagYear_1_n = datmalaria$notifsLagYear_1/1000
summary(datmalaria$notifsLagYear_1_n)
datmalaria$bednetsLagSem_1_n = datmalaria$bednetsLagSem_1/1000
summary(datmalaria$bednetsLagSem_1_n)

datmalaria$semindex_g = factor(datmalaria$semindex)
model1 = glmer(Notifs ~ 1 + factor(Year) + factor(Semester) + 
                  bednetsLagSem_1_n+notifsLagYear_1_n + 
                  (1+bednetsLagSem_1_n+notifsLagYear_1_n|deptocode) # + (1|deptocode:semindex_g)
                , 
            data = datmalaria[(datmalaria$semindex %in% c(5,6,7,8)), ],
            family=poisson)
summary(model1)

#---------------- IHME ----------------
# I tried the previous model but it couldnt converge and recommended to rescale covariates.
model = glmer.nb(Notifs ~ 1 + factor(Year) + factor(Semester) + 
                  bednetsLagSem_1_n+notifsLagYear_1_n + 
                  (1+bednetsLagSem_1_n+notifsLagYear_1_n|deptocode) # + (1|deptocode:semindex_g)
              , 
              data = datmalaria[(datmalaria$semindex %in% c(5,6,7,8)), ])
summary(model)

# Rescaled and removed lagged random fx. 
datmalaria$notifsLagSem_1_n_log10 = log10(datmalaria$notifsLagSem_1+1)
datmalaria$notifsLagSem_2_n_log10 = log10(datmalaria$notifsLagSem_2+1)
datmalaria$bednetsLagSem_1_n_log10 = log10(datmalaria$bednetsLagSem_1 + 1)  
datmalaria$bednetsLagSem_2_n_log10 = log10(datmalaria$bednetsLagSem_2 + 1)  
model = glmer.nb(Notifs ~ 1  + factor(Year) + 
                     bednetsLagSem_1_n_log10 + 
                     (1 + factor(Year) + 
                          bednetsLagSem_1_n_log10 |deptocode) # + (1|deptocode:semindex_g)
                 , 
                 data = datmalaria[(datmalaria$semindex %in% c(5,6,7,8)), ])
summary(model)
warnings()
#datmalaria[datmalaria$deptocode == 5, ]
