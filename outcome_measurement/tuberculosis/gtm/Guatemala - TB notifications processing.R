# ----------------------------------------------
# Guillermo Ambrosio (CIEAR)
#
# 2017-11-14
# Explore TB outcome data from MOH notifications databases.


# ----------------------------------------------
# Dependencies:
library(data.table)
library(stringr)
library(readxl)
library(ggplot2)
library(gridExtra)


# ----Configure------------------------------------------
saveGraphs = T
codePath = "PCE/gf/"

# Requirements:
source(paste0(codePath, "core/GT_load_data.R"), encoding = "UTF-8")
source(paste0(codePath, "core/GT_helper_functions.R"), encoding = "UTF-8")

# ------- Load TB notifications aggregated database ----------------
tbnots = read.csv("PCE/Outcome Measurement Data/TUBERCULOSIS/GTM - TB notifications 2012-2017.csv")

tbnots = data.table(tbnots)
table(tbnots$CONTACTOS, tbnots$YEAR)

tbnots[, .N, by = .(YEAR, PACIENTEPRIVADOLIBERTAD)]
tbnots[, .N, by = .(YEAR, CAUSADEMUERTE)]
tbnots[, .N, by = .(YEAR, LOCALIZACIONTB)]
tbnots[, .N, by = .(YEAR, TIPODETBPEDIATRICOS)]

# Overview of enter condition
ggplot(data = tbnots[CONDICIONINGRESO %in% c("nuevo", "fracaso", "recaida", "abandono recuperado", "abandono recuperada"),
                     .(Count = .N),
                        by=.(YearMonth = floor(YearMonth/100)*100 + floor(( YearMonth%% 100 - 1)/3)*3 + 1 , CONDICIONINGRESO)][,
                     .(Count, CONDICIONINGRESO, YearMonth =factor(YearMonth))]) + 
    geom_col(aes(YearMonth, Count, fill = CONDICIONINGRESO))  + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) + 
    labs(x="Quarter", title="TB Notifications time series\nGrouped by condition of entrance (including exits)")
# Confirming our quarter generator works well:
# tbnots[,.N,by = .(YearMonth = floor(YearMonth/100)*100 + floor(( YearMonth%% 100 - 1)/3)*3 + 1, YearMonth) ]

tbnots[CONDICIONINGRESO %in% c("nuevo", "fracaso", "recaida", "abandono recuperado", "abandono recuperada"),
       .(Count = .N),
       by=.(YearMonth = floor(YearMonth/100)*100)]
table(tbnots$CONDICIONINGRESO, tbnots$YEAR, useNA = "always")
table(tbnots$YEAR)
table(tbnots[CONDICIONINGRESO %in% c("nuevo", "fracaso", "recaida",
                                     "abandono recuperado", "abandono recuperada")]$CLASIFICACION)
# NAs are kids, and most probably they are new cases. It turns out these are prophylactic cases that receive isoniazide. Mostly children.
ggplot(data = tbnots[is.na(CONDICIONINGRESO), .(Count = .N), by=.(AgeGroup = ceiling(EDAD/5)*5)][order(AgeGroup), 
                   .(AgeGroup = factor(AgeGroup), Count)]) +
    geom_col(aes(AgeGroup, Count))

# Prophylactic cases
ggplot(data = tbnots[CONTACTOS == "quimio", .(AgeGroup = ceiling(EDAD/5)*5), by= .(YEAR)]) +
    geom_violin(aes(YEAR, AgeGroup), fill="blue")  + labs(title="Age distribution of contacts under treatment by year")
ggplot(data = tbnots[CONTACTOS == "quimio", .(Count = .N), by= .(YEAR)]) +
    geom_col(aes(YEAR, Count), fill="blue")  + labs(title="Contacts under treatment by year")

# New cases
ggplot(data = tbnots[CONDICIONINGRESO == "nuevo", .(AgeGroup = ceiling(EDAD/5)*5), by= .(YEAR) ]) +
    geom_violin(aes(factor(YEAR), AgeGroup), fill="blue")  + labs(title="Age distribution of new cases by year")
ggplot(data = tbnots[CONDICIONINGRESO == "nuevo", .(Count=.N), by= .(YEAR) ]) +
    geom_col(aes(factor(YEAR), Count), fill="blue")  + labs(title="New cases by year")
extramuros = tbnots[ str_detect(str_to_lower(CLASIFICACION), "posit")==T & 
             COD_DEPT%in%c(9,12,1,10,19,5,11,18) & CONDICIONINGRESO == "nuevo", .(Count=.N), by= .(YEAR) ]
extramuros$Contribution = factor("Health services", c("Outreach", "Health services"))
extramuros[YEAR==2017, Count:=Count-132]
extramuros = rbind(extramuros, list(2017,132,"Outreach"))
ggplot(data = extramuros) +
    geom_col(aes(factor(YEAR), Count,  fill=Contribution)) +
    scale_fill_manual(values=c("#4499FF", "#75CF56"), name="")+ 
    labs(x="", y="TB Notifications", title="Outreach contribution in prioritized departments" ) 

# Genexpert
table(str_detect(str_to_lower(tbnots$METODODX), "xpert"))
ggplot(data = tbnots[str_detect(str_to_lower(METODODX), "xpert")==T,
    .(Count = .N), by= .(YEAR)]) + geom_col(aes(factor(YEAR), Count), fill="blue") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))
dcast(tbnots[str_detect(str_to_lower(METODODX), "xpert"), .(Count=.N), by = .(UNIDADDX, COD_DEPT)], 
      COD_DEPT~UNIDADDX, sum)

genexpert = tbnots[str_detect(str_to_lower(METODODX), "xpert")==T,
       .(Count = .N), by= .(VIH, EDAD=ceiling(EDAD/10)*10, SEXO, COD_DEPT, YEAR )]
write.csv(genexpert, "/tmp/genexpert.csv")
dcast(genexpert, COD_DEPT~YEAR, sum)

# HIV counts
dcast.data.table(tbnots[CONDICIONINGRESO %in% c(NA, "nuevo", "recaida")  & !(CONTACTOS %in% c("quimio")),.(Count = .N),
       by=.(Year = floor(YearMonth/100), VIH)], Year~VIH, fun.aggregate = sum)
# Age counts
dcast.data.table(tbnots[CONDICIONINGRESO %in% c(NA, "nuevo", "recaida")  & !(CONTACTOS %in% c("quimio")),.(Count = .N),
            by=.(Year = floor(YearMonth/100), EDAD = ceiling(EDAD/10)*10 )], EDAD~Year, fun.aggregate = sum)

# Incidence time series:
yearly = tbnots[CONDICIONINGRESO %in% c(NA, "nuevo", "recaida")  & !(CONTACTOS %in% c("quimio")),.(Count = .N),
       by=.(Year = floor(YearMonth/100))]

yearly$Pop = sapply(yearly$Year, function (i)     sum(GTMuniPopulation(munisGT$municode, 
                                    rep(i, nrow(munisGT))), na.rm = T))

yearly[,Incidence := 100000*Count/Pop]
ggplot(data=yearly) + geom_line(aes(Year, Incidence)) + ylim(0,40) + labs(title = "Guatemala TB incidence rate per 100,000 people\nby year")

# Map of tb incidence by DAS for 2017.
mapdata = tbnots[CONDICIONINGRESO %in% c(NA, "nuevo") & floor(YearMonth/100) == 2017 & !(CONTACTOS %in% c("quimio")),.(counts = .N),
                by=.(deptocode = COD_DEPT )]
mapdata$pop = GTDeptoPopulation(mapdata$deptocode, rep(2017, nrow(mapdata) ) )
mapdata[, values := 100000*counts/pop]
gtmap_depto(mapdata) + scale_fill_distiller(name="Incidence rate", palette = "Blues", direction = 1, na.value = "#444444") + 
    labs(title="2017 TB incidence rate per 100,000 people\nby department") + 
    theme(legend.text=element_text(size=16), legend.title  = element_text(size=16), 
          title = element_text(size=16) 
        )

# Department trends
mapdataT = tbnots[CONDICIONINGRESO %in% c(NA, "nuevo") & !(CONTACTOS %in% c("quimio")),.(counts = .N),
                 by=.(deptocode = COD_DEPT, Year = floor(YearMonth/100))]

priordepts2014 = c( 5, 12, 10, 9, 11, 18, 19, 1)
priordepts2018 = c( 10, 11, 12, 14, 9, 13, 16, 18, 1, 5, 17)

mapdataT$pop = GTDeptoPopulation(mapdataT$deptocode, mapdataT$Year)
mapdataT[,Incidence := 100000 * counts/pop]
dcast(mapdataT, deptocode ~ Year )
mapLinmod = lm(Incidence ~ factor(deptocode)+ Year:factor(deptocode), data = mapdataT)
summary(mapLinmod)
factores = grep("\\d\\d?\\:Year$",names(coef(mapLinmod)))
trends = data.frame(values_ = coef(mapLinmod)[factores], deptocode = str_match( names(coef(mapLinmod))[factores], "(\\d\\d?)\\:Year$" )[,2])
trends$values = cut(trends$values_, rev(c(2.4,1.2,0.6,0,-0.6,-1.2,-2.4)), 
                    labels =rev(c("1.2 to 2.4","0.6 to 1.2","0 to 0.6","-0.6 to 0","-1.2 to -0.6","-2.4 to -1.2")))
#trends$values = trends$values_
highlight2014 = data.frame(coordinates(gtmDeptosIGN)[gtmDeptosIGN@data$CODIGO %in% priordepts2014,])
colnames(highlight2014) = c("lon", "lat")
highlight2018 = data.frame(coordinates(gtmDeptosIGN)[gtmDeptosIGN@data$CODIGO %in% priordepts2018,])
colnames(highlight2018) = c("lon", "lat")


plot = gtmap_depto(trends) + scale_fill_manual(values=rev(c("#DD2211", "#FF5511","#FFAA33", "#88CCFF", "#55AAFF", "#3377DD")), name="Incidence rate trend", na.value = "#444444") + 
    labs(title="TB incidence annual trend by department.\nFrom 2012 to 2017", 
         caption = "2014 prioritized departments are marked with a green dot.\n2018 prioritized departments are marked with a yellow dot.") + 
    theme(legend.text=element_text(size=16), legend.title  = element_text(size=16), 
          title = element_text(size=16) 
    ) 
plot + 
    geom_point(data = highlight2014, aes(fill = NULL, x=lon, y=lat), color = "#00AA11", size =4, show.legend=F) +
    geom_point(data = highlight2014, aes(fill = NULL, x=lon, y=lat), color = "black", size = 4, shape = 1, show.legend = F) +
    geom_point(data = highlight2018, aes(fill = NULL, x=lon+0.1, y=lat), color = "#FFAA00", size =4, show.legend=F) +
    geom_point(data = highlight2018, aes(fill = NULL, x=lon+0.1, y=lat), color = "black", size = 4, shape = 1, show.legend = F)

merge(trends[, c("values_", "deptocode")], deptosGT[,c("deptocode", "deptoname")], by.x = "deptocode", by.y = "deptocode")
