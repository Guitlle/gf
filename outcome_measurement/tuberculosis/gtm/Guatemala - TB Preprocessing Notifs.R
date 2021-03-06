# ----------------------------------------------
# Guillermo Ambrosio (CIEAR)
#
# 2017-11-14
# Explore TB outcome data from MOH notifications databases.


# ----------------------------------------------
# Dependencies:
library(data.table)
library(reshape2)
library(stringr)
library(readxl)
library(zoo)
library(ggplot2)
library(gridExtra)
library(stringdist)

# ----Configure------------------------------------------
saveGraphs = T
codePath = "PCE/gf/"

# Requirements:
source(paste0(codePath, "core/GT_load_data.R"), encoding = "UTF-8")
source(paste0(codePath, "core/GT_helper_functions.R"), encoding = "UTF-8")

# ----------------------------------------------
# Read the data:
TBNotif2012 = read_excel(paste0(dataPath, "Outcome Measurement Data/TUBERCULOSIS/Notificaciones TB/NOTIFICACIONES 2012.xlsx"), sheet = 1, col_names = F)
# Ignore rows without a department. This is to ignore extra rows with no data at all at the end of the spreadsheet.
tempcols = colnames(TBNotif2012)
TBNotif2012 = data.table(TBNotif2012)[3:.N,][!is.na(get(tempcols[4])),]
names(TBNotif2012) = c( "NOMBRES", "DIRECCION", "MUNICIPIO", "DEPARTAMENTO", "SERVICIODESALUD", "SEXO", "EDAD", 
                        "RANGOEDAD", "PESOLBS", "PESOKG", "CONDICIONINGRESO", "FECHANOTIFICACION", "FECHAINICIOTX",
                        "CLASIFICACION", "VIH" , "ESQUEMA", "CONTACTOS", 
                        "OTRASPATOLOGIAS")
TBNotif2012[,YEAR := 2012]

uglyTB2012DateToYearMonth <- function (input) {
    matches1 = str_match(input, "^(\\d\\d)\\.?\\.(\\d?\\d)(\\.?\\.|\\-)\\d{0,2}(\\d\\d)$")
    matches2 = str_match(input, "^(\\d{5})$")
    months1 = matches1[!is.na(matches1[,1]),3]
    input[!is.na(matches1[,1])] = paste0("20",matches1[!is.na(matches1[,1]),5], ifelse(nchar(months1)==1, paste0("0", months1), months1))
    input[!is.na(matches2[,1])] = format(as.Date(as.numeric(input[!is.na(matches2[,1])] )-2, origin="1900-01-01"), format="%Y%m")
    input[(is.na(matches2[,1]) & is.na(matches1[,1]))| as.integer(matches1[,3])>12] = NA
    as.integer(input)
}

TBNotif2012[,YearMonth := uglyTB2012DateToYearMonth(FECHANOTIFICACION)]

TBNotif2013 = read_excel(paste0(dataPath, "Outcome Measurement Data/TUBERCULOSIS/Notificaciones TB/NOTIFICACIONES 2013.xlsx"), sheet = 1, col_names = F)
# Ignore rows without a department. This is to ignore extra rows with no data at all at the end of the spreadsheet.
tempcols = colnames(TBNotif2013)
TBNotif2013 = data.table(TBNotif2013)[3:.N,][!is.na(get(tempcols[4])),]
names(TBNotif2013) = c( "NOMBRES", "DIRECCION", "MUNICIPIO", "DEPARTAMENTO", "SERVICIODESALUD", "SEXO", "EDAD", 
                        "RANGOEDAD", "PESOLBS", "PESOKG", "CONDICIONINGRESO", "FECHANOTIFICACION", "FECHAINICIOTX",
                        "CLASIFICACION", "VIH", "ESQUEMA", "CONTACTOS", 
                        "OTRASPATOLOGIAS")
TBNotif2013[,YEAR := 2013]

TBNotif2014 = read_excel(paste0(dataPath, "Outcome Measurement Data/TUBERCULOSIS/Notificaciones TB/NOTIFICACIONES 2014 GENERAL anterior.xlsx"), sheet = 2, col_names = F)
# Ignore rows without a department. This is to ignore extra rows with no data at all at the end of the spreadsheet.
tempcols = colnames(TBNotif2014)
TBNotif2014 = data.table(TBNotif2014[,1:25])[3:.N,][!is.na(get(tempcols[4])),]
names(TBNotif2014) = c( "NOMBRES", "DIRECCION", "MUNICIPIO", "DEPARTAMENTO", "SERVICIODESALUD", "SEXO", "EDAD", 
                        "RANGOEDAD", "PESOLBS", "PESOKG", "CONDICIONINGRESO", "FECHANOTIFICACION", "FECHAINICIOTX",
                        "CLASIFICACION", "TIPODETBPEDIATRICOS", "VIH", "FECHAPRUEBAVIH", "ESQUEMA", "CONTACTOS", 
                        "OTRASPATOLOGIAS", "CONDICIONEGRESO", "FECHAMUERTE", "CAUSADEMUERTE", 
                        "PACIENTEPRIVADOLIBERTAD", "DEPORTADO")
TBNotif2014[,YEAR := 2014]

TBNotif2015 = read_excel(paste0(dataPath, "Outcome Measurement Data/TUBERCULOSIS/Notificaciones TB/NOTIFICACIONES 2015.xlsx"), sheet = 2, col_names = F)
tempcols = colnames(TBNotif2015)
TBNotif2015 = data.table(TBNotif2015)[3:.N, 1:31][!is.na(get(tempcols[3])),]
names(TBNotif2015) = c("NOMBRES", "DIRECCION", "MUNICIPIO", "DEPARTAMENTO", "SERVICIODESALUD", "SEXO", "EDAD",
                       "RANGOEDAD", "PESOLBS", "PESOKG", "CONDICIONINGRESO", "FECHANOTIFICACION", "FECHAINICIOTX", 
                       "CLASIFICACION", "LOCALIZACIONTB", "METODODX", "VIH", "FECHAPRUEBAVIH", "ESQUEMA", "CONTACTOS",
                       "CONTACTO_000_014", "CONTACTO_MAYORA_015", "CASOINDICE", "DESARROLLOTBCLASIFICACION", 
                       "OTRASPATOLOGIAS", "EMPLEADOMSPAS", "UNIDADDX", "FALLECIDOS", "FECHAMUERTE", "CAUSADEMUERTE", 
                       "PACIENTEPRIVADOLIBERTAD")
TBNotif2015[,YEAR := 2015]

namesTB2016 = c( "CORRELATIVO", "MUNICIPIO", "DEPARTAMENTO", "SERVICIODESALUD", 
                 "DAS", "SEXO", "FECHANACIMIENTO", "FECHAACTUAL", "EDAD", "RANGOEDAD", "EDADDECADA", "EDUCACION", "PUEBLO", "OCUPACION",
                 "PESOLBS", "PESOKG", "CONDICIONINGRESO", "NUEVACONDICIONINGRESO", "CLASIFICACION", "LOCALIZACIONTB",
                 "FECHANOTIFICACION", "FECHAINICIOTX", "ESQUEMA", "FECHADX", "METODODX", "NUEVOMETODODX", "VIH", "FECHAPRUEBAVIH",
                 "OTRASPATOLOGIAS", "PDS", "PACIENTEPRIVADOLIBERTAD", "DEPORTADO", "UNIDADDX", "EMPLEADOMSPAS", "CONTACTOS",
                 "CONTACTO_000_0004", "CONTACTO_MAYORA_005", "QUIMIO_VIH", "CASOINDICE", "FALLECIDOS", "FECHAMUERTE", 
                 "CAUSADEMUERTE")
TBNotif2016 = read_excel(paste0(dataPath, "Outcome Measurement Data/TUBERCULOSIS/Notificaciones TB/NOTIFICACIONES TB 2016.xlsx"), sheet = 2, col_names = F)
# Ignore rows without a department. This is to ignore extra rows with no data at all at the end of the spreadsheet.
tempcols = colnames(TBNotif2016)
TBNotif2016 = data.table(TBNotif2016)[3:.N,][!is.na(get(tempcols[3])),]
names(TBNotif2016) = namesTB2016
TBNotif2016_Quimio = read_excel(paste0(dataPath, "Outcome Measurement Data/TUBERCULOSIS/Notificaciones TB/NOTIFICACIONES TB 2016.xlsx"), sheet = 4, col_names = F)
# Ignore rows without a department. This is to ignore extra rows with no data at all at the end of the spreadsheet.
tempcols = colnames(TBNotif2016_Quimio)
TBNotif2016_Quimio = data.table(TBNotif2016_Quimio)[2:.N,0:42][!is.na(get(tempcols[3])),]
names(TBNotif2016_Quimio) = namesTB2016
TBNotif2016_Quimio[,DBCATEGORY:="QUIMIO"]

TBNotif2016_Mb = read_excel(paste0(dataPath, "Outcome Measurement Data/TUBERCULOSIS/Notificaciones TB/NOTIFICACIONES TB 2016.xlsx"), sheet = 1, col_names = F)
# Ignore rows without a department. This is to ignore extra rows with no data at all at the end of the spreadsheet.
tempcols = colnames(TBNotif2016_Mb)
TBNotif2016_Mb = data.table(TBNotif2016_Mb)[2:.N,0:42][!is.na(get(tempcols[3])),]
names(TBNotif2016_Mb) = namesTB2016
TBNotif2016_Mb[,DBCATEGORY:="Mycobacterium"]
TBNotif2016 = rbind(TBNotif2016, TBNotif2016_Quimio, TBNotif2016_Mb, fill = T)

TBNotif2016[,YEAR := 2016]
nrow(TBNotif2016)


# 2017 data
TBNotif2017 = read_excel(paste0(dataPath, "Outcome Measurement Data/TUBERCULOSIS/Notificaciones TB/NOTIFICACIONES TB 2017.xlsx"), 
                         sheet = 5, col_names = F)
# Ignore rows without a department. This is to ignore extra rows with no data at all at the end of the spreadsheet.
tempcols = colnames(TBNotif2017)
TBNotif2017 = data.table(TBNotif2017)[5:.N,][!is.na(get(tempcols[2])),]
names(TBNotif2017) = c("CORRELATIVO", "DAS",	"DISTRITO",	"SERVICIODESALUD",	"FECHANACIMIENTO", 	"EDAD",	"SEXO",
                       "PESOLBS",	"EDUCACION",	"PUEBLO",	"OCUPACION",	"MIGRACION",
                       "CONDICIONPX", 	"CAUSAMUERTE",	"CONDICIONINGRESO",	"NUEVACONDICIONINGRESO",
                       "TipoTB",	"CLASIFICACION",	"LOCALIZACIONTB",	"FECHANOTIFICACION",
                       "MESNOTIFICACION", "FECHADX",	"METODODX",	"FECHAINICIOTX", "OTRASPATOLOGIAS",
                       "OTRASPATOLOGIAS_2", "OTRASPATOLOGIAS_3",
                       "CONTACTOS", "CONTACTO_000_004", "CONTACTO_MAYORA005", "QUIMIO_VIH", "CASOINDICE",
                       "VIH",	"FECHAPRUEBAVIH",	"TPC",	
                       "TARV",	"LUGARTARV",	"INICIOTARV",	"ESQUEMA",	"REFIERE",	"BXPOSITIVAS",	"PDS",	
                       "RESULTADOPDS",	"FECHAPDS",	"CONTROL2_RESULTADO", "CONTROL2_FECHA", 
                       "CONTROL4_RESULTADO", "CONTROL4_FECHA", "CONTROL6_RESULTADO", "CONTROL6_FECHA", "CULTIVO_RESULTADO", 
                       "CULTIVO_FECHA", "CONDICIONEGRESO", "FECHAEGRESO", "OBSERVACION")
TBNotif2017_Qm = read_excel(paste0(dataPath, "Outcome Measurement Data/TUBERCULOSIS/Notificaciones TB/NOTIFICACIONES TB 2017.xlsx"), 
                            sheet = 6, col_names = F)
tempcols = colnames(TBNotif2017_Qm)
TBNotif2017_Qm = data.table(TBNotif2017_Qm)[5:.N,1:55][!is.na(get(tempcols[3])),]
names(TBNotif2017_Qm) = names(TBNotif2017)
TBNotif2017_Qm[,DBCATEGORY:="QUIMIO"]

namesMDR17 = c( "CORRELATIVO",	"DIRECCION",	"MUNICIPIO",	"DAS",	"SERVICIODESALUD",	"SEXO",
                "EDAD",	"CONDICIONINGRESO",	"FECHANOTIFICACION",	"FECHAINICIOTX",	"CLASIFICACION",
                "RESISTENCIA",	"FECHAPDS",	"FECHARESULTADOPDS")
TBNotif2017_mdr = read_excel(paste0(dataPath, "Outcome Measurement Data/TUBERCULOSIS/Notificaciones TB/NOTIFICACIONES TB 2017.xlsx"), 
                             sheet = 7, col_names = F)
tempcols = colnames(TBNotif2017_mdr)
TBNotif2017_mdr = data.table(TBNotif2017_mdr)[7:.N,1:14][!is.na(get(tempcols[4])),]
names(TBNotif2017_mdr) = namesMDR17
TBNotif2017_mdr[,DBCATEGORY:="MDR"]

TBNotif2017 = rbind(TBNotif2017, TBNotif2017_Qm, TBNotif2017_mdr, fill=T)
TBNotif2017[,YEAR := 2017]


TBNotif2018 = read_excel(paste0(dataPath, "Outcome Measurement Data/TUBERCULOSIS/Notificaciones TB/NOTIFICACIONES TB Jun 2018.xlsx"), sheet = 1, col_names = F)
tempcols = colnames(TBNotif2018)
TBNotif2018 = data.table(TBNotif2018)[4:.N, 0:66][!is.na(get(tempcols[3])),]
names(TBNotif2018) = c("ID", "X1", "DAS", "DISTRITO", "SERVICIODESALUD", "X2", "X3", "X4", "EDAD", "SEXO", "PESOLBS", "EDUCACION", "PUEBLO", "OCUPACION", "MIGRACION", "CONDICIONPX", 
                       "CAUSAMUERTE", "CONDICIONINGRESO", "NUEVACONDICIONINGRESO", "TipoTB", "LOCALIZACIONTB", "FECHANOTIFICACION", "MESNOTIFICACION",
                       "FECHADX", "METODODX", "GENEXPERT", "FECHAGX", 
                       "FECHAINICIOTX", "PACIENTEPRIVADOLIBERTAD", "DIABETES", "HIPERTENSION", "OTRASPATOLOGIAS", 
                       "VIH", "FECHAPRUEBAVIH", "TPC", "TARV", "LUGARTARV", "INICIOTARV",
                       "ESQUEMA", "REFIERE", "PDS", "RESULTADOPDS", "FECHAPDS", 
                       "CONTROL1_RESULTADO", "CONTROL1_FECHA", "CONTROL2_RESULTADO", "CONTROL2_FECHA", "CONTROL2_GX",
                       "CONTROL3_RESULTADO", "CONTROL3_FECHA", "CONTROL3_GX", 
                       "CONTROL4_RESULTADO", "CONTROL4_FECHA", "CONTROL4_GX", "CONTROL5_RESULTADO", "CONTROL5_FECHA", "CONTROL5_GX",
                       "CONTROL6_RESULTADO", "CONTROL6_FECHA", "CONTROL6_GX", "CONTROL7_RESULTADO", "CONTROL7_FECHA", "CONTROL7_GX",
                       "CONDICIONEGRESO", "FECHAEGRESO", "OBSERVACIONES")
TBContactos2018 = read_excel(paste0(dataPath, "Outcome Measurement Data/TUBERCULOSIS/Notificaciones TB/NOTIFICACIONES TB Jun 2018.xlsx"), sheet = 3, col_names = F)
tempcols = colnames(TBContactos2018)
TBContactos2018 = data.table(TBContactos2018)[2:.N, 1:39][!is.na(get(tempcols[3])),]
names(TBContactos2018) = c("ID", "X1", "DAS", "DISTRITO", "SERVICIODESALUD", "PRIORIZACION", "X2", "X3", 
                           "EDAD", "SEXO", "PESOLB", "EDUCACION", "PUEBLO", "OCUPACION", "MIGRACION", 
                           "CONDICIONPX", "CAUSAMUERTE", "FECHANOTIFICACION", "MESNOTIFICACION", "FECHADX", 
                           "METODODX", "FECHAINICIOTX", "CONTACTOS", "CONTACTO_000_004", 
                           "CONTACTO_MAYORA_005", "QUIMIO_VIH", "X4", "VIH", "FECHAPRUEBAVIH", "TPC", 
                           "TARV", "LUGARTARV", "INICIOTARV", "ESQUEMA", "REFIERE", "BXPOSITIVAS", 
                           "PDS", "RESULTADOPDS", "FECHAPDS")
TBContactos2018[,DBCATEGORY:="QUIMIO"]
TBMDR2018 = read_excel(paste0(dataPath, "Outcome Measurement Data/TUBERCULOSIS/Notificaciones TB/NOTIFICACIONES TB Jun 2018.xlsx"), sheet = 4, col_names = F)
tempcols = colnames(TBMDR2018)
TBMDR2018 = data.table(TBMDR2018)[6:.N, 1:15][!is.na(get(tempcols[5])),]
names(TBMDR2018) = c("ID", "X1", "X2", "MUNICIPIO", "DEPARTAMENTO", "SERVICIODESALUD", "SEXO", "EDAD", "CONDICIONINGRESO", "FECHANOTIFICACION", "FECHAINICIOTX", "METODODX", "RESISTENCIA", "FECHAPDS", "FECHARESULTADOPDS")
TBMDR2018[,DBCATEGORY:="MDR"]
TBNotif2018 = rbind(TBNotif2018, TBContactos2018, TBMDR2018, fill=T)
TBNotif2018[,YEAR := 2018]

# Readxl has serious issues with guessing data types and handling empty cells around the document. 
# Thus, the easiest way to load a bad excel, such as these, is to load everything without type 
# conversion and columns definitions and then assign column names manually. Missing python xlrd.

# Load other covariates:

deptoIndgnProp = read.csv(paste0(dataPath, "Covariates and Other Data/Demographics/Guatemala_indigenousPobProp.csv"))
deptoIndgnProp = data.table(deptoIndgnProp)

pobrezaGT11 = data.table(read.csv(paste0(dataPath, "Covariates and Other Data/Demographics/Guatemala-Pobreza-2011.csv")))


# ----------------------------------------------
# Prepare the data
TBNotifAll = rbindlist(list(TBNotif2012, TBNotif2013, TBNotif2014, TBNotif2015, TBNotif2016, 
                            TBNotif2017, TBNotif2018), fill = TRUE)
TBNotifAll$YEAR = as.integer(TBNotifAll$YEAR)
TBNotifAll[YEAR > 2012, NotificationDate:= as.Date(as.numeric(FECHANOTIFICACION)-2, origin="1900-01-01")]
TBNotifAll[YEAR > 2012, YearMonth_ := format(NotificationDate, format = "%Y%m")];
TBNotifAll[, YearMonth := ifelse(YEAR>2012, as.numeric(YearMonth_), YearMonth)]
print("Bad dates:")
TBNotifAll[is.na(YearMonth), .N, by = YEAR]
TBNotifAll = TBNotifAll[YearMonth>=201201 & YearMonth<=201812]
TBNotifAll[, CONDICIONINGRESO := str_to_lower(trimws(CONDICIONINGRESO))]
TBNotifAll[, CONTACTOS := str_to_lower(trimws(CONTACTOS))]
TBNotifAll[, RANGOEDAD := trimws(TBNotifAll$RANGOEDAD)]
TBNotifAll[, SEXO := str_to_lower(trimws(SEXO))]
TBNotifAll[, VIH := str_to_lower(trimws(TBNotifAll$VIH))]
TBNotifAll[, VIH := factor(VIH, level=c("nr", "r", "fallecido", "fallecio", "nd", "nhp"), 
                           labels=c("Not Reactive", "Reactive", "Death", "Death_temp", "NA", "NA_temp"))]
TBNotifAll[VIH=="NA_temp" | VIH=="NA", VIH := "NA"]
TBNotifAll[VIH=="Death_temp", VIH := "Death"]
table(TBNotifAll$VIH, TBNotifAll$YEAR)

TBNotifAll[, EDAD := 
               str_replace(
                   str_replace( 
                       str_replace(
                           str_replace(EDAD, "^\\d*\\s*(D|d)\\s*$", "0"), 
                           "^(\\d*)\\s*?(mese?s?|MESES?|m|M|N|n)\\s*?(\\d*\\s*?(D|d)\\s*?)?$", "-\\1") ,  # Negative values will be months
                       "\\s*(AÑOS|AÑO|anos|años|año|ano)\\s*", "A"), 
                   "^\\s*(\\d*)\\s*(a|A|AEM)\\.?(\\s*\\d*\\s*[mMdDA]?\\.?\\s*\\d*)?\\s*$", "\\1")
           ]

TBNotifAll[, EDAD := as.integer(EDAD)]
TBNotifAll[, EDAD := ifelse(EDAD < 0, floor(-EDAD/12), EDAD)]
# Conteos por año por decada de edad
# Using data only for jan - jun  because 2018 data is available for these months only
table(ceiling(TBNotifAll[YearMonth%%100 <= 6, EDAD]/10)*10, TBNotifAll[YearMonth%%100 <= 6, YEAR], useNA = "always")

# Mujeres en edad reproductiva con TB
TBNotifAll[CONDICIONINGRESO== "nuevo" & SEXO=="f" & EDAD>10 & EDAD<54 & YearMonth%%100<=6,.N,by=.(YEAR)]

#TBNotifAll[, COD_MUNI := as.integer(getMuniCodeByName(ifelse(is.na(MUNICIPIO) || (MUNICIPIO=="ND"), 
#               ifelse(is.na(SERVICIODESALUD) || (SERVICIODESALUD=="ND"), NA, SERVICIODESALUD), 
#                 MUNICIPIO), 
#               ifelse(is.na(DEPARTAMENTO) || (DEPARTAMENTO=="ND"), NA, DEPARTAMENTO))), 
#           by=1:nrow(TBNotifAll)]

TBNotifAll[, DEPTO_CORRECTED := ifelse(is.na(DEPARTAMENTO) || (DEPARTAMENTO=="ND"), 
                                       str_replace(
                                           str_replace(
                                               str_to_lower(trimws(DAS)), 
                                               "\\s*(sur|norte|nor|central)\\s*(oriente|occidente)?\\s*", ""),
                                           "(quich(e|é))?\\s*(ixcan|ixcán|ixil)", "quiche"),
                                       DEPARTAMENTO), by=1:nrow(TBNotifAll)]

TBNotifAll[, COD_DEPT := getDeptoCodeByName(DEPTO_CORRECTED), by=1:nrow(TBNotifAll)]

table(TBNotifAll$YearMonth)
# Produce a unified dataset with all years and columns:
# Notifications
write.csv(TBNotifAll[is.na(CONTACTOS),], "./PCE/Outcome Measurement Data/TUBERCULOSIS/Notificaciones TB/GTM - TB notifications 2012-Sep2018.csv")
# Contactos - Quimioprofilaxis
write.csv(TBNotifAll[!is.na(CONTACTOS),], "./PCE/Outcome Measurement Data/TUBERCULOSIS/Notificaciones TB/GTM - TB quimio 2012-Sep2018.csv")

write.csv(TBNotifAll[is.na(CONTACTOS),.(notificaciones = .N), by = .(YEAR, COD_DEPT)], "./PCE/Outcome Measurement Data/TUBERCULOSIS/Notificaciones TB/GTM - TB Notifications by Department and Year 2012-Sep2018.csv")




TBNotifAll[CONTACTOS == "quimio", .N, by = YEAR]
TBNotifAll[DBCATEGORY == "QUIMIO", .N, by = YEAR]
dcast(TBNotifAll[CONTACTOS == "quimio", .N, by = .(YEAR, EDAD <= 5)], 
        YEAR ~ EDAD, value.var = "N")

table(TBNotifAll[, "QUIMIO_VIH"])

### CHECKPOINT
TBNotifAll = read.csv("./PCE/Outcome Measurement Data/TUBERCULOSIS/Notificaciones TB/GTM - TB notifications 2012-Jun2018.csv")
TBNotifAll = data.table(TBNotifAll)

# ----Month by Municipality table:---------------------------------

# write.csv(table(TBNotifAll$YearMonth, TBNotifAll$COD_MUNI), file = paste0(dataPath, "Outcome Measurement Data/TUBERCULOSIS/MunicipalityMonth.csv") )

write.csv(TBNotifAll[, .(NCases = .N, NInitiating = sum(as.integer(toupper(CONDICIONINGRESO) == "nuevo")) ), by = .(COD_DEPT, YearMonth, AgeRange = ceiling(EDAD/10), Gender = SEXO, HIV = VIH) ], file = paste0(dataPath, "Outcome Measurement Data/TUBERCULOSIS/GTM - TB 2012-2017 - DeptMonth.csv") )

# TBNotifAll$RANGOEDAD = factor(TBNotifAll$RANGOEDAD, levels = order(unique(TBNotifAll$RANGOEDAD)))

# --------Monthly Time Series--------------------------------------
# Exploring
# All plots group by gender
# Plot monthly count time series
ggplot(data = TBNotifAll, aes(factor(YearMonth), fill=toupper(SEXO)))+ geom_bar(position="dodge")+ theme(axis.text.x = element_text(angle = 90)) + labs(fill="Gender", x="Month", y="Notifications counts per month", title = "Time series of notifications")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2012-2016-Monthly.png")
# Bar plot by age range
ggplot(TBNotifAll, aes(x=RANGOEDAD, fill=SEXO))+geom_bar(position="dodge")  + theme(axis.text.x = element_text(angle = 90)) + labs(fill="Gender", x="Age range", y="Notifications counts per Age range", title="Age range")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2012-2016-AgeRange.png")
# Bar plot by weight in pounds (some KG data is just missing, so pounds will be preferred)
# This is only for adults
ggplot(TBNotifAll[(RANGOEDAD %in% c("000 - 004","005 - 010")),], aes(x= as.numeric(PESOLBS), fill=SEXO))+geom_histogram(bins=20, position="dodge") + labs(fill="Gender", x="Weight (Pounds)", y="Notifications counts per month", title="By weight for children < 10 years old")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2012-2016-Weight_Kids0-10.png")
# This is for childs from 0 to 10 years old
ggplot(TBNotifAll[!(RANGOEDAD %in% c("000 - 004","005 - 010")),], aes(x= as.numeric(PESOLBS), fill=SEXO ))+geom_histogram(bins=20, position="dodge")  + labs(fill="Gender", x="Weight (Pounds)", y="Notifications count per month", title="By weight for people > 10 years old")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2012-2016-Weight GT 10.png")
# Diagnostic methods:
ggplot(as.data.frame(table(TBNotifAll$METODODX, TBNotifAll$SEXO, useNA = "always")), aes(x=reorder(Var1, Freq), y=Freq, fill=Var2)) + 
    geom_bar(stat="identity", position="dodge") + theme(axis.text.x = element_text(angle = 90,hjust=0.95,vjust=0.2))  + labs(fill="Gender", x="Diagnosis methods", y="Notifications counts per month", title="Diagnosis methods")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2012-2016-Dx_method.png")
# ---------Monthly Time Series with HIV-------------------------------------
# VIH plots
# Monthly time series of notifications (separate by HIV and gender)
ggplot(data = TBNotifAll, aes(factor(YearMonth), fill=SEXO))+ geom_bar(position="dodge")+ theme(axis.text.x = element_text(angle = 90)) + labs(fill="Gender", x="Month", y="Notifications counts per month", title = "Time series of notifications") + facet_wrap(~ VIH, nrow = 1, ncol=3, scales="free_y")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2012-2016-Monthly-VIH.png", height=4, width=9)
# Bar plot by age range
ggplot(TBNotifAll, aes(x=RANGOEDAD, fill=SEXO))+geom_bar(position="dodge")  + theme(axis.text.x = element_text(angle = 90)) + labs(fill="Gender", x="Age range", y="Notifications counts per Age range", title="Age range") + facet_wrap(~ VIH, nrow = 1, ncol=3, scales="free_y")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2012-2016-AgeRange-VIH.png", height=4, width=9)
# Bar plot by weight in pounds (some KG data is just missing, so pounds will be preferred)
# This is only for adults
ggplot(TBNotifAll[(RANGOEDAD %in% c("000 - 004","005 - 010")),], aes(x= as.numeric(PESOLBS), fill=SEXO))+geom_histogram(bins=20, position="dodge") + labs(fill="Gender", x="Weight (Pounds)", y="Notifications counts per month", title="By weight for children < 10 years old") + facet_wrap(~ VIH, nrow = 1, ncol=3, scales="free_y")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2012-2016-Weight_NoKids-VIH.png", height=4, width=9)
# This is for childs from 0 to 10 years old
ggplot(TBNotifAll[!(RANGOEDAD %in% c("000 - 004","005 - 010")),], aes(x= as.numeric(PESOLBS), fill=SEXO ))+geom_histogram(bins=20, position="dodge")  + labs(fill="Gender", x="Weight (Pounds)", y="Notifications count per month", title="By weight for people > 10 years old") + facet_wrap(~ VIH, nrow = 1, ncol=3, scales="free_y")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2012-2016-WeightKids0-10-VIH.png", height=4, width=9)
# Diagnostic methods:
ggplot(as.data.frame(table(TBNotifAll$METODODX, TBNotifAll$SEXO, TBNotifAll$VIH, useNA = "always")), aes(x=reorder(Var1, Freq), y=Freq, fill=Var2)) + 
    geom_bar(stat="identity", position="dodge") + theme(axis.text.x = element_text(angle = 90,hjust=0.95,vjust=0.2, size = 5))  + labs(fill="Gender", x="Diagnosis methods", y="Notifications counts per month", title="Diagnosis methods") + facet_wrap(~ Var3, nrow = 1, ncol=3, scales="free_y")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2014-2015-Dx_method-VIH.png", height=4, width=9)

# ----------Maps------------------------------------
# Prepare GIS data. R has a very ugly way of handling this:
gtmMunisDataCopy = cbind(gtmMunisIGN@data)
gtmMunisIGN@data$id = rownames(gtmMunisIGN@data)
gtmMunisIGN@data = merge(gtmMunisIGN@data, TBNotifAll[toupper(CONDICIONINGRESO)=="NUEVO" & YEAR < 2016 & YEAR > 2011, .(TBCases=.N),by=COD_MUNI], by.x = "COD_MUNI__", by.y="COD_MUNI", all.x=TRUE, sort=FALSE)
gtmMunisIGN@data = merge(gtmMunisIGN@data, dt.munisGT[, .(Poblacion2015, COD_MUNI__)], by.x = "COD_MUNI__", by.y="COD_MUNI__", all.x=TRUE, sort=FALSE)
gtmMunisIGN.map.df = fortify(gtmMunisIGN)
gtmDeptosIGN.map.df = fortify(gtmDeptosIGN)


# Plotting TB cases per municipality
plot = ggplot(data=gtmMunisIGN@data, aes(fill=100000*TBCases/Poblacion2015/4)) + geom_map(aes(map_id=id), colour = rgb(1,1,1,0.5), map = gtmMunisIGN.map.df) + expand_limits(x = gtmMunisIGN.map.df$long, y = gtmMunisIGN.map.df$lat) + coord_quickmap() + scale_fill_gradientn(colours = c("#777777", "#ddcc22", "#DD5522", "#AA1111"), values=c(0,0.005,0.7,1), trans="log10") + labs(fill= "Rate", title="TB incidence rate per 100,000 people per year (Data from 2012 to 2015)")
# Overlay the departments
plot + geom_polygon(data = gtmDeptosIGN.map.df, aes(long, lat, group=group), fill="#00000000", color="#00000066", size=1)  + theme_void()

if (saveGraphs) 
    
    ggsave("PCE/Graficas/TB_Gt_Notifications_2012-2015-Map-Muncipalities.png", height=8, width=8)

# Restore data
gtmMunisIGN@data = gtmMunisDataCopy

# ----TB Relation with indigenous pop----------------------
# Indigenous population proportion by department
gtmDeptosIGN@data$id = rownames(gtmDeptosIGN@data)
gtmDeptosIGN@data = merge(gtmDeptosIGN@data, deptoIndgnProp[, .(INDGN, DEPTO)], by.x = "CODIGO", by.y="DEPTO", all.x=TRUE, sort=FALSE)

gtmDeptosIGN.map.df = fortify(gtmDeptosIGN)

ggplot(data = gtmDeptosIGN@data, aes(fill= 100*INDGN)) + geom_map(aes(map_id=id), colour = "#FFFFFF88", map=gtmDeptosIGN.map.df) + expand_limits(x = gtmDeptosIGN.map.df$long, y = gtmDeptosIGN.map.df$lat) + coord_quickmap() + labs(fill="Indigenous population", title="Indigenous population proportion by department")

if (saveGraphs) 
    ggsave("PCE/Graficas/GT_Indigenous population by Depto.png", height=8, width=8)

# Relation between indigenous population proportion and TB prevalence
deptoData = merge(TBNotifAll[, .(TB=.N) ,by=COD_DEPTO], deptoIndgnProp[, .(DEPTO,INDGN)], by.x="COD_DEPTO", by.y="DEPTO")
deptoData = merge(deptoData, dt.munisGT[,.(Pob = sum(Poblacion, na.rm=TRUE)), by=COD_DEPT__], by.x="COD_DEPTO", by.y="COD_DEPT__")
ggplot(data = deptoData, aes(y=log(TB/Pob), x=log(INDGN/(1-INDGN)))) + geom_point() + geom_smooth(method=lm)  + labs(title="Indigenous population proportion vrs TB Cases per department", y="[Log] TB incidence, cases per 1,000 persons per 2 years", x="[Logit] Indigenous population proportion")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2012-2016-IndgnPop vs Incidence by Depto.png", height=8, width=8)
# Histogram of both variables with log and logit transformations
grid.arrange(
    ggplot(deptoData, aes(INDGN)) + geom_histogram(bins = 10),
    ggplot(deptoData, aes(log(INDGN))) + geom_histogram(bins = 10),
    ggplot(deptoData, aes(log(INDGN/(1-INDGN)))) + geom_histogram(bins = 10),
    ggplot(deptoData, aes(TB/Pob)) + geom_histogram(bins = 10),
    ggplot(deptoData, aes(log(TB/Pob))) + geom_histogram(bins = 10),
    ncol=3, nrow=2
) 

if (saveGraphs) { 
    g = arrangeGrob(
        ggplot(deptoData, aes(INDGN)) + geom_histogram(bins = 10),
        ggplot(deptoData, aes(log(INDGN))) + geom_histogram(bins = 10),
        ggplot(deptoData, aes(log(INDGN/(1-INDGN)))) + geom_histogram(bins = 10),
        ggplot(deptoData, aes(TB/Pob)) + geom_histogram(bins = 10),
        ggplot(deptoData, aes(log(TB/Pob))) + geom_histogram(bins = 10),
        ncol=3, nrow=2
    ) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2014-2015-IndgnPop,Incidence Histograms.png", height=8, width=8, g)
}

# Fitting the linear model
fit_TBIndg = lm(formula = log(TB/Pob) ~ log(INDGN/(1-INDGN) ), data = deptoData)
summary(fit_TBIndg)
# 
# Call:
#     lm(formula = log(TB/Pob) ~ log(INDGN/(1 - INDGN)), data = deptoData)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -1.2313 -0.4186 -0.1030  0.4059  1.4763 
# 
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            -7.82236    0.16247 -48.147   <2e-16 ***
#     log(INDGN/(1 - INDGN)) -0.03544    0.05796  -0.611    0.548    
# ---
#     Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.6963 on 20 degrees of freedom
# Multiple R-squared:  0.01835,	Adjusted R-squared:  -0.03073 
# F-statistic: 0.3739 on 1 and 20 DF,  p-value: 0.5478

# ----TB relation with poverty-----------------------------

pobrData = merge( merge(TBNotifAll[, .(TBCases=.N),by=COD_MUNI], pobrezaGT11[is.na(IsDepto)], by.x = "COD_MUNI", by.y = "Codigo"), munisGT, by.x ="COD_MUNI", by.y="COD_MUNI__" ) 

hist(log(pobrezaGT11$IncidPobrezaExt))

fit_TBPobr = lm(formula = log(TBCases/Poblacion) ~ log(IncidPobrezaExt), data = pobrData)
summary(fit_TBPobr) 

grid.arrange(
    ggplot(data = pobrData, aes(y=log(TBCases/Poblacion), x=IncidPobrezaT)) + geom_point() + geom_smooth(method=lm)  + labs(title="Total Poverty Incidence vrs TB Cases per municipality", y="[Log] TB incidence, cases per 1,000 persons per 2 years", x="Total poverty incidence"),
    ggplot(data = pobrData, aes(y=log(TBCases/Poblacion), x=IncidPobrezaExt)) + geom_point() + geom_smooth(method=lm)  + labs(title="Extreme Poverty Incidence vrs TB Cases per municipality", y="[Log] TB incidence, cases per 1,000 persons per 2 years", x="Extreme poverty incidence"),
    ggplot(data = pobrData, aes(y=log(TBCases/Poblacion), x=Brecha_FGT1)) + geom_point() + geom_smooth(method=lm)  + labs(title="Brecha vrs TB Cases per municipality", y="[Log] TB incidence, cases per 1,000 persons per 2 years", x="Brecha"),
    ggplot(data = pobrData, aes(y=log(TBCases/Poblacion), x=GINI)) + geom_point() + geom_smooth(method=lm)  + labs(title="GINI vrs TB Cases per municipality", y="[Log] TB incidence, cases per 1,000 persons per 2 years", x="GINI"),
    ncol=2, nrow=2
) 

if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2014-2015-Poverty Vars vs TBIncidence by Muni.png", height=8, width=8)

# ----Monthly counts of deaths--------------------------------------------
# defsData is loaded in ../Guatemala_load_outcomes_data.R
defsData = loadDeathsData()
tbDeaths = NULL
for (year in seq(2009, 2015, 1)) {
    temp = defsData[[year]][(CaudefPRE %in% c("A15", "A16", "A17", "A18", "A19","B90")) | 
                                (Caudef %in% c("A301", "A302", "J65X", "K230", "K673", "M011",
                                               "N330", "M490", "M900", "N741", "O980", "K930",
                                               "P370", "Z030", "Z111", "Z201", "Z232")), 
                            .(conteo = .N), 
                            by = .(date = paste0(year, "-", Mesocu, "-01")) ]
    if (is.null(tbDeaths)) {
        tbDeaths = temp
    }
    else { 
        tbDeaths = rbind(tbDeaths, temp)
    }
}  

ggplot(data = tbDeaths, aes(x= as.Date(date), y = conteo)) + geom_line() + labs(title="TB deaths in Gt from 2009 to 2016", y="Cases per month", x="Time")
if (saveGraphs) 
    ggsave(paste0(dataPath, "Graficas/GT_TB_Deaths_TS 2009-2015.png"), height=8, width=8)


tbPrivHospI = NULL
for (year in seq(2009, 2015, 1)) {
    temp = privHospIData[[year]][(CAUFINPRE %in% c("A15", "A16", "A17", "A18", "A19","B90")) | (CAUFIN %in% c("A301", "A302", "J65X", "K230", "K673", "M011", "N330", "M490", "M900", "N741", "O980", "K930", "P370", "Z030", "Z111", "Z201", "Z232")),  .(conteo = .N), 
                                 by = .(date = paste0(year, "-", MES, "-01")) ]
    if (is.null(tbDeaths)) {
        tbPrivHospI = temp
    }
    else { 
        tbPrivHospI = rbind(tbPrivHospI, temp)
    }
}  

ggplot(data = tbPrivHospI, aes(x= as.Date(date), y = conteo)) + geom_line()  + labs(title="TB internal private hospital services in Gt from 2009 to 2016", y="Cases per month", x="Time")
if (saveGraphs) 
    ggsave(paste0(dataPath, "Graficas/GT_TB_PrivHospIntern_TS 2009-2015.png"), height=8, width=8)