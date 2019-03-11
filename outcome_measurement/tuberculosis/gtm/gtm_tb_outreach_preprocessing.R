# -----------------------------------
#  Outreach data preprocessing

library(data.table)


codePath = "PCE/gf/"

# Requirements:
source(paste0(codePath, "core/GT_load_data.R"), encoding = "UTF-8")
source(paste0(codePath, "core/GT_helper_functions.R"), encoding = "UTF-8")

outreach = read.csv("PCE/Outcome Measurement Data/TUBERCULOSIS/Extramuros2017.csv", encoding = "latin1")

outreach = data.table(outreach)

outreach[,DeptoCode:=NULL]
outreach[,DeptoCode := getDeptoCodeByName(as.character(Department) ), by=1:nrow(outreach)]
outreach[, .(DeptoCode, Department)]
outreach[,MuniCode := getMuniCodeByName(as.character(Distrito), NULL, DeptoCode ), by=1:nrow(outreach)]
outreach[, .(MuniCode, Distrito)]
write.csv(outreach, "PCE/Outcome Measurement Data/TUBERCULOSIS/Extramuros2017.csv")

# ----------------------------
#   Comparing with other municipalities:
notifs = read.csv("PCE/Outcome Measurement Data/TUBERCULOSIS/Notificaciones TB/GTM - TB notifications 2012-Sep2018.csv")

notifs$extraMuros = notifs$COD_MUNI %in% outreach$MuniCode
table(notifs$extraMuros)
notifs = data.table(notifs)
notifs[, sum(.N), by = .(YearMonth, extraMuros)]
ggplot(data = notifs[YearMonth <= 201806, sum(.N), by = .(x = YEAR*100 + ceiling((YearMonth%%100)/3), extraMuros)], 
       aes(factor(x), y=V1, group=extraMuros))+ geom_line(aes(colour=extraMuros))+ theme(axis.text.x = element_text(angle = 90)) + labs(fill="Outreach", x="Month", y="Notifications counts per month", title = "Time series of notifications")
length(unique(outreach$MuniCode))
