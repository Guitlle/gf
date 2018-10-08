# -----------------------------------
# David Phillips
# 
# 10/2/2018
# Quick graphs of stockouts from DHIS
# -----------------------------------


# ------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(scales)
library(RColorBrewer)
# ------------------


# ------------------------------------------------------------------------
# Files and directories

# switch for the cluster
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')

# data directory
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

# codebook file
codebookFile = paste0(dir, 'catalogues/data_elements_cod.csv')

# input file where the given variable can be found
inFile = paste0(dir, 'prepped/sigl_drc_01_2015_07_2018_prepped.rds')

# output file
outFile = paste0(dir, '../visualizations/snis_stockouts.pdf')
# ------------------------------------------------------------------------


# --------------------------------------------------
# Load/prep data

# load codebook
codebook = fread(codebookFile)

# identify stockout variables
stockoutCodebook = codebook[grepl('stock', element) & 
					grepl('out', element) & type!='']
variables = stockoutCodebook[order(-type, -drug, element)]$element

# load LMIS data
data = readRDS(inFile)

# subset to the specified variable(s) post 2016
data = data[element_eng %in% variables & year>=2017]

# drop outliers (since we're only assessing the mean, this is a reasonable thing to do)
data[, outlier:=value>31]
pctOutliers = data[, .(pct=mean(outlier)), by=element_eng]
data[value>31, value:=NA]

# exclude facilities that seemingly never have had any drugs 
data[, fullSO:=0]
data[month(date)==2 & value==28, fullSO:=1]
data[month(date) %in% c(4, 6, 9, 11) & value==30, fullSO:=1]
data[month(date) %in% c(1, 3, 5, 7, 8, 10, 12) & value==31, fullSO:=1]
data[value==0, fullSO:=1] # a few zeroes is ok because that's probably just a data quality issue...
data[, is_zero:= value==0]
data[, pct_zero:=mean(is_zero), by=c('element_eng','org_unit_id')]
data[, min:=min(fullSO), by=c('element_eng','org_unit_id')]
data[, never_stock:=(pct_zero<.34 & min==1)]
neverStock = unique(data[, c('org_unit_id','element_eng','never_stock'), with=FALSE])
neverStock = neverStock[, .(pct=mean(never_stock)), by=element_eng]
data = data[never_stock==FALSE] # drop facility-variables that never didn't have a stockout, including some that had a few zeroes mixed in there ("a few"=33%)
# --------------------------------------------------


# -------------------------------------------------------------------------
# Aggregate

# aggregate all other DPS's
agg = copy(data)
agg[mtk=='No', dps:='All Other Provinces'] 

# compute monthly average value by dps
byVars = c('dps','date','element_eng')
agg = agg[, .(value=mean(value, na.rm=TRUE)), by=byVars]

# compute percentage of HZ's and percentage of facilities with any stockout per DPS
data[, hf_any_stockout:= value>0]
data[, hz_max:=max(value,na.rm=TRUE), by=c('element_eng','health_zone','date')]
data[, hz_any_stockout:= hz_max>0]
pct = copy(data)
pct[!dps %in% c('Equateur','Kwilu','Kinshasa','Maniema','Tshopo'), dps:='All Other Provinces']
pct = pct[, .(hf_pct=mean(hf_any_stockout, na.rm=TRUE), 
			hz_pct=mean(hz_any_stockout, na.rm=TRUE)), by=byVars]
			
# melt the two percentages long
pct = melt(pct, id.vars=byVars)
pct[variable=='hf_pct', variable:='facilities']
pct[variable=='hz_pct', variable:='health zones']
# -------------------------------------------------------------------------


# ---------------------------------------------
# Set up to graph
c = brewer.pal(12, 'Paired')
colors = c('All Other Provinces'=c[1], 'Kinshasa'=c[4], 
		'Maniema'=c[8], 'Tshopo'=c[10], 'Equateur'=c[6], 
		'Kwilu'=c[2])
# ---------------------------------------------


# ------------------------------------------------------
# Graph mean days by element and dps
meanPlots = list()
for(i in seq(length(variables))) { 
	pctO = round(pctOutliers[element_eng==variables[i]]$pct*100,1)
	pctN = round(neverStock[element_eng==variables[i]]$pct*100,1)
	meanPlots[[i]] = ggplot(agg[element_eng==variables[i]], aes(y=value, x=date, color=dps)) +
		geom_line() + 
		geom_point() + 
		scale_x_date(labels=date_format("%b-%Y"), 
			date_breaks ="3 month") + 
			scale_color_manual(values=colors) + 
		labs(title=variables[i], y='Average days of stock-out per facility', 
			x='', color='', 
			caption=paste0('Reported stockouts greater than 31 days excluded (', pctO, '% of facility-months)\n
			Facilities which seem to never stock this commodity (', pctN, '% of facilities) also excluded')) + 
		theme_bw()
}
# ------------------------------------------------------


# ------------------------------------------------------
# Graph percent of HZ's and HF's with any stockout by element and dps
pctPlots = list()
i=1
for(j in seq(length(variables))) { 
	for(v in c('facilities','health zones')) {
		pctO = round(pctOutliers[element_eng==variables[j]]$pct*100,1)
		pctN = round(neverStock[element_eng==variables[j]]$pct*100,1)
		pctPlots[[i]] = ggplot(pct[element_eng==variables[j] & variable==v], 
				aes(y=value*100, x=date, color=dps)) +
			geom_line(size=1) + 
			geom_point(size=2) + 
			scale_x_date(labels=date_format("%b-%Y"), 
				date_breaks ="3 month") + 
				scale_color_manual(values=colors) + 
			labs(title=variables[j], y=paste('Percentage of', v, 'with any stockouts'), 
				x='', color='', 
				caption=paste0('Reported stockouts greater than 31 days excluded (', pctO, '% of facility-months)\n
				Facilities which seem to never stock this commodity (', pctN, '% of facilities) also excluded')) + 
			theme_bw()
		i=i+1
	}
}
# ------------------------------------------------------


# --------------------------------------------------
# Save
pdf(outFile, height=5.5, width=9)
for(i in seq(length(meanPlots))) print(meanPlots[[i]])
for(i in seq(length(pctPlots))) print(pctPlots[[i]])
dev.off()
# --------------------------------------------------

