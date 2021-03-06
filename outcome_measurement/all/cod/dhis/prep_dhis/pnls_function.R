pnls_subset = function(x) {

# to run a test without re-running the file merge 
# x = readRDS(paste0(dir, 'pre_prep/merged/', folder,'_', min, '_', max, '.rds' ))
  
#---------------------
# convert factors to characters
x[ , element:=as.character(element)]
x[ , data_set:=as.character(category)]
x[ , data_set:=as.character(data_set)]

# drop unecessary variables
x[ ,c('coordinates', 'country', 'element_eng'):=NULL]  

#---------------------------------------
# drop out duplicate entries

# create a variable that contains only the last word
x[ , last:=word(element, -1)]

# drop anything containing soutien as the last word
# these elements are duplicates or totals of other variables
x = x[!grep('soutien', last)]

# drop out elements that end in 'sex' or 'age'
# these variables are stratified only by sex or age, while others include sex/age
x = x[!grep('sex', last)]
x = x[!grep('age', last)]

#---------------------------------------
# create a data set variable based on element codes
x[ , set:=(unlist(lapply(strsplit(element, "-"), '[', 2)))]

# translate groupings
x[set=='CDV', set:='VCT']
x[set=='IST', set:='STI']
x[set=='PTME', set:='PMTCT']

#---------------------------------------
# create an element that is easier to grep
x[ , element1:=tolower(element)]

# run the function to eliminate diacritical marks
x[ , element1:=fix_diacritics(element1)]

#---------------------------------------
# create new variable names without 'PNLS' or set code

# drop the pnls and set code
# in 'co-infected' elements, there are three hyphens
x[ , element_new1:=(unlist(lapply(strsplit(element, "-"), '[', 3)))]
x[ , element_new2:=(unlist(lapply(strsplit(element, "-"), '[', 4)))]
x[ , element_new3:=(unlist(lapply(strsplit(element, "-"), '[', 5)))]

x[!is.na(element_new2) & !is.na(element_new3), element_new:=paste0(element_new1, "-", element_new2, "-", element_new3)]
x[!is.na(element_new2) & is.na(element_new3), element_new:=paste0(element_new1, "-", element_new2)]
x[is.na(element_new2), element_new:=element_new1]

# drop out the variables used to create new elements
x[ , c( 'last', 'element1','element_new1', 'element_new2', 'element_new3', 'element'):=NULL]
setnames(x, 'element_new', 'element')

#---------------------------------------
# export the abbreviated elements for translation

# to do this on the cluster, you must export as an RDS, then use local code to save
elements = x[ ,.(element = unique(element)), by=.(element_id)]
saveRDS(elements, paste0(dir, 'meta_data/translate/pnls_elements.rds'))

#---------------------
# xlsx files do not work well on the clsuter
# code to run offline
# to_translate = readRDS(elements, paste0(dir, 'meta_data/translate/pnls_elements.rds'))
# write.xlsx(paste0(paste0(dir,'meta_data/translate/pnls_elements_to_translate.xlsx' )))

# translate using onlinedoctranslator.com and save as file path below
#---------------------

# import the translated elements
new_elements = read.xlsx(paste0(paste0(dir,
        'meta_data/translate/pnls_elements_translations.xlsx' )))

# reset the variable name for the merge and merge on element id
setnames(new_elements, 'element', 'element_eng')
x = merge(x, new_elements, by='element_id', all.x=T )

#---------------------------------------
return(x)

}

#---------------------------------------