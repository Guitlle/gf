# ----------------------------------------------
# AUTHOR: Audrey Batzel and Emily Linebarger
# PURPOSE: Master file for preparing impact evaluation dataset. 
# DATE: Last updated January 2019. 
# 
# INSTRUCTIONS: The current working directory should be the root of this repo (set manually by user)
# ----------------------------------------------

#-----------------------------------------
# TO-DO: 


#-----------------------------------------

# ---------------------------------------
# Install packages and set up R  
# ---------------------------------------

# clear memory
rm(list=ls())

# run setup code (load file paths and packages)
source('./impact_evaluation/_common/set_up_r.r')


# ---------------------------------------
# Set boolean switches
# ---------------------------------------
rerun_inputs <- FALSE 
rerun_outputs <- FALSE
rerun_merge <- FALSE
rerun_explore <- FALSE
rerun_analysis <- FALSE
rerun_post <- FALSE

# ---------------------------------------
# Read in common files 
# ---------------------------------------

drc_mal_map <- read_excel(indicatorMapFile)
setDT(drc_mal_map)


# ---------------------------------------
# Prep resource tracking data  
# ---------------------------------------
if(rerun_inputs == TRUE){
  source(paste0(code_dir, '2a_prep_resource_tracking.r'))
}

# ---------------------------------------
# Prep activities and outputs data 
# ---------------------------------------
if(rerun_inputs == TRUE){
  source(paste0(code_dir, '2b_prep_activities_outputs.r'))
}

# ---------------------------------------
# Merge datasets together 
# ---------------------------------------
# file produced by 2b_prep_activities_outputs 
source(paste0(code_dir, '3_merge_data.r'))

# ---------------------------------------
# Validate data (EKL need to add this step)
# ---------------------------------------

# ---------------------------------------
# Run analysis 
# ---------------------------------------

#source(paste0(code_dir, '4_analysis.r'))


print('Master script completed. Outputs saved here: outputFile3')

