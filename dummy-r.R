##test R file##

#=======================================================================
# Program will create a data frame ("AAR") that has age-adjusted rates per 10k for NCSM HCAI health outcomes, along with counts and 95% CI
#  You will be able to customize:
#     - Geographical area (defined by a list of zip codes)
#     - Years (can aggregate across years)
#     - ED visits or Hospitalizations
#     - Optional: Stratification by sex 
#     - Optional: Stratification by age
# 
# Note: To add new years of ESRI data to population file, please see here: "J:\PUBLIC\Env_Health_Tracking\Sophie\Projects\AAR Macro\ESRI population by zip"
#
#Program created by Sophia Horiuchi on 4.21.2023
#=======================================================================


#=======================================================================
# LIBRARIES
#=======================================================================
library(RODBC) #Connect to Tracking server
library(tidyverse)
library(haven) #Reading in SAS data set

#=======================================================================
#STEP 1: Load function to calculate AAR (only need to run 1x, not every time you change specs)
#=======================================================================
calculate_AAR <-function(zip_list, outcome, startyear, endyear, util_location, strat_sex, strat_agecat2, strat_agecat5, strat_age_GE35, strat_age_GE25)
{
  #------------------------------------------------------------------------------
  # Check for errors
  #------------------------------------------------------------------------------
  #Check that outcome variable is correct
  if(!(outcome %in% c("Asthma", "MI", "COPD", "heat"))){
    stop("Your health outcome is not one of the acceptable options: Asthma, MI, COPD, heat. Double check spelling and/or capitalization", call.=FALSE)
  }
  #Check if start years is <= end year
  if(endyear < startyear){
    stop("End year is before start year", call.=FALSE)
  }
  #Check that <=1 age stratification options were chosen
  if(sum(strat_agecat2, strat_agecat5, strat_age_GE35, strat_age_GE25, na.rm=TRUE)>1){
    stop("You have set multiple age stratifications as TRUE.", call.=FALSE)
  }
  #Check that all stratification variables are boolean
  if(!(is.logical(strat_agecat2) & is.logical(strat_agecat5) & is.logical(strat_age_GE35) & is.logical(strat_age_GE25)))
  {
    stop("All age stratifications must be set to TRUE or FALSE", call.=FALSE)
  }
  if(!is.logical(strat_sex))
  {
    stop("Sex stratification option must be set to TRUE or FALSE", call.=FALSE)
  }
  #Check util_location is correct
  if(!(util_location %in% c("ED", "PDD"))){
    stop("Your util_location not one of the acceptable options: ED or PDD. Double check spelling and/or capitalization", call.=FALSE)
  }
  ####New code forpractice##