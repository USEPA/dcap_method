################################################################################

# THIS IS THE MAIN SCRIPT THAT RUNS DCAP PROCESS FOR THE ENTIRE DATABASE

################################################################################

#----------#
# PREAMBLE #
#----------#

# IN PREAMBLE,
# 1. REQUIRED PACKAGES ARE LOADED
# 2. DIRECTORY IS SET
# 3. GLOBAL PARAMETERS ARE SET
# 4. REQUIRED FUNCTIONS ARE LOADED
# 5. TOXVALDB IS ACCESSED


# 1. REQUIRED PACKAGES ARE LOADED
library(openxlsx)
library(stringr)
library(dplyr)
library(MASS)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(tictoc)
library(here)


#----------#
# 2. SETTING WORKING DIRECTORY [R project relative path - no updates required]
wd <- here::here() # when running the script while in an R project (standard approach)

###########
## NOTE: If a users prefers not to use the 'here' package, then the following code
##       should be uncommented and executed.  Furthermore, it is necessary that
##       the 'here::here' statements throughout the code are removed
##       at 'source' and 'saveWorkbook' calls.
##       
# 2. SETTING WORKING DIRECTORY [THIS PATH IS SPECIFIC TO THE USER AND MUST BE MODIFIED]
# wd <- "C:/Users/<user_path_to_DCAP_methods_code>"
# setwd(wd)
###########
#----------#

# 3. LOAD GLOBAL PARAMETERS
source(here::here("Code/Global Parameters.R")) # Load required information


#----------#

# 4. ADDITIONAL FUNCTIONS ARE CALLED
source(here::here("Code/Functions for Data Generator.R")) # Load required functions


#----------#

# 5. ACCESSING TOXVALDB
source(here::here("Code/Step 0.R")) # Load ToxvalDB Excel Spreadsheet


#------------------------# THIS IS THE END OF PREAMBLE #-----------------------#

#-----------------# THIS IS THE BEGINNING OF DATA GENERATION  #----------------#

# initiate timer
tic(msg = "Timer for Data Generator.R")


#----------#

# Set boolean variables to include all GSD calculations (in steps 1 - 6)
default_ECUA_boolean()


source(here::here("Code/Step 1.R")) # Performs step 1
source(here::here("Code/Steps 2 - 3.R")) # Performs steps 2 - 3
source(here::here("Code/Steps 4 - 6.R")) # Performs steps 4 - 6


#----------#

# Create workbook and store necessary information generated through steps 1-6
wb <- createWorkbook("DCAP_Output_All.wb")

# Create worksheet to be included in wb
addWorksheet(wb, sheetName = "ToxValDB")
addWorksheet(wb, sheetName = "dat.1")
addWorksheet(wb, sheetName = "dat.2a")
addWorksheet(wb, sheetName = "dat.2b")
addWorksheet(wb, sheetName = "dat.2c")
addWorksheet(wb, sheetName = "RMSD.tibble")
addWorksheet(wb, sheetName = "dat.4.all")
addWorksheet(wb, sheetName = "dat.4.DCAP")

# Add data to the created worksheets
writeData(wb, sheet = "ToxValDB", x = dat.1a)
writeData(wb, sheet = "dat.1", x = dat.1)
writeData(wb, sheet = "dat.2a", x = dat.2a)
writeData(wb, sheet = "dat.2b", x = dat.2b)
writeData(wb, sheet = "dat.2c", x = dat.2c)
writeData(wb, sheet = "RMSD.tibble", x = RMSD.tibble)
writeData(wb, sheet = "dat.4.all", x = dat.4.all)
writeData(wb, sheet = "dat.4.DCAP", x = dat.4.DCAP)


#----------#

# Obtain various ECUA information for uncertainty decomposition
# (i.e., calculate CTV by setting various GSD = 1)
if(Compute_various_ECUA == TRUE){

  source(here::here("Code/Uncertainty Decomposition.R"))
  
  # Store various ECUA information calculated Uncertainty Decomposition.R
  addWorksheet(wb, sheetName = "dat.UD")
  writeData(wb, sheet = "dat.UD", x = dat.UD)
}


#----------#

# Define file name for Excel Spreadsheet
if (process_variable_uncertainty == FALSE) {
  output.filename <- paste0("DCAP_Output_", Sys.Date(), ".xlsx")
} else {
  output.filename <- paste0("DCAP_Output_", Sys.Date(), "_with_variable_UFD.xlsx")
}


# Save Excel Spreadsheet
saveWorkbook(wb, file = paste0(here::here("Output"),"/", output.filename), overwrite = TRUE)


#----------#

# Print out session information
sessionInfo()


# Print out system time
Sys.time()


# End timer for Data Generator.R
toc()


#-------------------------------# END OF CODE #-------------------------------#