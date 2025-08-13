################################################################################

# THIS CODE (STEP 0.R) IMPORTS THE EXCEL SPREADSHEET THAT CONTAINS
# THE TOXVALDB INFORMATION

################################################################################

file.ToxValDB <- paste0(dir,"/","ToxValDB for BMDh res_toxval_v96_1 POD filtered_20250417.xlsx") # specify path to the data
dat.1a <- read.xlsx(file.ToxValDB) # load data into R. dat.1a = initial form of dataset used in Step 1 of DCAP.


#-------------------------------# END OF CODE #-------------------------------#
