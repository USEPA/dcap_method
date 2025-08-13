################################################################################

# THIS FILE CONTAINS FUNCTIONS WRITTEN BY RISK SCIENCES INTERNATIONAL
# TO PERFORM DCAP ASSESSMENT (for Data Generator.R).

################################################################################

# FUNCTION THAT IS USED TO FILTER OUT CHEMICALS THAT ARE IN CERTAIN GROUPS
# This function is used in filter() to exclude chemicals that matches the condition.
`%!in%` = Negate(`%in%`)


#----------##----------##----------#

# A FUNCTION TO SET BOOLEAN VARIABLES FOR ECUA COMPARISON FOR DEFAULT DCAP ASSESSMENT
# Setting each variable to FALSE implies that all applicable GSD calculations are performed.
default_ECUA_boolean <- function(){
  no.GSD.CF1 <<- FALSE # Boolean - if TRUE, UF1 is set to 1
  no.GSD.CF2 <<- FALSE # Boolean - if TRUE, UF2 is set to 1
  no.GSD.CF3 <<- FALSE # Boolean - if TRUE, UF3 is set to 1
  no.GSD.CF4 <<- FALSE # Boolean - if TRUE, UF41 and UF42 are set to 1
  no.GSD.CF5 <<- FALSE # Boolean - if TRUE, UF5 is set to 1
  no.pcalib <<- FALSE # Boolean - if TRUE, GSD_pcalib is set to 1
  no.disc <<- FALSE # Boolean - if TRUE, GSD_disc is set to 1
  no.UFH <<- FALSE # Boolean - if TRUE, UFH is set to 1
  no.UFD <<- FALSE # Boolean - if TRUE, UFD is set to 1
}


#-------------------------------# END OF CODE #-------------------------------#