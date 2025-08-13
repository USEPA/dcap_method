################################################################################

# THIS CODE (GLOBAL PARAMETERS.R) DEFINES OBJECTS AND PARAMETERS REQUIRED AT
# VARIOUS STEPS IN DCAP

################################################################################

# Objects used in the overall DCAP

dir <- here::here("Data/") # Set the directory to import data from
count_SG.min <- 5 # Minimum number of study groups for a chemical to be included in DCAP
GSD.p.calib.cutoff <- 10 # Value in which if GSD(p.calib.eBMD.HED) is greater, the chemical is excluded from DCAP
n.iter.intra <- 1e5 # Number of iteration for bootstrapping (to obtain GSD.p.calib)
Compute_various_ECUA <- TRUE # Boolean - if TRUE, various Effective Composite Uncertainty Adjustment (ECUA) calculation by setting different GSDs to 1 is performed
z95 <- qnorm(0.95) # quantile associated with 95th percentile of the standard normal distribution
UF.H <- 10 # Default uncertainty factor for inter-individual variability
UF.D <- 10 # Default uncertainty factor for database uncertainty

# Note: The user may choose to enable variable UFD in the calculation of CTV.
# If TRUE, variable UFD is used (the default is FALSE, as per main analysis).
# The choice of constant and variable UFD only affects step 6, and therefore
# the user does not need to repeat steps 1 - 3.
process_variable_uncertainty <- FALSE # Boolean - if TRUE variable UFD is used to derive CTVs


#------------------------------------------------------------------------------#

# FOR STEP 1.R 

# For allometric scaling computation in Step 1.R

BW.human <- 80 # Average body weight (humans, in kg) based on EPA guidance
BW.rat <- 0.25 # Average body weight (rats, in kg)
BW.mouse <- 0.025 # Average body weight (mouse, in kg)
BW.rabbit <- 2 # Average body weight (rabbit, in kg)
BW.dog <- 15 # Average body weight (dog, in kg)
exp.ASCF <- 0.3 # Exponent for allometric scaling (for CF) based on WHO IPCS (2018)
exp.ASU <- 0.04 # Exponent for allometric scaling (for uncertainty about CF)


# The following CF2s correspond to Eq.2 of Harrill et al. (2025) for various animals

# Conversion factors for various animals (CF2s) that is used to convert
# animal-based DRSVs into human equivalent dose. See WHO IPCS (2018) Eq. 4-2
CF2.rat <- (BW.human / BW.rat)^exp.ASCF
CF2.mouse <- (BW.human / BW.mouse)^exp.ASCF
CF2.rabbit <- (BW.human / BW.rabbit)^exp.ASCF
CF2.dog <- (BW.human / BW.dog)^exp.ASCF

# Uncertainty about conversion factors for various animals (CF2s) that is used
# to convert animal-based DRSVs into human equivalent dose.
# See WHO IPCS (2018) Eq. 4-2
P95.P50.CF2.rat <- (BW.human / BW.rat)^exp.ASU
P95.P50.CF2.mouse <- (BW.human / BW.mouse)^exp.ASU
P95.P50.CF2.rabbit <- (BW.human / BW.rabbit)^exp.ASU
P95.P50.CF2.dog <- (BW.human / BW.dog)^exp.ASU

#------------------------------------------------------------------------------#

# FOR STEPS 2 - 3.R

# List of sources that are considered from "authoritative source" in Steps 2 - 3.R
auth.sources <- c("ATSDR",
                  "EPA HEAST",
                  "EPA HHTV",
                  "EPA IRIS",
                  "EPA PPRTV",
                  "Health Canada")


px.vec <- (1:5000) / 10000 # A vector of percentiles to determine p.calib (with 0.01% increment)


#-------------------------------# END OF CODE #-------------------------------#