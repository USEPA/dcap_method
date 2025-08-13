################################################################################

# TO RUN THIS CHUNK OF SCRIPT, Step 1 MUST FIRST BE LOADED.

# THE CODE IN THIS FILE EXPLORES AND DETERMINE THE MOST APPROPRIATE PERCENTILE
# CALLED P.CALIB FOR THE CALIBRATED TOXICITY VALUE (CTV). THE CALIBRATION IS
# CONDUCTED BY COMPARING THE LOG10(EBMD.HED) FROM AUTHORITATIVE SOURCES AGAINST
# PERCENTILE OF THE EBMD.HED DISTRIBUTION THAT MINIMIZES THE ROOT MEAN SQUARED
# DIFFERENCE (RMSD).

################################################################################

# Initiate timer for this code
tic(msg = "Timer for Steps 2 - 3.R")


#------------------------------------------------------------------------------#

# PREAMBLE - OBTAIN CHEMICAL-SPECIFIC eBMD.HED DISTRIBUTION INFORMATION
# BY CONSOLIDATING DRSV-SPECIFIC eBMD.HED INFORMATION FROM STEP 1.
# NOTE: WHILE THIS INFORMATION (dat.4) IS THE FOUNDATION IN PERFORMING STEPS 4 -6,
# IT IS ALSO REQUIRED TO PERFORM STEPS 2 - 3.

# Initial processing of eBMD.HED distribution information (for each Grouped_DTXSID)
dat.4a <- dat.1 %>% # dat.4a = initial form of chemical-specific data used in DCAP.
  group_by(Grouped_DTXSID) %>%
  filter(!is.na(Grouped_DTXSID)) %>%
  reframe(casrn = casrn[1],
          name = name[1],
          count_NOAEL = sum(toxval_type_standard == "NOAEL"), # Number of NOAEL records
          count_LOAEL = sum(toxval_type_standard == "LOAEL"), # Number of LOAEL records
          count_BMDL = sum(toxval_type_standard == "BMDL"), # Number of BMDL records
          count_BMD = sum(toxval_type_standard == "BMD"), # Number of BMD records
          count_Repeat = sum(type == "repeat dose"), # Number of repeat-dose records
          count_ST = sum(study_type == "short-term"), # Number of short-term records
          count_SC = sum(study_type == "subchronic"), # Number of subchronic records
          count_C = sum(study_type == "chronic"), # Number of chronic records
          count_RD = sum(type == "reproductive developmental"), # Number of dev/repro records
          count_SG = length(unique(study_group)), # Number of study groups
          count_ATSDR = sum((supersource == "ATSDR") & (calibration_record == 1)), # Number of records from ATSDR used in calibration
          count_HEAST = sum((supersource == "EPA HEAST") & (calibration_record == 1)), # Number of records from EPA HEAST used in calibration
          count_HHTV = sum((supersource == "EPA HHTV") & (calibration_record == 1)), # Number of records from EPA HEAST used in calibration
          count_IRIS = sum((supersource == "EPA IRIS") & (calibration_record == 1)), # Number of records from EPA IRIS used in calibration
          count_PPRTV = sum((supersource == "EPA PPRTV") & (calibration_record == 1)),  # Number of records from EPA PPRTV used in calibration
          count_HC = sum((supersource == "Health Canada") & (calibration_record == 1)), # Number of records from HC used in calibration
          count_auth = (count_ATSDR + count_HEAST + count_HHTV + count_IRIS + count_PPRTV + count_HC),  # Number of records from authoritative sources in calibration
          count_unique = length(unique(eBMD.HED)),  # Number of unique eBMD.HED values per chemical
          count = count_NOAEL + count_LOAEL + count_BMDL + count_BMD,  # Total number of records per chemical
          average_log10.eBMD = mean(log10(eBMD.HED)), # Average log10(eBMD.HED) for chemical; corresponds to Eq.3 of Harrill et al. (2025)
          SDV_log10.eBMD = sd(log10(eBMD.HED)), # Standard deviation about log10(eBMD.HED); corresponds to Eq.4 of Harrill et al. (2025)
  )


# From dat.4a, remove chemicals in which the sigma (inter-study variability) is NA
# Note: the minimum number of DRSV per chemical for dat.4b is 2
dat.4b <- dat.4a %>%
  dplyr::filter(!is.na(SDV_log10.eBMD)) %>%
  dplyr::filter(SDV_log10.eBMD > 0)


# Only retain chemicals with sufficient number of DRSVs (set in Global Parameters.R)
# Note: the minimum number of DRSV per chemical for dat.4b is count_SG.min (= 5 by default)
# Define dat.4 (dataset that is the basis of Step 4-6 of DCAP process) based on dat.4b
dat.4 <- dat.4b %>%
  dplyr::filter(count_SG >= count_SG.min)


#------------------------------------------------------------------------------#

# dat.2a consolidates chemical data for DRSVs that are from authoritative sources
# AND included in the DCAP process (i.e., minimum number of study groups met).
# When there are more than one DRSV from a single source, the minimum
# of such values is used. When there are more than one authoritative bodies that have
# such values, these are averaged in log10-space.

dat.2a <- dat.1 %>% # From dat.1
  filter(supersource %in% auth.sources) %>% # Retain records from authoritative sources only
  filter(Grouped_DTXSID %in% dat.4$Grouped_DTXSID) %>% # Retain records that are included in DCAP
  filter(calibration_record == 1) %>% # Retain records with calibration_record indicator = 1
  group_by(Grouped_DTXSID) %>% # Summarize the data by Grouped_DTXSID
  dplyr::reframe(
    casrn = casrn[1],
    name = name[1],
    count_NOAEL = sum(toxval_type_standard == "NOAEL"),
    count_LOAEL = sum(toxval_type_standard == "LOAEL"),
    count_BMDL = sum(toxval_type_standard == "BMDL"),
    count_BMD = sum(toxval_type_standard == "BMD"),
    count_Repeat = sum(type == "repeat dose"),
    count_ST = sum(type == "short-term"),
    count_SC = sum(type == "subchronic"),
    count_C = sum(type == "chronic"),
    count_RD = sum(type == "reproductive developmental"),
    count_ATSDR = sum(supersource == "ATSDR"),
    count_HEAST = sum(supersource == "EPA HEAST"),
    count_HHTV = sum(supersource == "EPA HHTV"),
    count_IRIS = sum(supersource == "EPA IRIS"),
    count_PPRTV = sum(supersource == "EPA PPRTV"),
    count_HC = sum(supersource == "Health Canada"),
    count_auth = (count_ATSDR + count_HEAST + count_HHTV + count_IRIS + count_PPRTV + count_HC),
    count_SG = length(unique(study_group)),
    count = count_NOAEL + count_LOAEL + count_BMDL + count_BMD,
    
    # Calculate minimum log10(eBMD.HED.auth) values coming from each authoritative source
    min_log10.eBMD_ATSDR = if (count_ATSDR > 0) {
      min(log10(eBMD.HED[which(supersource == "ATSDR")]))
    } else {NA},
    min_log10.eBMD_HEAST = if (count_HEAST > 0) {
      min(log10(eBMD.HED[which(supersource == "EPA HEAST")]))
    } else {NA},
    min_log10.eBMD_HHTV = if (count_HHTV > 0) {
      min(log10(eBMD.HED[which(supersource == "EPA HHTV")]))
    } else {NA},
    min_log10.eBMD_IRIS = if (count_IRIS > 0) {
      min(log10(eBMD.HED[which(supersource == "EPA IRIS")]))
    } else {NA},
    min_log10.eBMD_PPRTV = if (count_PPRTV > 0) {
      min(log10(eBMD.HED[which(supersource == "EPA PPRTV")]))
    } else {NA},
    min_log10.eBMD_HC = if (count_HC > 0) {
      min(log10(eBMD.HED[which(supersource == "Health Canada")]))
    } else {NA},
    
    # Calculate the average of the minimum log10(eBMD.HED.auth) from each authoritative source
    average_log10.eBMD = mean(c(min_log10.eBMD_ATSDR, 
                                min_log10.eBMD_HEAST, 
                                min_log10.eBMD_HHTV,
                                min_log10.eBMD_IRIS,
                                min_log10.eBMD_PPRTV,
                                min_log10.eBMD_HC),
                              na.rm = TRUE
    )
  )



# Exclude all instances where the average_log10.eBMD is NaN (not a number)
dat.2a <- dat.2a %>%
  filter(!is.nan(average_log10.eBMD))


#----------##----------##----------#

# dat.2b is a subset of dat.4 for chemicals with at least one
# study group used by the authoritative sources
dat.2b <- dat.4 %>%
  filter(Grouped_DTXSID %in% dat.2a$Grouped_DTXSID)


# dat.2c is used for the grid search of p.calib
dat.2c <- matrix(NA, ncol = length(px.vec), nrow = dim(dat.2b)[1]) # Create a matrix with all entries = NA
colnames(dat.2c) <- paste0("p", px.vec * 100) # Set column names for dat.2c
RMSD <- NULL # Object to store all RMSD values
count <- 1 # Indicator variable used in the for loop below
J <- length(dat.2b$Grouped_DTXSID) # Number of chemicals included in the calibration process


# Calculate RMSD for each percentile
for (p in px.vec) {
  dat.2c[, count] <- qnorm(p, dat.2b$average_log10.eBMD, dat.2b$SDV_log10.eBMD) # This corresponds to log10 of Eq.5 of Harrill et al. (2025)
  RMSD[count] <- sqrt(sum((dat.2a$average_log10.eBMD - dat.2c[, count])^2) / J) # This corresponds to Eq.6 of Harrill et al. (2025)
  count <- count + 1
}

dat.2c <- as_tibble(dat.2c) # Convert to tibble object


# A dataframe combining px and RMSD values
RMSD.tibble <- tibble(
  "px.vec" = px.vec,
  "RMSD" = RMSD
)


p.calib <- px.vec[which(RMSD == min(RMSD))] # Value of p.calib (in probability)
loc.p.calib <- p.calib * 100 # Value of p.calib (%)


# Print messages summarizing the calibration step.
print(paste0(eval(dim(dat.2b)[1]), " chemicals are used in the calibration process"))
print(paste0("The optimal calibration percentile is ", eval(loc.p.calib), "%"))
print(paste0("10^RMSD without cross-validation is ", signif(10^min(RMSD.tibble$RMSD), digits = 4)))


#------------------------------------------------------------------------------#

# OBTAIN GSD.DISC BY CROSS-VALIDATION (CV)

# Define a matrix of error terms
error.various <- dat.2c - matrix(rep(dat.2a$average_log10.eBMD, length(px.vec)), ncol = length(px.vec))

# Square error.various matrix to obtain 
error2.various <- error.various^2


set.seed(1234) # Set seed for randomization for replicability
m <- ceiling(J * .5) # Number of training data points
n.CV <- 1e4 # Number of iterations for CV
cv.all <- NULL # To store CV results


# Perform cross-validation using 50-50 split
source(here::here("Code/Calculate GSDdisc.R")) # Perform CV to obtain GSD.disc computation


#------------------------------------------------------------------------------#

# End timer for Steps 2 - 3.R
toc()


#-------------------------------# END OF CODE #-------------------------------#