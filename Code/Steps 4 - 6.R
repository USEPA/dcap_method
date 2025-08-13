################################################################################

# TO RUN THIS CHUNK OF SCRIPT, Steps 2 - 3.R MUST FIRST BE LOADED.

# THE CODE IN THIS FILE WILL PROVIDE ADDITIONAL COLUMNS IN DAT.4 BY
# CALCULATING GSD_p_calib.
#
# GSD_p_calib AND GSD_disc WILL THEN BE USED TO OBTAIN GSD_composite.
# cPOD IS CALCULATED USING log10.p.calib.eBMD.HED AND GSD_composite.
# FINALLY, UF.A AND UF.D ARE APPLIED TO OBTAIN CTV FOR EACH CHEMICAL.


################################################################################

# Initiate timer for this code
tic(msg = "Timer for Steps 4 - 6.R")


#----------# THIS IS THE BEGINNING OF STEP 4 #----------#

# ONCE THE OPTIMAL PERCENTILE p.calib IS OBTAINED,
# CALCULATE HD^50 = p.calib.eBMD FOR EACH CHEMICAL
# AND MERGE TO dat.4

dat.4$p.calib <- p.calib
dat.4$log10.p.calib.eBMD <- qnorm(p = p.calib,
                                  mean = dat.4$average_log10.eBMD,
                                  sd = dat.4$SDV_log10.eBMD)


#-------------# THIS IS THE END OF STEP 4 #-------------#

#----------# THIS IS THE BEGINNING OF STEP 5 #----------#

# CALCULATE GSD_p_calib

# Set seed for replicability of randomization
set.seed(1234)


dtxsid.list <- dat.4$Grouped_DTXSID # DTXSIDs that are included in dat.4
J <- length(dtxsid.list) # A total number of chemicals included in dat.4
GSD_p_calib.vec <- NULL # A vector for GSDs for each chemical in dat.4


# Bootstrap to obtain GSD(p.calib.eBMD.HED)
# This section corresponds to the bootstrap approach described in section
# "QUANTIFYING THE UNCERTAINTY IN THE ESTIMATED p.calib.eBMD.HED" of Harrill et al. (2025)
if (no.pcalib == FALSE) {
  for (j in 1:J) {
    dtxsid.j <- dtxsid.list[j] # set DTXSID for jth chemical
    dat.bs.j <- dat.1[dat.1$Grouped_DTXSID == dtxsid.j, ] # extract studies corresponding to jth chemical
    log10.eBMD.HED1 <- log10(dat.bs.j$eBMD.HED1) # log10(eBMD.HED) values for each study for jth chemical under CM1
    log10.GSD_CM1 <- log10(dat.bs.j$GSD_CM1) # log10(GSD.AH) values for each study for jth chemical under CM1
    log10.eBMD.HED2 <- log10.eBMD.HED1
    log10.GSD_CM2 <- log10.GSD_CM1
    log10.eBMD.HED2[!is.na(dat.bs.j$eBMD.HED2)] <- log10(dat.bs.j$eBMD.HED2[!is.na(dat.bs.j$eBMD.HED2)]) # Overwrite the variable when CM2 exists for chemical j
    log10.GSD_CM2[!is.na(dat.bs.j$eBMD.HED2)] <- log10(dat.bs.j$GSD_CM2[!is.na(dat.bs.j$eBMD.HED2)]) # Overwrite the variable when CM2 exists for chemical j
    bs.log10.eBMD.HED <- c(log10.eBMD.HED1, log10.eBMD.HED2) # Combine log10.eBMD.HED based on CM1 and CM2
    bs.log10.GSD_CF <- c(log10.GSD_CM1, log10.GSD_CM2) # Combine log10.GSD_CF based on CM1 and CM2
    
    # Index with replacement for which DRSV to be used in bootstrap
    iwr <- sample(
      size = dim(dat.bs.j)[1] * n.iter.intra,
      x = 1:dim(dat.bs.j)[1],
      replace = TRUE
    ) 
    
    # Index with replacement for whether CM1 or CM2 to be used in bootstrap
    iwr.CM <- sample(
      size = dim(dat.bs.j)[1] * n.iter.intra,
      x = 0:1,
      replace = TRUE
    ) 
    
    # Obtain bootstrap sample log10(eBMD.HED) values using either CM1 or CM2
    # (If iwr.CM == 0, CM1 is used, otherwise CM2 is used)
    # This portion corresponds to Eq.8 of Harrill et al. (2025)
    bs.log10.eBMD.HED.j <- rnorm(
      n = dim(dat.bs.j)[1] * n.iter.intra,
      mean = bs.log10.eBMD.HED[iwr + iwr.CM*length(log10.eBMD.HED1)],
      sd = bs.log10.GSD_CF[iwr + iwr.CM*length(log10.eBMD.HED1)]
    )
    
    # Convert into matrices
    bs.log10.eBMD.HED.j <-matrix(
      bs.log10.eBMD.HED.j,
      ncol = dim(dat.bs.j)[1],
      byrow = TRUE
    )
    
    bs.m <- apply(bs.log10.eBMD.HED.j, 1, mean) # bootstrap sample means across studies for jth chemical. This corresponds to Eq.9 of Harrill et al. (2025)
    s.j <- apply(bs.log10.eBMD.HED.j, 1,  sd)  # bootstrap sample standard deviations across studies for jth chemical. This corresponds to Eq.10 of Harrill et al. (2025)
    bs.q.j <- qnorm(p.calib, bs.m, s.j) # bootstrap sample quantiles for jth chemical. This corresponds to Eq.11 of Harrill et al. (2025)
    bs.q95 <- quantile(x = bs.q.j, prob = 0.95)
    bs.q05 <- quantile(x = bs.q.j, prob = 0.05)
    GSD_p_calib.j <- as.numeric(10^((bs.q95 - bs.q05) * (1 / (2 * qnorm(0.95))))) # This corresponds to Eq.12 of Harrill et al. (2025). Mathematical equivalence of 10^a^b = 10^(a*b)
    GSD_p_calib.vec <- c(GSD_p_calib.vec, GSD_p_calib.j) # Merge jth bootstrap GSD(p.calib) to all previous values
    
    # Print the progress (every 100 iterations)
    if (j %% 100 == 0) {
      cat(j, "out of", J, "\n")
    }
  }
  
  dat.4$GSD_p_calib <- GSD_p_calib.vec # Add GSD_p.calib to dat.4
} else{
  # For Uncertainty Decomposition
  dat.4$GSD_p_calib <- 1
}


# For Uncertainty Decomposition
if (no.disc == TRUE) {
  GSD_disc <- 1
}

# Add GSD_disc to dat.4
dat.4$GSD_disc <- GSD_disc


# Calculate GSD_comp by combining GSD_p_calib and GSD_disc
dat.4$GSD_comp <- 10^sqrt((log10(dat.4$GSD_p_calib))^2 + (log10(dat.4$GSD_disc))^2) # This corresponds to Eq.14 of Harrill et al. (2025)


#----------##----------##----------#

# CTV Calculation
dat.4$p.calib.eBMD <- 10^dat.4$log10.p.calib.eBMD
# p.calib.eBMD is the probabilistic CTV terminology. Set to pcalib.eBMD.HED.


# Calculate the denominator used for the CTV calculation
dat.4$cPOD.denom <- (dat.4$GSD_comp)^z95

# cPOD is the ratio of pcalib.eBMD.HED and cPOD.denom
dat.4$cPOD <- dat.4$p.calib.eBMD / dat.4$cPOD.denom # This corresponds to Eq.15 of Harrill et al. (2025)


#-------------# THIS IS THE END OF STEP 5 #-------------#

#----------# THIS IS THE BEGINNING OF STEP 6 #----------#

# Default UF.D is set to 10
dat.4$UF.D <- 10
UF.D <- 10

# If process_variable_uncertainty == TRUE, variable UFD is calculated. 
if (process_variable_uncertainty == TRUE) {
  UF.D.threshold <- 1
  dat.4$UF.D.ind <- (
    (dat.4$count_Repeat >= UF.D.threshold)
    * (dat.4$count_RD >= UF.D.threshold)
  )
  dat.4$UF.D <- 10 - 7 * dat.4$UF.D.ind
}

# For uncertainty decomposition
if (no.UFD == TRUE) {
  dat.4$UF.D <- 1
  UF.D <- 1
}

# Default UF.H is set to 10
dat.4$UF.H <- 10
UF.H <- 10 # For inter-individual variability


# Uncertainty decomposition
if (no.UFH == TRUE) {
  dat.4$UF.H <- 1
  UF.H <- 1
}


dat.4$CTV <- dat.4$cPOD / dat.4$UF.D / dat.4$UF.H # CTV for each chemical. This corresponds to Eq.16 of Harrill et al. (2025)
dat.4$ECUA <- 10^(dat.4$log10.p.calib.eBMD) / dat.4$CTV # ECUA for each chemical. This corresponds to Eq. 17 of Harrill et al. (2025)

#-----#

# dat.4.all include CTVs for all chemicals processed in DCAP.
# There will be two more filters applied to obtain the finalized DCAP dataset.
dat.4.all <- dat.4


# Remove chemicals with GSD_dist > threshold
dat.4.GSD_p_calib.10 <- dat.4.all %>%
  filter(GSD_p_calib < GSD.p.calib.cutoff)


# Since DCAP produces CTV for chemicals which do NOT have RfD,
# chemicals included in the calibration step is removed.
dat.4.DCAP <- dat.4.GSD_p_calib.10 %>%
  filter(Grouped_DTXSID %!in% dat.2a$Grouped_DTXSID)

#-------------# THIS IS THE END OF STEP 6 #-------------#

# End timer for Steps 4 - 6.R
toc()

#-------------------------------# END OF CODE #-------------------------------#
