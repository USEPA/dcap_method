################################################################################

# TO RUN THIS PORTION OF SCRIPT, Steps 4 - 6 MUST FIRST BE LOADED.

# THE CODE IN THIS FILE EXAMINES THE UNCERTAINTY DECOMPOSITION - THE EFFECT OF
# VARIOUS GSDS IN THE COMPUTATION OF THE CTVs. IN PARTICULAR, WE CALCULATE THE
# EFFECT BY SETTING CERTAIN GSDs TO 1, WHICH ENABLES US TO DETERMINE CTV HAVE WE
# NOT INCORPORATED THE UNCERTAINTY FROM THE ASSOCIATED SOURCE(S).
# NOTE THAT THIS ANALYSIS IS OUTSIDE THE DERIVATION OF THE CTV (AND RATHER AN
# INVESTIGATION ON THE CHARACTERISTICS OF DCAP)

################################################################################

# Initiate timer for this code
tic(msg = "Timer for Uncertainty Decomposition.R")


#------------------------------------------------------------------------------#

# Set boolean variables to include all GSD calculation (in steps 1 - 6)
default_ECUA_boolean()


#------------------------------------------------------------------------------#

# Store original parameter values from the main analysis
p.calib.orig <- p.calib
GSD_disc.orig <- GSD_disc
dat.4.all.orig <- dat.4.all


# A data frame to store uncertainty decomposition (UD) information
dat.4.all.UD <- NULL


for (i in 1:12) {
  print(i)
  
  # For each iteration, change boolean values
  no.GSD.CF1 <- if (i == 1 | i == 11) {
    TRUE
  } else {
    FALSE
  } # Boolean - if TRUE, GSD.CF1 is set to 1
  no.GSD.CF2 <- if (i == 2 | i == 12) {
    TRUE
  } else {
    FALSE
  } # Boolean - if TRUE, GSD.CF2 is set to 1
  no.GSD.CF3 <- if (i == 3 | i == 11) {
    TRUE
  } else {
    FALSE
  } # Boolean - if TRUE, GSD.CF3 is set to 1
  no.GSD.CF4 <- if (i == 4 | i == 12) {
    TRUE
  } else {
    FALSE
  } # Boolean - if TRUE, GSD.CF41 and GSD.CF42 are set to 1
  no.GSD.CF5 <- if ((i == 5) | (i == 11)) {
    TRUE
  } else {
    FALSE
  } # Boolean - if TRUE, GSD.CF5 is set to 1
  no.pcalib <- if ((i == 6) | (i == 8)) {
    TRUE
  } else {
    FALSE
  } # Boolean - if TRUE, GSD_p.calib is set to 1
  no.disc <- if ((i == 7) | (i == 8) | (i == 12)) {
    TRUE
  } else {
    FALSE
  } # Boolean - if TRUE, GSD_disc is set to 1
  no.UFH <- if ((i == 9) | (i == 11)) {
    TRUE
  } else {
    FALSE
  } # Boolean - if TRUE, UFH is set to 1
  no.UFD <- if ((i == 10) | (i == 11)) {
    TRUE
  } else {
    FALSE
  } # Boolean - if TRUE, UFD is set to 1

  
  source(here::here("Code/Step 1.R")) # Perform DRSV-to-eBMD.HED conversion

  # These values are called instead of running Steps 2 - 3.R
  p.calib <- p.calib.orig
  GSD_disc <- GSD_disc.orig

  # Perform Steps 4-6 with modified GSDs
  source(here::here("Code/Steps 4 - 6.R")) # CTV calculation


  # Store UD information
  dat.4.all.UD[[i]] <- dat.4.all
}


# Check to ensure chemicals are in the same rows. These should all be zero
sum(dat.4.all.orig$Grouped_DTXSID != dat.4.all.UD[[1]]$Grouped_DTXSID)
sum(dat.4.all.orig$Grouped_DTXSID != dat.4.all.UD[[2]]$Grouped_DTXSID)
sum(dat.4.all.orig$Grouped_DTXSID != dat.4.all.UD[[3]]$Grouped_DTXSID)
sum(dat.4.all.orig$Grouped_DTXSID != dat.4.all.UD[[4]]$Grouped_DTXSID)
sum(dat.4.all.orig$Grouped_DTXSID != dat.4.all.UD[[5]]$Grouped_DTXSID)
sum(dat.4.all.orig$Grouped_DTXSID != dat.4.all.UD[[6]]$Grouped_DTXSID)
sum(dat.4.all.orig$Grouped_DTXSID != dat.4.all.UD[[7]]$Grouped_DTXSID)
sum(dat.4.all.orig$Grouped_DTXSID != dat.4.all.UD[[8]]$Grouped_DTXSID)
sum(dat.4.all.orig$Grouped_DTXSID != dat.4.all.UD[[9]]$Grouped_DTXSID)
sum(dat.4.all.orig$Grouped_DTXSID != dat.4.all.UD[[10]]$Grouped_DTXSID)
sum(dat.4.all.orig$Grouped_DTXSID != dat.4.all.UD[[11]]$Grouped_DTXSID)


# Convert the UD information as tibble
UD <- tibble(
  "Grouped_DTXSID" = dat.4.all.orig$Grouped_DTXSID,
  "p.calib.eBMD" = dat.4.all.orig$p.calib.eBMD,
  "UF.DCAP.orig" = p.calib.eBMD / dat.4.all.orig$CTV,
  "UF.DCAP.GSD.CF1" = p.calib.eBMD / dat.4.all.UD[[1]]$CTV,
  "UF.DCAP.GSD.CF2" = p.calib.eBMD / dat.4.all.UD[[2]]$CTV,
  "UF.DCAP.GSD.CF3" = p.calib.eBMD / dat.4.all.UD[[3]]$CTV,
  "UF.DCAP.GSD.CF4" = p.calib.eBMD / dat.4.all.UD[[4]]$CTV,
  "UF.DCAP.GSD.CF5" = p.calib.eBMD / dat.4.all.UD[[5]]$CTV,
  "UF.DCAP.dist" = p.calib.eBMD / dat.4.all.UD[[6]]$CTV,
  "UF.DCAP.disc" = p.calib.eBMD / dat.4.all.UD[[7]]$CTV,
  "UF.DCAP.comp" = p.calib.eBMD / dat.4.all.UD[[8]]$CTV,
  "UF.DCAP.UFH" = p.calib.eBMD / dat.4.all.UD[[9]]$CTV,
  "UF.DCAP.UFD" = p.calib.eBMD / dat.4.all.UD[[10]]$CTV,
  "UF.DCAP.trad" = p.calib.eBMD / dat.4.all.UD[[11]]$CTV,
  "UF.DCAP.nontrad" = p.calib.eBMD / dat.4.all.UD[[12]]$CTV
)


values.all <- NULL # UF.DCAP (arithmetic scale)
p.all <- NULL # Percentage reduction in UF.DCAP (in log10-scale)
Grouped_DTXSID.all <- NULL
parameters.all <- NULL


# Group names for UD
parameters <- c(
  "None",
  "GSD.CF1",
  "GSD.CF2",
  "GSD.CF3",
  "GSD.CF4",
  "GSD.CF5",
  "dist",
  "disc",
  "comp",
  "UFH",
  "UFD",
  "trad",
  "nontrad"
)

# Categorizing groups used for Figure 15
groupings <- c(
  1,
  2,
  2,
  2,
  2,
  2,
  3,
  3,
  4,
  4,
  4,
  5,
  5
)


parameters.all <- rep(parameters, dim(UD)[1])
grouping.all <- rep(groupings, dim(UD)[1])
for (i in 1:dim(UD)[1]) {
  Grouped_DTXSID.all <- c(Grouped_DTXSID.all, rep(UD[i, 1], 13))
  values.i <- as.numeric(UD[i, 3:15])
  values.all <- c(values.all, values.i)
  p.i <- (1 - log10(values.i) / log10(values.i[1])) * 100
  p.all <- c(p.all, p.i)
}

dat.UD <- tibble(
  "Grouped_DTXSID" = unlist(Grouped_DTXSID.all),
  "Parameter" = parameters.all,
  "Grouping" = grouping.all,
  "UF.DCAP" = values.all,
  "Percentage.Reduction" = p.all
)


# Set boolean variables to include all GSD calculation (in steps 1 - 6)
default_ECUA_boolean()


#-------------------------------# END OF CODE #-------------------------------#