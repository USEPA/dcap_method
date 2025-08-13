################################################################################

# TO RUN THIS CHUNK OF SCRIPT, Steps 4 - 6.R (or dat.4.all) MUST FIRST BE LOADED.
# THE CODE IN THIS FILE WILL COMPUTE pCTV VALUES FOR CHEMICALS IN dat.4.

# NOTE 1: WHILE THIS SCRIPT FOLLOWS THE APPROACH DESCRIBED IN AURISANO ET AL. (2023)
# THIS SCRIPT IS NOT INTENDED TO REPLICATE RESULTS PRODUCED BY AURISANO ET AL. (2023)
# NOTE 2: THE COMPUTATION OF PROBABILISTIC CTV IS NOT CURRENTLY UTILIZED IN THE
# DCAP METHOD, AND IT IS NOT DESCRIBED IN HARRILL ET AL. (2025)


################################################################################

# THE FOLLOWING CODE IS SUPPRESSED AS THEY ARE RELATED TO THE COMPUTATION OF THE
# PROBABILISTIC CTV.


# # THIS SECTION PRODUCES 9 DIFFERENT pCTV VALUES
# # 3 LEVELS OF INCIDENCE RATES X 3 LEVELS OF CONFIDENCE
# 
# dat.7.all <- dat.4.all
# 
# 
# # IR = Incidence rates for pCTV calculation
# IR.05 <- 0.05
# IR.01 <- 0.01
# IR.005 <- 0.005
# 
# 
# # z-score associated with 3 confidence levels
# z90 <- qnorm(0.9)
# z95 <- qnorm(0.95)
# z99 <- qnorm(0.99)
# 
# 
# # Define GSDs for interindividual variabitlity in the human equipotent dose distribution (see WHO 2017, Table 4.5)
# log10.GSDHMedian <- 0.324 # log10(P50)
# log10.GSDHGSDU <- 2.152^(1/z95) # log10(P95/P50)^(1/z95)
# 
# 
# # Intermediate step to obtain extrapolation factor below by taking the 
# # upper confidence limit (UCL) and lower confidence limit (LCL)
# UCL.05 <- 10^(qnorm(1-IR.05)*log10.GSDHMedian*log10.GSDHGSDU^(z95))
# LCL.05 <- 10^(qnorm(1-IR.05)*log10.GSDHMedian/log10.GSDHGSDU^(z95))
# UCL.01 <- 10^(qnorm(1-IR.01)*log10.GSDHMedian*log10.GSDHGSDU^(z95))
# LCL.01 <- 10^(qnorm(1-IR.01)*log10.GSDHMedian/log10.GSDHGSDU^(z95))
# UCL.005 <- 10^(qnorm(1-IR.005)*log10.GSDHMedian*log10.GSDHGSDU^(z95))
# LCL.005 <- 10^(qnorm(1-IR.005)*log10.GSDHMedian/log10.GSDHGSDU^(z95))
# 
# 
# # Extrapolation factor (EF) from 50% to I% incidence rate
# EF.H.05 <- sqrt(UCL.05*LCL.05)
# EF.H.01 <- sqrt(UCL.01*LCL.01)
# EF.H.005 <- sqrt(UCL.005*LCL.005)
# 
# 
# # GSD associated with the uncertainty in the estimation of Ith IR
# GSD_IH.05 <- (UCL.05/EF.H.05)^(1/z95) # for lower 5th percentile of most susceptible persons
# GSD_IH.01 <- (UCL.01/EF.H.01)^(1/z95) # for lower 1st percentile of most susceptible persons
# GSD_IH.005 <- (UCL.005/EF.H.005)^(1/z95) # for lower 0.5th percentile of most susceptible persons
# 
# 
# # Obtain HDMI (Human Dose with magnitude M and incidence rate I - see Aurisano et al. 2023)
# dat.7.all$HD05 <- dat.7.all$p.calib.eBMD / EF.H.05 # HDM05
# dat.7.all$HD01 <- dat.7.all$p.calib.eBMD / EF.H.01 # HDM01
# dat.7.all$HD005 <- dat.7.all$p.calib.eBMD / EF.H.005 # HDM005
# 
# 
# # Obtain GSD for the combined uncertainty utilized in DCAP (except for UF.D)
# dat.7.all$GSD_HD05 <- 10^sqrt(log10(dat.7.all$GSD_comp)^2 + log10(GSD_IH.05)^2)
# dat.7.all$GSD_HD01 <- 10^sqrt(log10(dat.7.all$GSD_comp)^2 + log10(GSD_IH.01)^2)
# dat.7.all$GSD_HD005 <- 10^sqrt(log10(dat.7.all$GSD_comp)^2 + log10(GSD_IH.005)^2)
# 
# 
# # Obtain pCTVs by using different percentile for susceptibility and confidence
# dat.7.all$I05C99 <- dat.7.all$HD05 / (dat.7.all$GSD_HD05)^z99 / dat.7.all$UF.D # 5% IR, 99% confidence
# dat.7.all$I05C95 <- dat.7.all$HD05 / (dat.7.all$GSD_HD05)^z95 / dat.7.all$UF.D # 5% IR, 95% confidence
# dat.7.all$I05C90 <- dat.7.all$HD05 / (dat.7.all$GSD_HD05)^z90 / dat.7.all$UF.D # 5% IR, 90% confidence
# dat.7.all$I01C99 <- dat.7.all$HD01 / (dat.7.all$GSD_HD01)^z99 / dat.7.all$UF.D # 1% IR, 99% confidence
# dat.7.all$I01C95 <- dat.7.all$HD01 / (dat.7.all$GSD_HD01)^z95 / dat.7.all$UF.D # 1% IR, 95% confidence
# dat.7.all$I01C90 <- dat.7.all$HD01 / (dat.7.all$GSD_HD01)^z90 / dat.7.all$UF.D # 1% IR, 90% confidence
# dat.7.all$I005C99 <- dat.7.all$HD005 / (dat.7.all$GSD_HD005)^z99 / dat.7.all$UF.D # 0.5% IR, 99% confidence
# dat.7.all$I005C95 <- dat.7.all$HD005 / (dat.7.all$GSD_HD005)^z95 / dat.7.all$UF.D # 0.5% IR, 95% confidence
# dat.7.all$I005C90 <- dat.7.all$HD005 / (dat.7.all$GSD_HD005)^z90 / dat.7.all$UF.D # 0.5% IR, 90% confidence
# 
# 
# # Obtain a subset of dat.7 that only contains DCAP eligible chemicals
# dat.7.DCAP <- dat.7.all %>%
#   filter(Grouped_DTXSID %in% dat.4.DCAP$Grouped_DTXSID)
#       
# 
# # Create workbook and store necessary information generated in Step 7
# wb <- createWorkbook("DCAP_Output_pCTV.wb")
# addWorksheet(wb, sheetName = "dat.7.all")
# addWorksheet(wb, sheetName = "dat.7.DCAP")
# writeData(wb, sheet = "dat.7.all", x = dat.7.all)
# writeData(wb, sheet = "dat.7.DCAP", x = dat.7.DCAP)
# 
# 
# # Save Excel Spreadsheet
# output.filename <- paste0("DCAP_Output_pCTV_", Sys.Date(), ".xlsx")
# saveWorkbook(wb, file = paste0(here::here("Output"),"/",output.filename), overwrite = TRUE)
# 
#
# #-------------------------------# END OF CODE #-------------------------------#

