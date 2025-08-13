################################################################################

# THE CODE IN THIS FILE PERFORMS CROSS-VALIDATION TO OBTAIN GSD.DISC AS DESCRIBED
# IN STEP 5 OF HARRILL ET AL. (2025) (SEE EQ. 13). WHILE STEP IS DESCRIBED AS PART OF STEP 5,
# IT IS PERFORMED AT THE END OF STEPS 2 - 3.R, AS THE INFORMATION REQUIRED TO
# OBTAIN GSD.DISC IS DEFINED IN STEPS 2 - 3.R.


################################################################################

# column names for the data frame cv.all
df_column_names <- c(
  "i",
  "loc.opt.train",
  "SSE.train",
  "SSE.test.train"
)


# Cross-validation. For each i in 1:n.CV=1e4
for (i in 1:n.CV) {
  temp.train.loc <- sort(sample(x = J, size = m, replace = FALSE)) # Draw a random sample of indices
  temp.train <- error2.various[temp.train.loc, ] # Obtain error^2 for training set
  temp.train.error2.sum <- apply(temp.train, 2, sum) # Obtain SSE for training set using all px.vec
  temp.train.loc.opt <- as.numeric(which(temp.train.error2.sum == min(temp.train.error2.sum)))
  temp.train.SSE <- temp.train.error2.sum[temp.train.loc.opt] # Choose minimum SSE
  temp.test.loc <- seq(1:J)[-temp.train.loc] # Choose testing set (not used in training)
  temp.test <- error2.various[temp.test.loc, ] # Obtain error^2 for testing set
  temp.test.error2.sum <- apply(temp.test, 2, sum) # Obtain SSE for training set using all px.vec
  temp.test.SSE.train <- temp.test.error2.sum[temp.train.loc.opt] # Obtain SSE using p.calib based on training set
  
  # Save ith iteration of cross-validation results
  temp.cv <- c(
    i,
    temp.train.loc.opt,
    temp.train.SSE,
    temp.test.SSE.train
  )
  cv.all <- rbind(cv.all, temp.cv) # Bind ith iteration CV results to all previous results
}


rownames(cv.all) <- NULL # Remove rownames for cv.all
colnames(cv.all) <- df_column_names # Set column names
cv.all <- as_tibble(cv.all) # Convert to tibble object


# Predictive RMSD and its distribution
cv.all$RMSD <- sqrt(cv.all$SSE.test.train / (J - m))
cv.all$GSD.disc <- 10^cv.all$RMSD


# 95% CI for p.calib across cross-validation
quantile(cv.all$loc.opt.train, probs = c(0.025, 0.975))


# Summary of GSD.disc values
summary(cv.all$GSD.disc)


# 95% upper confidence bound (UCB = GSD_disc),
# lower confidence bound (LCB) and median GSD values.
GSD_disc <- quantile(cv.all$GSD.disc, probs = 0.95) # This corresponds to Eq.13 of Harrill et al. (2025)
GSD_LCB <- quantile(cv.all$GSD.disc, probs = 0.05)
GSD_median <- median(cv.all$GSD.disc)


# Print out the results
print(paste0("The 5% lower confidence bound on the 10^RMSD distribution: GSD.disc is ", signif(eval(GSD_LCB), digits = 4)))
print(paste0("The median of the 10^RMSD distribution: GSD.disc is ", signif(eval(GSD_median), digits = 4)))
print(paste0("The 95% upper confidence bound on the 10^RMSD distribution: GSD.disc is ", signif(eval(GSD_disc), digits = 4)))


#-------------------------------# END OF CODE #-------------------------------#