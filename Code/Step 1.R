################################################################################

# THIS CODE (STEP 1.R) CONVERTS DRSV FROM TOXVALDB TO EBMD.HED

################################################################################

# Initiate timer for this code
tic(msg = "Timer for Step 1.R")


#------------------------------------------------------------------------------#

# 4 main objectives (parts) of STEP 1.R are:
# 1. Standardization of DRSV information extracted from ToxValDB
# 2. Calculation of conversion factors (CFs) to perform DRSV -> eBMD.HED conversion
# 3. Calculation of uncertainty about these CFs (which will be required in Step 5)
# 4. Perform DRSV -> eBMD.HED conversion based on 2 above.

dat.1 <- dat.1a %>%
  
  # Part 1: STANDARDIZE VARIABLES
  # Part 1.1: If type (reproductive developmental) is in shortened form, write it out 
  dplyr::mutate(
    type = dplyr::case_when(
      type == "repro dev" ~ "reproductive developmental",
      TRUE ~ type
    )
  ) %>%
  
  
  # Part 1.2: Coalesce DTXSID group
  dplyr::mutate(
    Grouped_DTXSID = dplyr::case_when(
      is.na(Grouped_DTXSID) == TRUE | Grouped_DTXSID == "-" ~ dtxsid,
      is.na(Grouped_DTXSID) == FALSE ~ Grouped_DTXSID,
      TRUE ~ Grouped_DTXSID
    )
  ) %>%
  
  # Part 1.3: Standardize toxicity value type (i.e., into NOAEL, LOAEL, BMDL, and BMD)
  dplyr::mutate(
    toxval_type_standard = dplyr::case_when(
      substr(toxval_type, 1, 1) == "N" ~ "NOAEL",
      substr(toxval_type, 1, 1) == "L" ~ "LOAEL",
      substr(toxval_type, 1, 4) == "BMDL" ~ "BMDL",
      ((substr(toxval_type, 1, 3) == "BMD") & (substr(toxval_type, 4, 4) != "L")) ~ "BMD",
      TRUE ~ as.character(NA)
    )
  ) %>%
  
  # Part 1.4: Simplify toxicity value type (i.e., Remove (05), (HED), etc.)
  dplyr::mutate(
    toxval_type_2 = dplyr::case_when(
      substr(toxval_type, 1, 5) == "NOAEL" ~ "NOAEL",
      substr(toxval_type, 1, 5) == "LOAEL" ~ "LOAEL",
      substr(toxval_type, 1, 4) == "NOEL" ~ "NOEL",
      substr(toxval_type, 1, 4) == "LOEL" ~ "LOEL",
      substr(toxval_type, 1, 4) == "BMDL" ~ "BMDL",
      substr(toxval_type, 1, 3) == "NEL" ~ "NEL",
      substr(toxval_type, 1, 3) == "LEL" ~ "LEL",
      ((substr(toxval_type, 1, 3) == "BMD") & (substr(toxval_type, 4, 4) != "L")) ~ "BMD",
      TRUE ~ as.character(NA)
    )
  ) %>%
  
  
  #-------------#
  
  # Part 2: ASSIGN VARIOUS CONVERSION FACTORS (CFs) (see Table 2 of Harrill et al. (2025))
  # Part 2.1: Assign CF1 for duration adjustment
  dplyr::mutate(
    CF1 = dplyr::case_when(
      duration_adjustment == "subchronic" ~ 2,
      duration_adjustment == "short-term" ~ 5,
      duration_adjustment == "chronic" ~ 1,
      duration_adjustment == "no adjustment" ~ 1
    )
  ) %>%
  
  
  # Part 2.2: Assign CF2 for allometric scaling
  dplyr::mutate(
    CF2 = dplyr::case_when(
      is.na(final_model1) ~ NA,
      toxval_numeric_hed == 1 ~ 1,
      common_name == "Rat" ~ CF2.rat,
      common_name == "Mouse" ~ CF2.mouse,
      common_name == "Rabbit" ~ CF2.rabbit,
      common_name == "Dog" ~ CF2.dog,
      common_name == "Human" ~ 1
    )
  ) %>%
  
  
  # Part 2.3: Assign CF3 for effect level adjustment
  dplyr::mutate(
    CF3 = dplyr::case_when(
      toxval_type_standard == "LOAEL" ~ 3,
      TRUE ~ 1
    )
  ) %>%
  
  
  # Part 2.4A: Assign CF4 for conceptual model 1
  dplyr::mutate(
    CF41 = dplyr::case_when(
      is.na(final_model1) ~ NA,
      ((toxval_type_standard == "BMDL") & (final_model1 == "continuous") & (type != "reproductive developmental")) ~ 1/3,
      ((toxval_type_standard == "BMDL") & (final_model1 == "continuous") & (type == "reproductive developmental")) ~ 1/3,
      ((toxval_type_standard == "BMDL") & (final_model1 == "quantal-deterministic")) ~ 1/9,
      ((toxval_type_standard == "BMDL") & (final_model1 == "quantal-stochastic")) ~ 1/3,
      ((toxval_type_standard == "NOAEL") | (toxval_type_standard == "LOAEL")) & (final_model1 == "continuous") & (type != "reproductive developmental") ~ 1/3,
      ((toxval_type_standard == "NOAEL") | (toxval_type_standard == "LOAEL")) & (final_model1 == "continuous") & (type == "reproductive developmental") ~ 1/3,
      ((toxval_type_standard == "NOAEL") | (toxval_type_standard == "LOAEL")) & (final_model1 == "quantal-deterministic") ~ 2/9,
      ((toxval_type_standard == "NOAEL") | (toxval_type_standard == "LOAEL")) & (final_model1 == "quantal-stochastic") ~ 2/3,
      TRUE ~ 1
    )
  ) %>%
  
  # Part 2.4B: Assign CF4 for conceptual model 2
  dplyr::mutate(
    CF42 = dplyr::case_when(
      is.na(final_model1) | is.na(final_model2) ~ NA,
      (toxval_type_standard == "BMDL") & (final_model2 == "continuous") & (type != "reproductive developmental") ~ 1/3,
      (toxval_type_standard == "BMDL") & (final_model2 == "continuous") & (type == "reproductive developmental") ~ 1/3,
      (toxval_type_standard == "BMDL") & (final_model2 == "quantal-deterministic") ~ 1/9,
      (toxval_type_standard == "BMDL") & (final_model2 == "quantal-stochastic") ~ 1/3,
      ((toxval_type_standard == "NOAEL") | (toxval_type_standard == "LOAEL")) & (final_model2 == "continuous") & (type != "reproductive developmental") ~ 1/3,
      ((toxval_type_standard == "NOAEL") | (toxval_type_standard == "LOAEL")) & (final_model2 == "continuous") & (type == "reproductive developmental") ~ 1/3,
      ((toxval_type_standard == "NOAEL") | (toxval_type_standard == "LOAEL")) & (final_model2 == "quantal-deterministic") ~ 2/9,
      ((toxval_type_standard == "NOAEL") | (toxval_type_standard == "LOAEL")) & (final_model2 == "quantal-stochastic") ~ 2/3,
      TRUE ~ 1
    )
  ) %>%
  
  
  # Part 2.5: Assign CF5 for TK/TD adjustment
  dplyr::mutate(CF5 = 1) %>%
  
  
  #-------------#
  
  # Part 3: ASSIGN UNCERTAINTY ABOUT VARIOUS CONVERSION FACTORS (CFs) 
  # (see Table 3 of Harrill et al. (2025))
  # The uncertainty is defined as the ratio of P95/P50.
  
  # Part 3.1: P95.P50.CF1 for duration adjustment
  dplyr::mutate(
    P95.P50.CF1 = if (no.GSD.CF1 == FALSE) {
      dplyr::case_when(
        duration_adjustment == "subchronic" ~ 4,
        duration_adjustment == "short-term" ~ 8,
        duration_adjustment == "chronic" ~ 1,
        duration_adjustment == "no adjustment" ~ 1,
        TRUE ~ 1
      )
    } else {
      dplyr::case_when(
        TRUE ~ 1
      )
    }
  ) %>%
  
  
  # Part 3.2: P95.P50.CF2 for allometric scaling adjustment
  dplyr::mutate(
    P95.P50.CF2 = if (no.GSD.CF2 == FALSE) {
      dplyr::case_when(
        is.na(final_model1) ~ NA,
        toxval_numeric_hed == 1 ~ 1,
        common_name == "Rat" ~ P95.P50.CF2.rat,
        common_name == "Mouse" ~ P95.P50.CF2.mouse,
        common_name == "Rabbit" ~ P95.P50.CF2.rabbit,
        common_name == "Dog" ~ P95.P50.CF2.dog,
        common_name == "Human" ~ 1
      )
    } else {
      dplyr::case_when(
        TRUE ~ 1
      )
    }
  ) %>%
  
  
  # Part 3.3: P95.P50.CF3 for LOAEL -> NOAEL adjustment
  dplyr::mutate(
    P95.P50.CF3 = if (no.GSD.CF3 == FALSE) {
      dplyr::case_when(
        toxval_type_standard == "LOAEL" ~ 3,
        TRUE ~ 1
      )
    } else {
      dplyr::case_when(
        TRUE ~ 1
      )
    }
  ) %>%
  
  
  # Part 3.4A: P95.P50.CF41 for conceptual model 1
  dplyr::mutate(
    P95.P50.CF41 = if (no.GSD.CF4 == FALSE) {
      dplyr::case_when(
        is.na(final_model1) ~ NA,
        toxval_type_standard == "BMD" ~ 1,
        (toxval_type_standard == "BMDL") & (final_model1 == "continuous") & (type != "reproductive developmental") ~ 3,
        (toxval_type_standard == "BMDL") & (final_model1 == "continuous") & (type == "reproductive developmental") ~ 3 ,
        (toxval_type_standard == "BMDL") & (final_model1 == "quantal-deterministic") ~ 1.5,
        (toxval_type_standard == "BMDL") & (final_model1 == "quantal-stochastic") ~ 3,
        ((toxval_type_standard != "NOAEL") | (toxval_type_standard != "LOAEL")) & (final_model1 == "continuous") & (type != "reproductive developmental") ~ 4.7,
        ((toxval_type_standard != "NOAEL") | (toxval_type_standard != "LOAEL")) & (final_model1 == "continuous") & (type == "reproductive developmental") ~ 7.0,
        ((toxval_type_standard != "NOAEL") | (toxval_type_standard != "LOAEL")) & (final_model1 == "quantal-deterministic") ~ 5.0,
        ((toxval_type_standard != "NOAEL") | (toxval_type_standard != "LOAEL")) & final_model1 == "quantal-stochastic" ~ 4.7,

        TRUE ~ 1
      )
    } else {
      dplyr::case_when(
        TRUE ~ 1
      )
    }
  ) %>%
  
  
  # Part 3.4B: P95.P50.CF42 for conceptual model 2
  dplyr::mutate(
    P95.P50.CF42 = if (no.GSD.CF4 == FALSE) {
      dplyr::case_when(
        is.na(final_model1) | is.na(final_model2) ~ NA,
        toxval_type_standard == "BMD" ~ 1,
        (toxval_type_standard == "BMDL") & (final_model2 == "continuous") & (type != "reproductive developmental") ~ 3,
        (toxval_type_standard == "BMDL") & (final_model2 == "continuous") & (type == "reproductive developmental") ~ 3 ,
        (toxval_type_standard == "BMDL") & (final_model2 == "quantal-deterministic") ~ 1.5,
        (toxval_type_standard == "BMDL") & (final_model2 == "quantal-stochastic") ~ 3,
        ((toxval_type_standard != "NOAEL") | (toxval_type_standard != "LOAEL")) & (final_model2 == "continuous") & (type != "reproductive developmental") ~ 4.7,
        ((toxval_type_standard != "NOAEL") | (toxval_type_standard != "LOAEL")) & (final_model2 == "continuous") & (type == "reproductive developmental") ~ 7.0,
        ((toxval_type_standard != "NOAEL") | (toxval_type_standard != "LOAEL")) & (final_model2 == "quantal-deterministic") ~ 5.0,
        ((toxval_type_standard != "NOAEL") | (toxval_type_standard != "LOAEL")) & (final_model2 == "quantal-stochastic") ~ 4.7,

        TRUE ~ 1
      )
    } else {
      dplyr::case_when(
        TRUE ~ 1
      )
    }
  ) %>%
  
  
  # Part 3.5: P95.P50.CF5 for animal to human TK/TD
  dplyr::mutate(
    P95.P50.CF5 = if (no.GSD.CF5 == FALSE) {
      dplyr::case_when(
        common_name == "Human" ~ 1,
        toxval_numeric_hed == 1 ~ 1,
        TRUE ~ 3
      )
    } else {
      dplyr::case_when(
        TRUE ~ 1
      )
    }
  ) %>%
  
  
  #-------------#
  
  # Part 4: DERIVE eBMD.HED VALUES AND THE OVERALL UNCERTAINTY ASSOCIATED WITH
  # DRSV -> eBMD.HED CONVERSION
  
  # Part 4.1: Convert P95.P50.CF1 to P95.P50.CF5 into GSD.CF1 to GSD.CF5 
  # (see Table 3 of Harrill et al. (2025))
  dplyr::mutate(
    GSD.CF1 = P95.P50.CF1^(1/z95), 
    GSD.CF2 = P95.P50.CF2^(1/z95),
    GSD.CF3 = P95.P50.CF3^(1/z95),
    GSD.CF41 = P95.P50.CF41^(1/z95),
    GSD.CF42 = P95.P50.CF42^(1/z95),
    GSD.CF5 = P95.P50.CF5^(1/z95)
  ) %>%
  
  
  # Part 4.2: Calculate overall CF (denominator of Eq. 1 of Harrill et al. (2025))
  dplyr::mutate(
    denom1 = CF1 * CF2 * CF3 * CF41 * CF5,
    denom2 = dplyr::case_when(
      is.na(final_model2) ~ NA,
      TRUE ~ CF1 * CF2 * CF3 * CF42 * CF5
    )
  ) %>%
  
  
  # Part 4.3: Calculate eBMD.HED (using conceptual models 1 and 2)
  # This corresponds to Eq. 1 of Harrill et al. (2025)
  dplyr::mutate(
    eBMD.HED1 = toxval_numeric / denom1,
    eBMD.HED2 = dplyr::case_when(
      is.na(final_model2) ~ NA,
      TRUE ~ toxval_numeric / denom2
    )
  ) %>%
  
  
  # Part 4.4: Obtain eBMD.HED values by taking the average (in log-scale) 
  # of two conceptual models. When conceptual model 2 does not exist, 
  # it is set to eBMD.HED from conceptual model 1
  dplyr::mutate(
    eBMD.HED = dplyr::case_when(
      !(final_model2 %in% c("-", "", as.character(NA))) ~ 10^(0.5 * (log10(eBMD.HED1) + log10(eBMD.HED2))),
      TRUE ~ eBMD.HED1
    ),
    
    eBMD.HED2 = dplyr::case_when(final_model2 %in% c("-", "", as.character(NA)) ~ NA,
                                 TRUE ~ eBMD.HED2
    )
  ) %>%
  
  
  # Part 4.5: Obtain overall GSD associated with DRSV -> eBMD conversions
  # (for conceptual models 1 and 2)
  # Here GSD_CM1 refers to GSD about conversion factors with conceptual model 1,
  # and GSD_CM2 refers to GSD about conversion factors with conceptual model 2.
  # This portion corresponds to Eq.7 of Harrill et al. (2025)
  dplyr::mutate(
    GSD_CM1 = (10^sqrt(log10(GSD.CF1)^2 + log10(GSD.CF2)^2 + log10(GSD.CF3)^2 + log10(GSD.CF41)^2 + log10(GSD.CF5)^2)),
    GSD_CM2 = dplyr::case_when(
      is.na(final_model2) ~ NA,
      TRUE ~ (10^sqrt(log10(GSD.CF1)^2 + log10(GSD.CF2)^2 + log10(GSD.CF3)^2 + log10(GSD.CF42)^2 + log10(GSD.CF5)^2))
    )
  ) %>%
  
  
  # Part 4.6: Remove any rows which result in eBMD.HED == NA
  dplyr::filter(!is.na(eBMD.HED))


#------------------------------------------------------------------------------#

# End timer for Step 1.R
toc()


#-------------------------------# END OF CODE #-------------------------------#