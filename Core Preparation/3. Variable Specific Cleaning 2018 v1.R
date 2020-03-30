
# -- 3. Variable-Specific Cleaning 2019 v1 --

library(data.table)

ALL_PREDICT <- fst::read.fst("source_data/R/PREDICT/2019/ALL_PREDICT_Part_2.fst", as.data.table = T)

DATA <- copy(ALL_PREDICT)

# ---- A.   Weight, Height, BMI ----

# Notes 
# Limits -- Height = <120 & > 220; Weight = <30 & >350
# Absolute -- Where the weight or height is negative then a assumption is made that the value is correct and the '-' is a key stroke error
# Height Units -- assumes height values 1.1 - 2.2 are in metres


# Starting placeholders (exisiting values by default)
bmi.vars <- c("pt_en_weight", "pt_en_height", "pt_en_bmi")

DATA[, (bmi.vars) := list(pt_weight, pt_height, pt_bmi)]


#   *** MUST Do in order *** 
# i.  Remove non-values
DATA[, (bmi.vars) := lapply(.SD,
                            function(x)
                              ifelse(x==0, NA, x)), .SDcols=bmi.vars]

# ii.   Turn negative values to absolute
DATA[, (bmi.vars) := lapply(.SD,
                            function(x)
                              ifelse(x<0, abs(x), x)), .SDcols=bmi.vars]

# iii.Height Unit adjustment 
#     Any height values between 1.0 ~ 2.3, multiple by 100 (assuming value is in metres)
DATA[pt_en_height >=1.2 & pt_en_height < 2.2, pt_en_height := pt_en_height * 100]

# At the stage, there are only 2474 records that have heights <110 or >220. Further cleaning will have effect. Can write as missing.
# These records represent ~1500 individuals but they might have repeated measures that include legit information
DATA[pt_en_height <120 | pt_en_height > 220, pt_en_height := NA]


# iv. Weight Range Check: Any Weight < 30 or > 350 <- NA
#     NB: these values are too heavy/light to be weight, and too short/tall to be height 
DATA[pt_en_weight<30 | pt_en_weight>350, pt_en_weight := NA]


# nb. Weight / Height Reversal using W/H Ratio not attempted in 2019. Any efforts made only affected 214 records. 
# library(dplyr)
# 
# DF_NORM <- DATA[!is.na(pt_en_height) & !is.na(pt_en_weight)] %>%
#              sample_n(100000)
# 
# plot(DF_NORM$pt_en_weight, DF_NORM$pt_en_height,
#      xlab="Weight (KG)",
#      ylab="Height (CM)")


# vii. Populate missing weight & height with existing known values
#      nb:  Ensure height & weight have already undergone range-checks 

#     Two-step approach:
#     1. If height is missing, carry forward / carry back from last known value
#     nb - using zoo::na.locf, 224812 records with missing height reduced down to 132268 
DATA[, pt_en_height := zoo::na.locf(pt_en_height, na.rm = FALSE)
        , by = VSIMPLE_INDEX_MASTER]

DATA[, pt_en_height := zoo::na.locf(pt_en_height, fromLast = T)
        , by = VSIMPLE_INDEX_MASTER]

#     2. If weight is missing, carry forward / carry back from last known value
#     nb - using zoo::na.locf, 124968 records with missing weight reduced down to 67267 
DATA[, pt_en_weight := zoo::na.locf(pt_en_weight, na.rm = FALSE)
        , by = VSIMPLE_INDEX_MASTER]

DATA[, pt_en_weight := zoo::na.locf(pt_en_weight, fromLast = T)
        , by = VSIMPLE_INDEX_MASTER]

# viii. Replace pt_BMI
#       nb: 14 records have a BMI even though height and weight are both missing (fine to keep them)
DATA[, pt_en_bmi := as.numeric(round(pt_en_weight / (pt_en_height / 100)^2, 1))]

# At this stage, there are some BMIs with extreme values still. This was a result of extreme weight/heights (typically far too low).
# The main thing is that the extreme outliers tail off.

ALL_PREDICT <- copy(DATA)


# ---- B   Range Check Other Continuous Variables ----

# Remove out-of-range values for selected variables
# NB: Not included are triglycerides, egfr

# Lipids
en.lipids <- c("pt_en_tchdl_ratio", "pt_en_tcl", "pt_en_hdl", "pt_en_ldl") 

ALL_PREDICT[, (en.lipids) := list(ifelse(pt_tchdl_ratio<1.08 | pt_tchdl_ratio>30.1, NA, pt_tchdl_ratio),
                                  ifelse(pt_tcl<1.51 | pt_tcl>35.5, NA, pt_tcl),
                                  ifelse(pt_hdl<0.13 | pt_hdl>5.1, NA, pt_hdl),
                                  ifelse(pt_ldl<0.3 | pt_ldl>11.5, NA, pt_ldl))]

# Blood Pressure
en.bp <- c("pt_en_sbp", "pt_en_dbp", "pt_en_sbp2", "pt_en_dbp2")

ALL_PREDICT[, (en.bp) := list(ifelse(pt_bps<40 | pt_bps>310, NA, pt_bps),
                              ifelse(pt_bpd<20 | pt_bpd>200, NA, pt_bpd),
                              ifelse(pt_bps2<40 | pt_bps2>310, NA, pt_bps2),
                              ifelse(pt_bpd2<20 | pt_bpd2>200, NA, pt_bpd2))]

# Renal
# NB: The Scr variables are duplicates (can ignore unit / no units)
en.renal <- c("pt_en_serum_creatinine", "pt_en_diab_acr")

ALL_PREDICT[, (en.renal) := list(ifelse(pt_serum_creatinine <10 | pt_serum_creatinine >1200, NA, pt_serum_creatinine),
                                 ifelse(pt_diab_acr>1660, NA, pt_diab_acr))]


# Glucose / Haemoglobin
ALL_PREDICT[pt_glucose<1.1 | pt_glucose>54.6, pt_glucose := NA]


# Remove out-of-range test dates
# Remove any test date older than 1999-01-01 (5 years prior to earliest record) or later than "2018-12-31" (appx. 8 weeks after last visit)
testvar.dates <- c("pt_diab_acrdate", "pt_glucosedate", "pt_hba1c_mmdate", "pt_hba1cdate",
                   "pt_hdldate", "pt_ldldate", "pt_serum_creatininedate", "pt_tchdl_ratiodate", "pt_tcldate")

en.testvar.dates <- c("pt_en_diab_acrdate", "pt_en_glucosedate", "pt_en_hba1c_mmdate", "pt_en_hba1cdate",
                      "pt_en_hdldate", "pt_en_ldldate", "pt_en_serum_creatininedate", "pt_en_tchdl_ratiodate", "pt_en_tcldate")

ALL_PREDICT[, (en.testvar.dates) := lapply(.SD, function(x)
  as.Date(ifelse(as.Date(as.numeric(x), origin="1970-01-01") < "1999-01-01"
                 | as.Date(as.numeric(x), origin="1970-01-01") > "2018-12-31",
                 NA,
                 x), origin="1970-01-01")), .SDcols=testvar.dates]

# Write to missing if either value or date is missing
date.vars <- c("pt_en_diab_acrdate", "pt_en_glucosedate", "pt_en_hdldate", "pt_en_ldldate", "pt_en_serum_creatininedate", 
               "pt_en_tchdl_ratiodate", "pt_en_tcldate")

test.vars <- c("pt_en_diab_acr", "pt_en_glucose", "pt_en_hdl", "pt_en_ldl", "pt_en_serum_creatinine", 
               "pt_en_tchdl_ratio", "pt_en_tcl")

for(i in test.vars){
  
  date.variable <- paste0(i, "date")
  test.variable <- i
  
  ALL_PREDICT[is.na(get(date.variable)), (test.variable) := NA]
  ALL_PREDICT[is.na(get(test.variable)), (date.variable) := NA]
  
}

# ---- C.  HBA1C -----
# HbA1c - Convert % values to mmol/mol
# If pt_hba1c value is greater than 22, then treat it as mmol/mol. Write into pt_hba1c_mm only when pt_hba1c_mm is missing (affects 9464 records).
# Then fill missing pt_hba1c_mm using value from pt_hba1c + date
# If enrich results are <5, then write as missing
# Nb: Procedural order is very important!
library(dplyr)

ALL_PREDICT <- ALL_PREDICT %>% 
  mutate(hba1cmm_missing = +(is.na(pt_hba1c_mm)),
         
         # Use mmol/mol (if missing, conver %)
         pt_en_hba1c_mm = ifelse(hba1cmm_missing==1, 
                                 (pt_hba1c*10.93)-23.5,
                                 pt_hba1c_mm),
         
         pt_en_hba1c_mmdate = as.Date(ifelse(hba1cmm_missing==1, 
                                             pt_hba1cdate,
                                             pt_hba1c_mmdate), origin="1970-01-01"),
         
         # If result of % is >22 & mmol/mol is missing, then treat value as mmol/mol
         pt_en_hba1c_mm = ifelse(pt_hba1c > 22 & hba1cmm_missing==1,
                                 pt_hba1c,
                                 pt_en_hba1c_mm),
         
         pt_en_hba1c_mmdate = as.Date(ifelse(pt_hba1c > 22 & hba1cmm_missing==1,
                                             pt_hba1cdate,
                                             pt_en_hba1c_mmdate), origin="1970-01-01"),
         
         # If enriched mmol/mol value is <5 and no other values exist, then treat as missing
         pt_en_hba1c_mm = ifelse(pt_en_hba1c_mm < 5 & hba1cmm_missing==1,
                                 NA,
                                 pt_en_hba1c_mm),
         
         pt_en_hba1c_mmdate = as.Date(ifelse(pt_en_hba1c_mm < 5 & hba1cmm_missing==1,
                                             NA,
                                             pt_en_hba1c_mmdate), origin="1970-01-01"),
         
         # If test date older than 1999-01-01 (5 years prior to earliest record) or later than "2018-12-31" (appx. 8 weeks after last visit)
         # then treat as missing
         pt_en_hba1c_mmdate = ifelse(pt_en_hba1c_mmdate < "1999-01-01" | pt_en_hba1c_mmdate > "2018-12-31", 
                                     NA,
                                     pt_en_hba1c_mmdate),
         
         
         # If either value or date are missing [both required], then treat as missing
         pt_en_hba1c_mm = ifelse(is.na(pt_en_hba1c_mm) | is.na(pt_en_hba1c_mmdate),
                                 NA,
                                 pt_en_hba1c_mm),
         
         pt_en_hba1c_mmdate = as.Date(ifelse(is.na(pt_en_hba1c_mm) | is.na(pt_en_hba1c_mmdate),
                                             NA,
                                             pt_en_hba1c_mmdate), origin="1970-01-01"))


# E.  Other Date Cleaning
#     NB: Some dates in PREDICT beyond 2018-12-31 is an error (ie. except for "nhi_en_dod", "nhi_last_updated_date").
#         General fix - For any dates containing "year" beyond 2017, re-write them as year of visit date eg. 3007 = 2007, 2911 = 2011, 9999 = 2008

#         In the dataset, the remaining variables containing odd dates [if not already tided up in previous steps] 
#         were mainly in diet and diabetes variables:
setDT(ALL_PREDICT)

otherdate.vars <- c("pt_last_diet_check","pt_referral_diet_given","pt_diab_diet_referral",
                    "pt_diab_edu_referral","pt_diab_feet_date_last_check","pt_diab_eye_lastret")

en.otherdate.vars <- gsub("pt_", "pt_en_", otherdate.vars)

# Apply Fix
library(lubridate)

ALL_PREDICT[, .SDcols = otherdate.vars
            , (en.otherdate.vars) := lapply(.SD, function(x)
              as.Date(ifelse(year(x)>=2019,
                             paste(year(view_visit_date), month(x), day(x), sep="-"),
                             as.character(x)), origin = "1970-01-01"))]

# F.  Drug Binary Conversion
#     Collapse all levels to 0 (no meds) or a 1 (medicated), but retain NA (missing / no input) 

# Non-aspirin meds (template 1) have a 3 level system where current 0 (no),1(Contraindicated / Not tolerated)
# map to new and current 2 ("yes") maps to 1  
pt.drug.vars <- c("pt_clopidogrel", "pt_warfarin", "pt_ace_inhibitor",
                  "pt_at2", "pt_beta_blocker", "pt_thiazide", "pt_calcium_antagonist",
                  "pt_other_hyp_drugs", "pt_statin", "pt_fibrate", "pt_other_lipid_drugs")

en.drug.vars <- gsub("pt_", "pt_en_", pt.drug.vars)

ALL_PREDICT[, .SDcols = pt.drug.vars
            , (en.drug.vars) := lapply(.SD, function(x){
              x <- replace(x, which(x==1), 0)
              replace(x, which(x==2), 1)}
            )]

#   NB: Aspirin has a 4 level system where current 0 (no), 1(Contraindicated / Not tolerated)
#       and 3 ("Don't know") map to new 0 ("No") and current 2 ("yes") maps to 1 ("yes")
ALL_PREDICT[, pt_en_aspirin := {
  
  pt_aspirin <- replace(pt_aspirin, which(pt_aspirin==1 | pt_aspirin==3), 0)
                replace(pt_aspirin, which(pt_aspirin==2), 1)
                
}]


# lapply(ALL_PREDICT[, drug.vars, with=F], function(x) table(x, exclude = NULL))
lapply(ALL_PREDICT[, en.drug.vars, with=F], function(x) table(x, exclude = NULL))
 
  
# Save / 564743 
fst::write.fst(ALL_PREDICT, "source_data/R/PREDICT/2018/ALL_PREDICT_Part_3.fst", compress = 75)
