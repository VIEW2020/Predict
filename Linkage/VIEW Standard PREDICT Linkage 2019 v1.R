
# VIEW Standard PREDICT Linkage for Primary Prevention - v1

# Updated September 2019

# Predict 2018 dataset - AUckland & Northland Only
# Use year 30-74

library(data.table)
library(dplyr)
library(fst)

# First PREDICT 
PREDICT <- read.fst("source_data/R/PREDICT/2018/Cleaned_PREDICT_2018_1st_Record_v1.fst", as.data.table = T)

# ---- A.   History / Outcomes ----

# Loaded pre-prepared national collection 
files.path <- "V:/source_data/R/PREDICT/2018/National Collection/"

files.list <- list.files(files.path, pattern="PUBLIC")

ALL_PUBLIC <- rbindlist(lapply(files.list, function(x)
  read.fst(paste0(files.path, x), 
           as.data.table = T)))

ALL_DEATHS <- read.fst(paste0(files.path, "VSIMPLE_DEATHS_v1.fst"), as.data.table = T)


# -- 1.   Definitions --

# Index Date
ALL_PUBLIC <- ALL_PUBLIC[VSIMPLE_INDEX_MASTER %in% PREDICT$VSIMPLE_INDEX_MASTER]
ALL_PUBLIC <- ALL_PUBLIC[, study_index_date := PREDICT$view_visit_date[match(VSIMPLE_INDEX_MASTER, PREDICT$VSIMPLE_INDEX_MASTER)]] # First PREDICT assessment

# CVD Clin Codes
VIEW_CVD_ICD  <- readxl::read_xlsx("V:/common_lookups/Definitions/VIEW_CVD_ICD10_II_28JUN18_Clean042.xlsx")
VIEW_DM_ICD   <- readxl::read_xlsx("V:/common_lookups/Definitions/VIEW_CVD_ICD10_II_28JUN18_Clean042.xlsx", sheet = 2)
VIEW_RENAL    <- readxl::read_xlsx("V:/common_lookups/Definitions/VIEW_CVD_ICD10_II_28JUN18_Clean042.xlsx", sheet = "Renal")

# Retrieve ICD10 Codes for Events
# Nb: PREDICT datasets must always have broad CVD, diabetes, AF, and heart failure
cvd.var.names <- c("hx_broad_cvd", "hx_heart_failure", "hx_unst_angina", "hx_other_chd", "hx_ischaemic_stroke", "hx_tia",
                   "hx_other_cevd", "hx_chd_procs", "hx_pci", "hx_cabg", "hx_other_chd", "hx_pvd_procs", "hx_pvd_diags",
                   "hx_other_chd_procs", "hx_haemorrhagic_stroke", "hx_mi", "out_broad_cvd", "mortality_broad_cvd_with_other")

for(var in cvd.var.names){
  
  var.codes <- VIEW_CVD_ICD$CLINICALCODE[which(eval(VIEW_CVD_ICD[,var])=="Y")]
  
  assign(gsub("_", ".", var), var.codes)
  
}

hx.diabetes   <- VIEW_DM_ICD$CLINICALCODE[which(VIEW_DM_ICD$hx_diabetes=="Y")]
out.diabetes  <- VIEW_DM_ICD$CLINICALCODE[which(VIEW_DM_ICD$out_diabetes=="Y")]

# Transplatation / Dialysis
hx.renal.dialysis.transp <- VIEW_RENAL$CLINICALCODE[which(VIEW_RENAL$hx_renal_dialysis_transp == "Y")]

# AF
hx.af <- "^I48"


# -- 2.  All-time History --
HX_EVENTS <- copy(ALL_PUBLIC)[EVSTDATE <= study_index_date]

history.vars <- ls(pattern = "^hx.")

for(var in history.vars){
  
  codes <- get(var)
  
  if(var=="hx.af"){
    EVENTS <- HX_EVENTS[, hx_event:= +(grepl(paste(codes, collapse = "|"), CLIN_CD_10))]
  } else {
    EVENTS <- HX_EVENTS[, hx_event:= +(CLIN_CD_10 %in% codes)]
  }
  
  EVENTS <- EVENTS[hx_event==1, 
                   unique(.SD),
                   .SDcols = c("VSIMPLE_INDEX_MASTER", "hx_event")]
  
  ALL_HX_VARS <- if(var == history.vars[1]){
    
    merge(PREDICT[,.(VSIMPLE_INDEX_MASTER)], EVENTS,
          by="VSIMPLE_INDEX_MASTER",
          all.x=T) 
    
  } else {
    
    merge(ALL_HX_VARS, EVENTS,
          by="VSIMPLE_INDEX_MASTER",
          all.x=T)
    
  }
  
  ALL_HX_VARS[, hx_event := +(!is.na(hx_event))]
  setnames(ALL_HX_VARS, "hx_event", gsub("\\.", "_", var))
  
  print(paste(var, " completed")); rm(EVENTS)
  
}

# -- 3.  Outcomes --
OUT_EVENTS <- copy(ALL_PUBLIC)[EVSTDATE > study_index_date]

outcome.vars <- c("out.broad.cvd", "out.diabetes")

for(var in outcome.vars){
  
  codes <- get(var)
  
  OUT_EVENTS[, out_event:= +(CLIN_CD_10 %in% codes)]
  
  EVENTS <- OUT_EVENTS[out_event==1][order(EVSTDATE)
                                     , index:=seq_len(.N)
                                     , by=VSIMPLE_INDEX_MASTER][index==1, .(VSIMPLE_INDEX_MASTER, out_event, EVSTDATE)]
  
  ALL_OUT_VARS <- if(var == outcome.vars[1]){
    
    merge(PREDICT[,.(VSIMPLE_INDEX_MASTER)], EVENTS,
          by="VSIMPLE_INDEX_MASTER",
          all.x=T) 
    
  } else {
    
    merge(ALL_OUT_VARS, EVENTS,
          by="VSIMPLE_INDEX_MASTER",
          all.x=T)
    
  }
  
  ALL_OUT_VARS[, out_event := +(!is.na(out_event))]
  
  setnames(ALL_OUT_VARS, "out_event", gsub("\\.", "_", var))
  setnames(ALL_OUT_VARS, "EVSTDATE", paste0(gsub("\\.", "_", var), "_adm_date"))
  
  print(paste(var, " completed")); rm(EVENTS)
  
}


# -- 4.  Admissions 28 days prior to death --
# CVD within 28 days of death [required for fatal CVD events]
# Captures CVD hospitalisation within 28 days of death occuring


# i.  Add procedures to Broad CVD Definition
out.pvd.procs <- VIEW_CVD_ICD$CLINICALCODE[which(VIEW_CVD_ICD$out_pvd_procs=="Y")]
out.pci.cabg  <- VIEW_CVD_ICD$CLINICALCODE[which(VIEW_CVD_ICD$out_pci_cabg=="Y")]

fatal.broad.cvd <- c(out.broad.cvd, out.pvd.procs, out.pci.cabg)

out.28d.vars <- "fatal.broad.cvd"

# Add info - date of death
ALL_PUBLIC[, "en_dod" := PREDICT$view_ag_dod[match(VSIMPLE_INDEX_MASTER, PREDICT$VSIMPLE_INDEX_MASTER)]]

# Find events
# The following conditions are extremely important to include - ALL must be included!
#   - DOD less than "2016-12-31 (ensure all deaths occur within study period and not after)
#   - Event admission greater than index date
#   - Event admission is within 28 days of death
#   - Event diagnosis includes outcome of broad CVD and procedures
EVENTS_28D_PRIOR <- copy(ALL_PUBLIC)[en_dod <="2018-12-31" & EVSTDATE > study_index_date][(EVSTDATE-en_dod >=-28) & (EVSTDATE <= en_dod)]

# Find earliest CVD & record type
for(var in out.28d.vars){
  
  codes <- get(var)
  
  EVENTS <- EVENTS_28D_PRIOR[, out_event:= +(CLIN_CD_10 %in% codes)]
  
  EVENTS <- EVENTS_28D_PRIOR[out_event==1][order(EVSTDATE)
                                           , index:=seq_len(.N)
                                           , by=VSIMPLE_INDEX_MASTER][index==1, .(VSIMPLE_INDEX_MASTER, out_event, EVSTDATE)]
  
  ALL_28D_OUT_VARS <-  if(var == out.28d.vars[1]){
    
    merge(PREDICT[,.(VSIMPLE_INDEX_MASTER, view_ag_dod)], EVENTS,
          by="VSIMPLE_INDEX_MASTER",
          all.x=T) 
    
  } else {
    
    merge(ALL_28D_OUT_VARS, EVENTS,
          by="VSIMPLE_INDEX_MASTER",
          all.x=T)
  }
  
  ALL_28D_OUT_VARS[, out_event := +(!is.na(out_event))]
  setnames(ALL_28D_OUT_VARS, "out_event", gsub("\\.", "_", var))
  setnames(ALL_28D_OUT_VARS, "EVSTDATE", paste0(gsub("\\.", "_", var), "_adm_date"))
  
  print(paste(var, " completed")); rm(EVENTS)
  
}

setnames(ALL_28D_OUT_VARS, "fatal_broad_cvd", "out_fatal_28d_cvd_adm")


# -- 5.  Mortality --

# CVD / non-CVD causes of death
# Define death codes and categories: Capture all ICD codes for CVD Deaths
death.vars <- "mortality.broad.cvd.with.other"

mort.icd.vars <- c("ICDD","ICDF1","ICDF2","ICDF3","ICDF4","ICDF5","ICDF6")

# a.  Find Deaths
#    Create a loop function to identify codes that are related to the groups and flag them
for(var in death.vars){
  
  codes <- get(var)
  name  <- tolower(gsub("out.", "mortality_", gsub(".", "_", var, fixed = T)))
  
  DEATHS <- copy(ALL_DEATHS)[, outcome := apply(.SD, 1, function(x)
    +(na.omit(any(x %in% codes)))), 
    .SDcols=mort.icd.vars ][outcome==1]
  
  if(var == death.vars[1]){
    
    CVD_DEATHS <- merge(PREDICT[,.(VSIMPLE_INDEX_MASTER)], DEATHS[,.(VSIMPLE_INDEX_MASTER, outcome)],
                        by="VSIMPLE_INDEX_MASTER", 
                        all.x=T)
  } else {
    
    CVD_DEATHS <- merge(CVD_DEATHS, DEATHS[,.(VSIMPLE_INDEX_MASTER, outcome)],
                        by="VSIMPLE_INDEX_MASTER", 
                        all.x=T)
    
  }
  
  CVD_DEATHS[, outcome := +(!is.na(outcome))]
  setnames(CVD_DEATHS, "outcome", paste0(name))
  
}


# b.  Find deaths where they didn't die of CVD (death known but not CVD)
DEATHS <- copy(ALL_DEATHS)[, outcome := apply(.SD, 1, function(x)
  +(na.omit(!any(x %in% mortality.broad.cvd.with.other)))), 
  .SDcols=mort.icd.vars][outcome==1]

CVD_DEATHS <- merge(CVD_DEATHS, DEATHS[,.(VSIMPLE_INDEX_MASTER, outcome)],
                    by="VSIMPLE_INDEX_MASTER", 
                    all.x=T)

CVD_DEATHS[, outcome := +(!is.na(outcome))]
setnames(CVD_DEATHS, "outcome", "mortality_non_cvd")


# -- 6.  Merge all to Baseline --
Baseline <- Reduce(function(...)
  merge(..., 
        by="VSIMPLE_INDEX_MASTER",
        all.x = T),
  list(PREDICT, ALL_HX_VARS, ALL_OUT_VARS, ALL_28D_OUT_VARS[,.(VSIMPLE_INDEX_MASTER, out_fatal_28d_cvd_adm)], CVD_DEATHS))


# -- 7.  Additional Info --

# Make sure DODs do not exceed start or end of study!
sum(Baseline$view_ag_dod > "2018-12-31", na.rm = T)

# Complementary Information
vars <- c("mortality_uncoded", "mortality_all_cause")

Baseline[, (vars) := .(+(!is.na(view_ag_dod) & mortality_broad_cvd_with_other==0 & mortality_non_cvd==0),
                       +(!is.na(view_ag_dod)))]

# Make sure all death categories add up to total all-cause mortality
sum(Baseline$mortality_all_cause) == sum(Baseline$mortality_broad_cvd_with_other + Baseline$mortality_non_cvd + Baseline$mortality_uncoded)


# Save
write.fst(Baseline, "source_data/R/PREDICT/2018/Working/PP_Link_EndPart_A.fst", 75)

rm(list=setdiff(ls(), "Baseline"))


# ---- B.   Pharms ----

# files.path <- "V:/source_data/R/PREDICT/2018/National Collection/"
# files.list <- list.files(files.path, pattern="PHH")
# 
# ALL_PHARMS <- rbindlist(lapply(files.list,
#                                function(x){
#                                  DATA <- read.fst(paste0(files.path, x), as.data.table = TRUE)
#                                  DATA[VSIMPLE_INDEX_MASTER %in% Baseline$VSIMPLE_INDEX_MASTER]
#                                }))
# 
# ALL_PHARMS <- ALL_PHARMS[year(DATE_DISPENSED) %in% 2005:2018]
# 
# write.fst(ALL_PHARMS, "source_data/R/PREDICT/2018/Working/PP_Link_ALL_PHARMS_v1.fst", 75)

ALL_PHARMS <- read.fst("source_data/R/PREDICT/2018/Working/PP_Link_ALL_PHARMS_v1.fst", as.data.table = T)

# 1.  Definition
DIM_FORM_PACK <- read.fst("V:/common_lookups/CURRENT_PHARMS_LOOKUP_VIEW.fst", 
                          as.data.table = T)

vars.needed <- c("antiplatelets", "anticoagulants", "lipid_lowering", "bp_lowering", "antidiabetes", "metolazone", 
                 "loopdiuretics", "antianginals")

new.varname <- c("all.antipla", "all.anticoag", "all.llds", "all.bplds", "all.diabetes", "metolazone", 
                 "loop.diuretics", "antianginals")

setnames(DIM_FORM_PACK, vars.needed, new.varname)

# Capture Form Pack IDs (Lookup method)
for(class in new.varname){
  
  class.codes <- eval(substitute(
    DIM_FORM_PACK$DIM_FORM_PACK_SUBSIDY_KEY[which(DIM_FORM_PACK[, class, with = F]==1)]
  ))
  
  assign(class, class.codes)  
  
}

# Finalise / tidy
all.mx.groups <- new.varname

rm(list=setdiff(ls(), c("Baseline","ALL_PHARMS", "all.mx.groups", all.mx.groups)))


# 2. Standard Drug Capture 
ALL_PHARMS[, study_index_date := Baseline$view_visit_date[match(VSIMPLE_INDEX_MASTER, Baseline$VSIMPLE_INDEX_MASTER)]]

source("V:/bwu009/My Projects/Global Procedures/Standard Pharms Procedure - Primary Prevention.R")

# Merge
Baseline <- merge(Baseline, ALL_DRUG_VARS,
                  by = "VSIMPLE_INDEX_MASTER",
                  all.x = T)

# Quality Assurance
# Ensure no record of dispensing of any drug > 28 days after date of death 
ALL_PHARMS[, dod := Baseline$view_ag_dod[match(VSIMPLE_INDEX_MASTER, Baseline$VSIMPLE_INDEX_MASTER)]]

DOD_PHARMS <- ALL_PHARMS[!is.na(dod)]

# Capture last dispensing
DOD_PHARMS <- DOD_PHARMS[order(DATE_DISPENSED, decreasing = T), 
                         index := seq_len(.N), by="VSIMPLE_INDEX_MASTER"][index==1]

DOD_PHARMS <- DOD_PHARMS[, exclude := +(DATE_DISPENSED > dod+28)][exclude==1]

# Merge flag to Baseline & exclude
# Removes 466 people
Baseline[, phh_post_dod := DOD_PHARMS$exclude[match(VSIMPLE_INDEX_MASTER, DOD_PHARMS$VSIMPLE_INDEX_MASTER)]]

Baseline <- Baseline[phh_post_dod==0 | is.na(phh_post_dod)][, -"phh_post_dod", with=F] # -466 

# save
write.fst(Baseline, "source_data/R/PREDICT/2018/Working/PP_Link_EndPart_B.fst", 75)

rm(list = setdiff(ls(), "Baseline"))


# ---- C.  Lab Values ----

FindNearestDate <- dget("V:/bwu009/My Projects/Generic Functions/Find Nearest Date.R")

lab.tests <- c("HBA1C", "TCHDL")

# Standard VIEW Tests
# nb: look back 2 years prior to index date, allow for 14 days post index
for(test in lab.tests){
  
  DATA <- read.fst(paste0("V:/source_data/R/TESTSAFE/", test, "/VSIMP_", test, "_2004_2018.fst"),
                   as.data.table = T)
  
  DATA <- DATA[VSIMPLE_INDEX_MASTER %in% Baseline$VSIMPLE_INDEX_MASTER]
  
  DATA[, study_index_date := Baseline$view_visit_date[match(VSIMPLE_INDEX_MASTER, Baseline$VSIMPLE_INDEX_MASTER)]]
  
  DATA <- DATA[RESULT_DATE - study_index_date >= -730 & RESULT_DATE - study_index_date <=14]
  
  test.vars <- c("nearest_res", "nearest_date")
  
  DATA[, (test.vars) := list(
    EN_OBSR_RESULT_NUM[FindNearestDate(study_index_date, RESULT_DATE, showdate=F)],
    FindNearestDate(study_index_date, RESULT_DATE, showdate=T)
  ), by = VSIMPLE_INDEX_MASTER]
  
  # Compile
  priority.vars <- c(paste0("index_dir24mth_", tolower(test), "_result"),
                     paste0("index_dir24mth_", tolower(test), "_resultdate"))
  
  Baseline[, (priority.vars) := list(DATA$nearest_res[match(VSIMPLE_INDEX_MASTER, DATA$VSIMPLE_INDEX_MASTER)],
                                     DATA$nearest_date[match(VSIMPLE_INDEX_MASTER, DATA$VSIMPLE_INDEX_MASTER)])]
  
}

# save
write.fst(Baseline, "source_data/R/PREDICT/2018/Working/PP_Link_EndPart_C.fst", 75)

# ---- D.  Improved Variables ----

# -- i.  Improve CVD deaths / non-CVD-death -- 
Baseline[, imp_fatal_cvd := +(out_fatal_28d_cvd_adm==1 | mortality_broad_cvd_with_other==1)]
Baseline[, imp_fatal_non_cvd := +(imp_fatal_cvd==0 & mortality_non_cvd==1)]
Baseline[, imp_fatal_uncoded_death := +(imp_fatal_cvd==0 & imp_fatal_non_cvd==0 & mortality_uncoded==1)]  

# Check: Improved fatal variables should all add up to total all cause mortality
sum(Baseline$mortality_all_cause) == sum(Baseline$imp_fatal_cvd + Baseline$imp_fatal_non_cvd + Baseline$imp_fatal_uncoded_death)


# -- ii. Improved hx of CVD, diabetes, atrial fibrillation, heart failure, and angina --

# nb: imp_hx_heart_failure - Requires at least one loop diuretic on at least 3 occasions in the last 5 years and
#                            Requires at least one metolazone in last 6 months
#     imp_hx_angina - Requires at least one anti-anginal on at least 3 occasions in the last 5 years
imp.hx.vars <- c("imp_hx_af", "imp_hx_heart_failure", "imp_hx_angina", "imp_hx_cvd", "imp_hx_diabetes")

Baseline[, (imp.hx.vars) := list(+(pt_atrial_fibrillation==1 | hx_af==1),
                                 
                                 +(hx_heart_failure==1 | ph_loop_diuretics_prior_5yrs_3evts==1 | ph_metolazone_prior_6mths==1),
                                 
                                 +(pt_angina==1 | hx_unst_angina==1 | ph_antianginals_prior_5yrs_3evts==1),
                                 
                                 +(pt_mi==1 | pt_ihd==1 | pt_ptca_cabg==1 | pt_stroke==1 | pt_tia==1 | pt_pvd==1 |
                                     hx_mi==1 | hx_unst_angina==1 | hx_other_chd==1 | hx_ischaemic_stroke==1 | hx_tia==1 |
                                     hx_other_cevd==1 | hx_pci==1 | hx_cabg==1 | hx_other_chd==1 | hx_pvd_procs==1 | hx_pvd_diags==1|
                                     hx_other_chd_procs==1 | hx_haemorrhagic_stroke | pt_angina==1 | ph_antianginals_prior_5yrs_3evts==1),
                                 
                                 +(pt_diabetes==1 | pt_diabetes==2 | pt_diabetes==3 | hx_diabetes==1 | ph_all_diabetes_prior_6mths==1))]

# -- iii. Improved hx of CVD drugs --
imp.mx.vars <- c("imp_hx_antihypertensives", "imp_hx_antithrombotics", "imp_hx_lipidlowering")

Baseline[, (imp.mx.vars) := list(+(pt_en_ace_inhibitor==1 | pt_en_at2==1 | pt_en_beta_blocker==1 | pt_en_calcium_antagonist==1 | 
                                     pt_en_other_hyp_drugs==1 | pt_en_thiazide==1 | ph_all_bplds_prior_6mths==1),
                                 
                                 +(ph_all_antipla_prior_6mths==1 | pt_en_aspirin==1 | pt_en_clopidogrel==1 |
                                     ph_all_anticoag_prior_6mths==1 | pt_en_warfarin==1),
                                 
                                 +(ph_all_llds_prior_6mths==1 | pt_en_statin==1 | pt_en_fibrate==1 | pt_en_other_lipid_drugs==1))]


# Binary
all.imps <- c(imp.hx.vars,imp.mx.vars)

Baseline[, (all.imps) := lapply(.SD, function(x)
  ifelse(is.na(x), 0, x)),
  .SDcols = all.imps]


# -- iv. Improved labtest results --

# Use both PREDICT + TestSafe data to improve lab values
# NB: Three possibilities
#     - Only 1 date / result (either PT only or TS Only)
#     - Both PT and TS dates are the same (then use PT result [because it was what the GP saw])
#     - PT and TS dates are different (Take the record nearest to index)
FindNearestDate <- dget("V:/bwu009/My Projects/Generic Functions/Find Nearest Date.R")

# HbA1c
test.types <- c("hba1c", "tchdl")

for(type in test.types){
  
  # Data Prep
  orig.date.vars <- if(type == "hba1c"){
    c("pt_en_hba1c_mmdate", "index_dir24mth_hba1c_resultdate")
  } else {
    c("pt_en_tchdl_ratiodate", "index_dir24mth_tchdl_resultdate")
  }
  
  orig.rslt.vars <- if(type == "hba1c"){
    c("pt_en_hba1c_mm", "index_dir24mth_hba1c_result")
  } else {
    c("pt_en_tchdl_ratio", "index_dir24mth_tchdl_result")
  }
  
  imp.vars <- if(type == "hba1c"){
    c("imp_index2y_hba1c_mm", "imp_index2y_hba1c_mmdate")
  } else {
    c("imp_index2y_tchdl_ratio", "imp_index2y_tchdl_ratiodate")
  }
  
  DATA <- copy(Baseline)[,c("VSIMPLE_INDEX_MASTER", "view_visit_date", orig.date.vars, orig.rslt.vars), with = F]
  
  date.vars <- c("pt_test_date", "ts_test_date")
  rslt.vars <- c("pt_test_result", "ts_test_result")
  
  setnames(DATA, orig.date.vars, date.vars)
  setnames(DATA, orig.rslt.vars, rslt.vars)
  
  DATA[, (imp.vars) := list(as.numeric(NA),
                            as.Date(NA))]
  
  # i.  If there is only one date/result, then use that one
  DATA[apply(DATA[, date.vars, with=F], 1, function(x)
    sum(is.na(x))==1), 
    
    (imp.vars) := list(
      apply(.SD, 1, function(x)
        as.numeric(na.omit(x[rslt.vars]))),
      
      apply(.SD, 1, function(x)
        as.Date(na.omit(x[date.vars])))
    )]
  
  # ii. If dates are the same in both, then write date and use PREDICT result
  DATA[apply(DATA[, date.vars, with=F], 1, function(x)
    !any(is.na(x)) & 
      as.Date(x["pt_test_date"]) == as.Date(x["ts_test_date"])),
    
    (imp.vars) := list(
      apply(.SD, 1, function(x)
        as.numeric(
          x["pt_test_result"])),
      
      apply(.SD, 1, function(x)
        as.Date(
          x["pt_test_date"]))
    )]
  
  
  # iii.  If dates are different, then take the one closest to visit date and take the result of that date
  #       NB: In case of a tie, use the date from the past (since it has already occured)
  DATA[apply(DATA[, date.vars, with=F], 1, function(x)
    !any(is.na(x)) & 
      as.Date(x["pt_test_date"]) != as.Date(x["ts_test_date"])),
    
    (imp.vars) := list(
      as.numeric(
        apply(.SD, 1, function(x)
          x[rslt.vars][FindNearestDate(x["view_visit_date"], x[date.vars], showdate=F)])),
      
      as.Date(
        apply(.SD, 1, function(x)
          FindNearestDate(x["view_visit_date"], x[date.vars], showdate=T)), "1970-01-01")
    )]
  
  # Merge
  Baseline <- merge(Baseline, DATA[,c("VSIMPLE_INDEX_MASTER", imp.vars), with = F],
                    by = "VSIMPLE_INDEX_MASTER", 
                    all.x = T)
  
  print(paste(type, " completed"))
  
}


# -- Serum Creatinin CKD-Epi KDIGO Classifications --

# nb. eGFR required - should have been pre-calcuated in the Scr prepartation step 
ALL_CREATININE <- read.fst("source_data/R/TESTSAFE/SCR/VSIMP_SCR_2004_2018.fst", as.data.table = T)

ALL_CREATININE <- ALL_CREATININE[VSIMPLE_INDEX_MASTER %in% Baseline$VSIMPLE_INDEX_MASTER]
ALL_CREATININE <- ALL_CREATININE[, study_index_date := Baseline$view_visit_date[match(VSIMPLE_INDEX_MASTER, Baseline$VSIMPLE_INDEX_MASTER)]]

source("bwu009/My Projects/Global Procedures/CKD KDIGO Classification.R")

Baseline <- merge(Baseline, VIEW_CKD[,.(VSIMPLE_INDEX_MASTER, egfr_index_result, egfr_index_result_date, ckd_egfr_lt30, ckd_egfr_lt60)],
                  by = "VSIMPLE_INDEX_MASTER",
                  all.x = T)

Baseline[is.na(ckd_egfr_lt30), ckd_egfr_lt30 := 5]
Baseline[is.na(ckd_egfr_lt60), ckd_egfr_lt60 := 5]

# save
write.fst(Baseline, "source_data/R/PREDICT/2018/Working/PP_Link_EndPart_D.fst", 75)

rm(list = setdiff(ls(), "Baseline"))

# ---- E. General CVD Risk Score ----

# -- i.  Apply Exclusions # ~110261

# Include only Auckland / Northland, age 30-74
# Exclude 'other' Ethnicities, history of CVD, type 1 diabetes, history of renal dialysis/transplatation,
# microalbuminuria / nephropathy, confirmed CKD <30 eGFR, history of heart failure
Baseline <- Baseline[view_en_dhbcatchment %in% c(11, 21:23) &
                       view_ag_age %in% 30:74 &
                       view_ag_eth %in% c(1:4, 42, 43) &
                       imp_hx_cvd != 1 &
                       pt_diabetes != 1 &
                       !pt_renal %in% c(2:3) &
                       hx_renal_dialysis_transp != 1 &
                       ckd_egfr_lt30 != 11 &
                       imp_hx_heart_failure != 1]

# Always ensure no missingness: remaining TCHDL, smoking
Baseline <- Baseline[!is.na(imp_index2y_tchdl_ratio) & 
                       !is.na(pt_smoking)]

# -- ii.   Equation Variable Preparation
DATA <- copy(Baseline)

# 1.  Age
DATA$age    <- DATA$view_ag_age

# 2.  Ethnicity
eth.vars <- c("maori", "pacific", "indian", "asian")

DATA[, (eth.vars) := list(+(view_ag_eth == 2),
                          +(view_ag_eth == 3),
                          +(view_ag_eth == 43),
                          +(view_ag_eth == 4 | view_ag_eth == 41))]

# 3.  Smoking
smoke.vars <- c("ex_smoke", "cur_smoke")

DATA[, (smoke.vars)] <- list(+(DATA$pt_smoking %in% 1:2),
                             +(DATA$pt_smoking %in% 3:5))

# 4. NZDep (as quintile)
DATA$nzdep <- DATA$view_en_nzdep

# 5.  History
DATA$diabetes   <- DATA$imp_hx_diabetes
DATA$af         <- DATA$imp_hx_af
DATA$familyhx   <- DATA$pt_familyhistory

# 6. CVD Mx
medication.vars <- c("lipidlower", "antithromb", "bplowering")

DATA[, (medication.vars)] <- list(DATA$imp_hx_lipidlowering,
                                  DATA$imp_hx_antithrombotics,
                                  DATA$imp_hx_antihypertensives)

# 7.  Measures
DATA$sbp    <- apply(DATA[, c("pt_en_sbp", "pt_en_sbp2")], 1, 
                     function(x)
                       mean(x, na.rm=T))

DATA$tchdl  <- DATA$imp_index2y_tchdl_ratio

# Calculate
source("V:/bwu009/Predict/Risk Scores/VIEW 5yr CVD Imp Vars (no BMI).R")

Baseline <- merge(Baseline, DATA[, c("VSIMPLE_INDEX_MASTER", "imp_cvd_risk5y_score")],
                  by = "VSIMPLE_INDEX_MASTER")

write.fst(Baseline, "source_data/R/PREDICT/2018/Working/PP_Link_EndPart_E.fst", 75)


# ---- F. Final Tidy Up ----

library(dplyr)

# Variable removal
ptdiab.vars <- names(Baseline)[startsWith(names(Baseline), "pt_diab_") | startsWith(names(Baseline), "pt_en_diab")]
ptrun.vars  <- names(Baseline)[startsWith(names(Baseline), "pt_run")]
admin.vars  <- c("pt_current_hba1cdate", "pt_submissions_timestamp")
mort.vars   <- c("mortality_broad_cvd_with_other", "mortality_non_cvd", "mortality_uncoded")

Baseline <- Baseline %>% 
  select(
    setdiff(
      names(Baseline), 
      c(mort.vars, ptdiab.vars, ptrun.vars, admin.vars)))

Baseline <- Baseline %>%
  select(VSIMPLE_INDEX_MASTER, view_visit_date,
         starts_with("view"),
         starts_with("pt"),
         starts_with("hx"),
         starts_with("cmi"),
         starts_with("out"),
         starts_with("mortality"),
         starts_with("ph"),
         starts_with("index"),
         starts_with("ckd"),
         starts_with("imp"))

# Remove all components used for improvement (i.e. keep only improved variable)
imp.components <- c("pt_atrial_fibrillation" , "ph_loop_diuretics_prior_5yrs_3evts" , "ph_metolazone_prior_6mths",
                    "hx_af",  "hx_heart_failure" , 
                    "pt_angina" , "hx_unst_angina" , "ph_antianginals_prior_5yrs_3evts", "pt_mi" , "pt_ihd" , "pt_ptca_cabg" , "pt_stroke" , "pt_tia" , 
                    "pt_pvd" , "hx_mi" , "hx_unst_angina" , "hx_other_chd" , "hx_ischaemic_stroke" , "hx_tia" , "hx_other_cevd" , "hx_pci" , "hx_cabg" ,
                    "hx_other_chd" , "hx_pvd_procs" , "hx_pvd_procs", "hx_haemorrhagic_stroke" , "pt_angina" , "ph_antianginals_prior_5yrs_3evts",
                    "pt_diabetes" , "pt_diabetes", "pt_diabetes", "hx_diabetes",
                    "pt_en_ace_inhibitor" , "pt_en_at2" , "pt_en_beta_blocker" , "pt_en_calcium_antagonist" , "pt_en_other_hyp_drugs" , 
                    "pt_en_thiazide" , "ph_all_bplds_prior_6mths", "ph_all_antipla_prior_6mths" , "pt_en_aspirin" , "pt_en_clopidogrel" ,
                    "ph_all_anticoag_prior_6mths" , "pt_en_warfarin", "ph_all_llds_prior_6mths" , "pt_en_statin" , "pt_en_fibrate" , 
                    "pt_en_other_lipid_drugs", "hx_pvd_diags", "hx_other_chd_procs", "index_dir24mth_tchdl_result", "index_dir24mth_tchdl_resultdate",
                    "index_dir24mth_hba1c_result", "index_dir24mth_hba1c_resultdate")

# DF <- copy(Baseline)
# DF <- DF[, -imp.components, with=F]
Baseline <- Baseline[, -imp.components, with=F]

# Save
library(feather)

write.fst(Baseline, "source_data/R/PREDICT/2018/Linked_PREDICT_2019_PrimPrev_v1.fst", 75)
write.fst(Baseline, "E:/Data/source_data/R/Predict/Linked_PREDICT_2019_PrimPrev_v1.fst", 75)
write_feather(Baseline, "E:/Data/source_data/PYTHON/Predict/Linked_PREDICT_2019_PrimPrev_v1.feather")














