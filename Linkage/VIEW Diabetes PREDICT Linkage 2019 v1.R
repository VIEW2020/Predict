
# Romana's Primary Prevention for Diabetes Prediction
 
library(data.table)
library(fst)
library(dplyr)

Baseline <- read.fst("source_data/R/PREDICT/2018/Working/PP_Link_EndPart_D.fst", as.data.table = T)

# ---- A. Preparation ----
# Remove redudant vars
# Variable removal
# ptdiab.vars <- names(Baseline)[startsWith(names(Baseline), "pt_diab_") | startsWith(names(Baseline), "pt_en_diab")]
ptrun.vars  <- names(Baseline)[startsWith(names(Baseline), "pt_run")]
admin.vars  <- c("pt_current_hba1cdate", "pt_submissions_timestamp")
mort.vars   <- c("mortality_broad_cvd_with_other", "mortality_non_cvd", "mortality_uncoded")

Baseline <- Baseline %>% 
 select(
  setdiff(
   names(Baseline), 
   c(mort.vars, ptrun.vars, admin.vars)))

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

Baseline <- Baseline[, -imp.components, with=F]

# ---- B. VDR ----
# files.path <- "source_data/R/VDR/"
# files.list <- list.files(files.path, pattern="VDR")
# 
# ALL_VDR <- rbindlist(lapply(files.list,
#                             function(x){
#                              DATA <- read.fst(paste0(files.path, x), as.data.table = TRUE)
#                              DATA[VSIMPLE_INDEX_MASTER %in% Baseline$VSIMPLE_INDEX_MASTER]
#                             }))
# 
# write.fst(ALL_VDR, "source_data/R/PREDICT/2018/Working/PP_Link_ALL_VDR_v1.fst", 75)

ALL_VDR <- read.fst("source_data/R/PREDICT/2018/Working/PP_Link_ALL_VDR_v1.fst", as.data.table = T)

# Find earliest first identification date
ALL_VDR[, study_index_date := Baseline$view_visit_date[match(VSIMPLE_INDEX_MASTER, Baseline$VSIMPLE_INDEX_MASTER)]]


PRIOR_VDR <- copy(ALL_VDR)[first_identification_date <= study_index_date]
POST_VDR <- copy(ALL_VDR)[first_identification_date > study_index_date]

# First
PRIOR_VDR <- PRIOR_VDR[order(first_identification_date), 
                       vdr_hx_diabetes := seq_len(.N),
                       by = VSIMPLE_INDEX_MASTER][vdr_hx_diabetes==1][,.(VSIMPLE_INDEX_MASTER, vdr_hx_diabetes, first_identification_date)]

POST_VDR <- POST_VDR[order(first_identification_date), 
                     vdr_out_diabetes := seq_len(.N),
                     by = VSIMPLE_INDEX_MASTER][vdr_out_diabetes==1][,.(VSIMPLE_INDEX_MASTER, vdr_out_diabetes, first_identification_date)]

setnames(PRIOR_VDR, "first_identification_date", "vdr_hx_diabetes_1st_iddate")
setnames(POST_VDR, "first_identification_date", "vdr_out_diabetes_1st_iddate")

# Merge
Baseline <- Reduce(function(...)
  merge(...,
        by = "VSIMPLE_INDEX_MASTER",
        all.x = T),
  list(Baseline, PRIOR_VDR, POST_VDR))

vars <- c("vdr_hx_diabetes", "vdr_out_diabetes")

Baseline[, (vars) := lapply(.SD, function(x)
  +(!is.na(x))),
  .SDcols = vars]

write.fst(Baseline, "source_data/R/PREDICT/2018/Working/PP_Link_EndPart_DM1.fst", 75)

rm(list = setdiff(ls(), "Baseline"))

# ----  Pharms: Antidiabetes medication split ----
# Insulin, Metformin, Other Oral Hypoglycaemic Drugs
ALL_PHARMS <- read.fst("source_data/R/PREDICT/2018/Working/PP_Link_ALL_PHARMS_v1.fst",
                       as.data.table = T)

DIM_FORM_PACK <- read.fst("V:/common_lookups/CURRENT_PHARMS_LOOKUP_VIEW.fst", 
                          as.data.table = T)

all.mx.groups <- c("antidiabetes", "insulin", "metformin", "other_oralhypos")

# Capture Form Pack IDs (Lookup method)
for(class in all.mx.groups){
  
  class.codes <- eval(substitute(
    DIM_FORM_PACK$DIM_FORM_PACK_SUBSIDY_KEY[which(DIM_FORM_PACK[, class, with = F]==1)]
  ))
  
  assign(class, class.codes)  
  
}
# DESC <- read.fst("common_lookups/CURRENT_TSDESC_LOOKUP.fst")
# Standard Drug Capture 
ALL_PHARMS[, study_index_date := Baseline$view_visit_date[match(VSIMPLE_INDEX_MASTER, Baseline$VSIMPLE_INDEX_MASTER)]]

source("V:/bwu009/My Projects/Global Procedures/Standard Pharms Procedure - Primary Prevention.R")

# Merge
ALL_DRUG_VARS[, ph_antidiabetes_prior_6mths := NULL]

Baseline <- merge(Baseline, ALL_DRUG_VARS,
                  by = "VSIMPLE_INDEX_MASTER",
                  all.x = T)

write.fst(Baseline, "source_data/R/PREDICT/2018/Working/PP_Link_EndPart_DM2.fst", 75)

rm(list = setdiff(ls(), "Baseline"))


# ---- C. TestSafe ----

# Special 24 months lookback / 18 month look forward for ACR and HbA1c
# First look back for value. If missing, then look forward
# Combines both PREDICT data & TestSafe as improved variable

# PT Data
ALL_PREDICT <- read.fst("source_data/R/PREDICT/2018/Cleaned_PREDICT_2018_All_Records_v1.fst", 
                        as.data.table = T)

PT_ACR    <- copy(ALL_PREDICT)[!is.na(pt_en_diab_acr), c("VSIMPLE_INDEX_MASTER", "pt_en_diab_acr", "pt_en_diab_acrdate"), with = F]
PT_HBA1C  <- copy(ALL_PREDICT)[!is.na(pt_en_hba1c_mm), c("VSIMPLE_INDEX_MASTER", "pt_en_hba1c_mm", "pt_en_hba1c_mmdate"), with = F]

# TestSafe Data
TS_ACR <- read.fst("source_data/R/TESTSAFE/ACR/VSIMP_ACR_2004_2018.fst",
                    as.data.table = T)[, c("VSIMPLE_INDEX_MASTER", "EN_OBSR_RESULT_NUM", "RESULT_DATE")]

TS_HBA1C <- read.fst("source_data/R/TESTSAFE/HBA1C/VSIMP_HBA1C_2004_2018.fst",
                      as.data.table = T)[, c("VSIMPLE_INDEX_MASTER", "EN_OBSR_RESULT_NUM", "RESULT_DATE")]


# Prepare the combined dataset
test.types <- c("ACR", "HBA1C")

for(set in test.types){
  
  pt_dat <- get(paste0("PT_", set))
  ts_dat <- get(paste0("TS_", set))
  
  st.names <- c("VSIMPLE_INDEX_MASTER", "result", "result_date")
  
  setnames(pt_dat, names(pt_dat), st.names)
  setnames(ts_dat, names(ts_dat), st.names)
  
  dat <- rbind(pt_dat, ts_dat)
  dat <- dat[VSIMPLE_INDEX_MASTER %in% Baseline$VSIMPLE_INDEX_MASTER][
    , view_visit_date := Baseline$view_visit_date[match(VSIMPLE_INDEX_MASTER, Baseline$VSIMPLE_INDEX_MASTER)]
  ]
  
  assign(set, dat); rm(dat, pt_dat, ts_dat)
  
}

rm(ALL_PREDICT, PT_ACR, PT_HBA1C, TS_ACR, TS_HBA1C)

# Find index test
# Mode: directional = look back, then forward if missing
# Range: Number of days prior (-730) and post (548) index

FindNearest <- dget("bwu009/My Projects/Generic Functions/Find Nearest Date v2.R")

for(test in test.types){
  
  DAT <- get(test)
  
  DAT[, by = VSIMPLE_INDEX_MASTER
      , nearest_flag := FindNearest(index = view_visit_date, 
                                    comparison = result_date, 
                                    mode = "directional",
                                    range = c(-730, 548))]
  
  DAT <- DAT[nearest_flag == 1][, unique(.SD),
                                .SDcols = st.names]
  
  assign(paste0("IMP_", test), DAT); rm(DAT)
  
}

setnames(IMP_ACR, names(IMP_ACR)[-1], c("imp_index2y18m_acr", "imp_index2y18m_acr_date"))
setnames(IMP_HBA1C, names(IMP_HBA1C)[-1], c("imp_index2y18m_hba1c_mm", "imp_index2y18m_hba1c_mmdate"))


# - Triglycerides (5 year look back) -
PT_TRI  <- copy(ALL_PREDICT)[!is.na(pt_tri), c("VSIMPLE_INDEX_MASTER", "pt_tri", "pt_tridate"), with = F]

TS_TRI <- read.fst("source_data/R/TESTSAFE/LIPIDS/VSIMP_TRI_2004_2018.fst",
                   as.data.table = T)[, c("VSIMPLE_INDEX_MASTER", "EN_OBSR_RESULT_NUM", "RESULT_DATE")]

st.names <- c("VSIMPLE_INDEX_MASTER", "result", "result_date")

setnames(PT_TRI, names(PT_TRI), st.names)
setnames(TS_TRI, names(TS_TRI), st.names)

TRI <- rbind(PT_TRI, TS_TRI)
TRI <- TRI[VSIMPLE_INDEX_MASTER %in% Baseline$VSIMPLE_INDEX_MASTER][
  , view_visit_date := Baseline$view_visit_date[match(VSIMPLE_INDEX_MASTER, Baseline$VSIMPLE_INDEX_MASTER)]
  ]

rm(PT_TRI, TS_TRI)

TRI[, by = VSIMPLE_INDEX_MASTER
    , nearest_flag := FindNearest(index = view_visit_date, 
                                  comparison = result_date, 
                                  mode = "nearest",
                                  range = c(-1825, 14))]

IMP_TRI <- TRI[nearest_flag == 1][, unique(.SD),
                                  .SDcols = st.names]

setnames(IMP_TRI, names(IMP_TRI)[-1], c("imp_index5y_tri", "imp_index5y_tri_date"))


# - eGFR (2 year lookback from TestSafe Only) -
TS_SCR <- read.fst("source_data/R/TESTSAFE/SCR/VSIMP_SCR_2004_2018.fst",
                   as.data.table = T)[, c("VSIMPLE_INDEX_MASTER", "EN_OBSR_RESULT_EGFR", "RESULT_DATE")]

TS_SCR <- TS_SCR[VSIMPLE_INDEX_MASTER %in% Baseline$VSIMPLE_INDEX_MASTER][
  , view_visit_date := Baseline$view_visit_date[match(VSIMPLE_INDEX_MASTER, Baseline$VSIMPLE_INDEX_MASTER)]
  ]

TS_SCR[, by = VSIMPLE_INDEX_MASTER
       , nearest_flag := FindNearest(index = view_visit_date, 
                                     comparison = RESULT_DATE, 
                                     mode = "nearest",
                                     range = c(-730, 14))]

TS_SCR <- TS_SCR[nearest_flag == 1][, unique(.SD),
                                    .SDcols = c("VSIMPLE_INDEX_MASTER", "EN_OBSR_RESULT_EGFR", "RESULT_DATE")]

setnames(TS_SCR, names(TS_SCR)[-1], c("egfr_index2y_result", "egfr_index2y_result_date"))

# Combine all
Baseline <- Reduce(function(...)
  merge(...,
        by = "VSIMPLE_INDEX_MASTER",
        all.x = T),
  list(Baseline, IMP_ACR, IMP_HBA1C, IMP_TRI, TS_SCR))


# Save IMP_HBA1C
write.fst(Baseline, "source_data/R/PREDICT/2018/Working/PP_Link_EndPart_DM3.fst", 75)

rm(list = setdiff(ls(), "Baseline"))


# ---- History of Mental Illness ----

# files.path <- "source_data/R/PRIMHD/"
# files.list <- list.files(files.path, pattern="PRIMHD")
# 
# ENHI <- read.fst("source_data/R/ENHI BOTH TABLES/VIEW_VSIMP_MOH__PS_AUG2019.fst", as.data.table = T)
# ENHI <- ENHI[, c("VSIMPLE_INDEX_MASTER", "VIEW_ENHI_2NDARY"), with = F]
# 
# setkey(ENHI, "VIEW_ENHI_2NDARY")
# 
# ALL_PRIMHD <- rbindlist(lapply(files.list,
#                             function(x){
#                              DATA <- as.data.table(readRDS(paste0(files.path, x))); setkey(DATA, "VIEW_ENHI_MASTER")
#                              DATA <- ENHI[DATA, nomatch = 0]
#                              DATA[VSIMPLE_INDEX_MASTER %in% Baseline$VSIMPLE_INDEX_MASTER]
#                             }))
# 
# write.fst(ALL_PRIMHD, "source_data/R/PREDICT/2018/Working/PP_Link_ALL_PRIMHD_v1.fst", 75)

# 1. Capture Activites

ALL_PRIMHD <- read.fst("source_data/R/PREDICT/2018/Working/PP_Link_ALL_PRIMHD_v1.fst", 
                       as.data.table = T)

ALL_PRIMHD[, view_visit_date := Baseline$view_visit_date[match(VSIMPLE_INDEX_MASTER, Baseline$VSIMPLE_INDEX_MASTER)]]

# Defintions
inpatient   <- c("T02","T03","T04","T11","T12","T13","T14","T21")
resident    <- c("T05", "T16", "T20", "T27", "T28", "T29", "T30", "T48")
treatment   <- c("T01", "T09", "T22", "T36", "T38", "T39", "T40", "T41", "T42", "T17", "T18", "T19")
support     <- c("T07", "T22", "T23", "T24", "T43", "T44", "T45")

Activity.types <- c(inpatient, resident, treatment, support)

HX_ACTIVITY <- copy(ALL_PRIMHD)[ACTIVITY_TYPE_CODE %in% Activity.types & ACTIVITY_START_DATE <= view_visit_date]

Baseline[, hx_primhd_activity := +(VSIMPLE_INDEX_MASTER %in% HX_ACTIVITY$VSIMPLE_INDEX_MASTER)]


# 2.  Mental disorders
PRIMHD_CLASS <- readRDS("source_data/R/PRIMHD/Classification Lookup.rds")

setkey(setDT(PRIMHD_CLASS), VIEW_ENHI_MASTER)

PRIMHD_CLASS <- ENHI[PRIMHD_CLASS, nomatch = 0]
PRIMHD_CLASS <- PRIMHD_CLASS[VSIMPLE_INDEX_MASTER %in% Baseline$VSIMPLE_INDEX_MASTER][
  , view_visit_date := Baseline$view_visit_date[match(VSIMPLE_INDEX_MASTER, Baseline$VSIMPLE_INDEX_MASTER)]
][CLASSIFICATION_START_DATE <= view_visit_date]

# Definitions
schiz_non_effect_dsm  <- paste0("^", c(2950:2959, 2970:2979, 2980:2989))
bipolar_effective_dsm <- paste0("^", c(2960:2961, 2964:2969))
dementia_org_dis_dsm  <- paste0("^", c(2900:2909, 2930:2949, 3100:3102, 3310:3333))
substance_use_dis_dsm <- paste0("^", c(2910:2929, 3030:3059))
depress_anxiety_dsm   <- paste0("^", c(2962:2963, 3000:3009, 3080:3099, "3110"))
dev_child_menret_dsm  <- paste0("^", c(2990:2998, 3120:3190))
other_mental_dis_dsm  <- paste0("^", c(3010:3019, 3020:3029, 3060:3078))

# ICD Codes:
schiz_non_effect_icd  <- paste0("^", paste0("F",20:29))
bipolar_effective_icd <- paste0("^", paste0("F",30:31))
dementia_org_dis_icd  <- paste0("^", c(paste0("F0",0:9), "F59", "G30"))
substance_use_dis_icd <- paste0("^", c(paste0("F",10:19), "F55"))
depress_anxiety_icd   <- paste0("^", paste0("F",32:49))
dev_child_menret_icd  <- paste0("^", paste0("F",70:98))
other_mental_dis_icd  <- paste0("^", c(paste0("F",50:54), paste0("F",60:69), "F99"))

Disorder.classes <- c("schiz_non_effect", "bipolar_effective", "dementia_org_dis", "substance_use_dis", 
                      "depress_anxiety", "dev_child_menret", "other_mental_dis")

library(dplyr)

for(i in Disorder.classes){
  
  dsm.codes <- get(paste0(i, "_dsm"))
  icd.codes <- get(paste0(i, "_icd"))
  
  CLASS_DATA_DSM <- PRIMHD_CLASS[grepl(paste0(dsm.codes, collapse = "|"), CLINICAL_CODE) & CLINICAL_CODING_SYSTEM_ID==7]
  CLASS_DATA_ICD <- PRIMHD_CLASS[grepl(paste0(icd.codes, collapse = "|"), CLINICAL_CODE) & CLINICAL_CODING_SYSTEM_ID!=7]
  
  CLASS_DATA <- as.data.table(rbind(CLASS_DATA_DSM, CLASS_DATA_ICD))
  
  CLASS_DATA <- CLASS_DATA[order(CLASSIFICATION_START_DATE),
                           index := seq_len(.N), 
                           by=VSIMPLE_INDEX_MASTER][index == 1, .(VSIMPLE_INDEX_MASTER, index)]
  
  setnames(CLASS_DATA, "index", i)
  
  ALL_CLASS <- if(i == Disorder.classes[1]){
    merge(Baseline[,.(VSIMPLE_INDEX_MASTER)], CLASS_DATA,
          by="VSIMPLE_INDEX_MASTER",
          all.x=T)
  } else {
    merge(ALL_CLASS, CLASS_DATA,
          by="VSIMPLE_INDEX_MASTER",
          all.x=T)
  }
  
  print(paste0(i, " completed")); rm(dsm.codes, icd.codes, CLASS_DATA_DSM, CLASS_DATA_ICD, CLASS_DATA, i)
  
}

ALL_CLASS[, hx_primhd_mental_disorder := apply(.SD, 1, function(x)
  +(any(x == 1, na.rm = T))),
  .SDcols = Disorder.classes]

Baseline <- merge(Baseline, ALL_CLASS[,.(VSIMPLE_INDEX_MASTER, hx_mental_disorder)],
                  by = "VSIMPLE_INDEX_MASTER",
                  all.x = T)


# ---- Other Diseases ----

# Loaded pre-prepared national collection 
files.path <- "V:/source_data/R/PREDICT/2018/National Collection/"

files.list <- list.files(files.path, pattern="PUBLIC")

ALL_PUBLIC <- rbindlist(lapply(files.list, function(x)
  read.fst(paste0(files.path, x), 
           as.data.table = T)))

ALL_PUBLIC <- ALL_PUBLIC[VSIMPLE_INDEX_MASTER %in% Baseline$VSIMPLE_INDEX_MASTER][
  , study_index_date := Baseline$view_visit_date[match(VSIMPLE_INDEX_MASTER, Baseline$VSIMPLE_INDEX_MASTER)]] # First PREDICT assessment

HX_EVENTS <- copy(ALL_PUBLIC)[EVSTDATE <= study_index_date]

hx.gest.diabetes <- "^O2441"
hx.polycys.ovary <- "^E282"

history.vars <- ls(pattern = "^hx.")

for(var in history.vars){
  
  codes <- get(var)
  
  EVENTS <- HX_EVENTS[, hx_event:= +(grepl(paste(codes, collapse = "|"), CLIN_CD_10))]
  
  EVENTS <- EVENTS[hx_event==1, 
                   unique(.SD),
                   .SDcols = c("VSIMPLE_INDEX_MASTER", "hx_event")]
  
  ALL_HX_VARS <- if(var == history.vars[1]){
    
    merge(Baseline[,.(VSIMPLE_INDEX_MASTER)], EVENTS,
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

Baseline <- merge(Baseline, ALL_HX_VARS,
                  by = "VSIMPLE_INDEX_MASTER",
                  all.x = T)

# Save
write.fst(Baseline, "source_data/R/PREDICT/2018/Working/PP_Link_EndPart_DM4.fst", 75)

rm(list = setdiff(ls(), "Baseline"))



# ---- Antipscyhotic Drugs & Corticosteroids ----

ALL_PHARMS <- read.fst("source_data/R/PREDICT/2018/Working/PP_Link_ALL_PHARMS_v1.fst",
                       as.data.table = T)

DIM_FORM_PACK <- read.fst("V:/common_lookups/CURRENT_PHARMS_LOOKUP_VIEW.fst", 
                          as.data.table = T)

corticosteroid <- unique(DIM_FORM_PACK$DIM_FORM_PACK_SUBSIDY_KEY[which(DIM_FORM_PACK$corticosteroid==1)])

ANTI_PSYCHOTICS <- readxl::read_xlsx("common_lookups/Definitions/antipsychotics_ valproate_clozapine_olanzapine.xlsx", col_names = F)

var.names <- c("DIM_FORM_PACK_SUBSIDY_KEY", "CHEMICAL_ID", "CHEMICAL_NAME", "TG_NAME1", "TG_NAME2","TG_NAME3", 
               "FORMULATION_ID", "FORMULATION_NAME", "NA1", "NA2", "type")

setnames(ANTI_PSYCHOTICS, names(ANTI_PSYCHOTICS), var.names)

atypical_oral   <- unique(ANTI_PSYCHOTICS$DIM_FORM_PACK_SUBSIDY_KEY[which(ANTI_PSYCHOTICS$type==1)])
typical_oral    <- unique(ANTI_PSYCHOTICS$DIM_FORM_PACK_SUBSIDY_KEY[which(ANTI_PSYCHOTICS$type==2)])
atypical_depot  <- unique(ANTI_PSYCHOTICS$DIM_FORM_PACK_SUBSIDY_KEY[which(ANTI_PSYCHOTICS$type==3)])
typical_depot   <- unique(ANTI_PSYCHOTICS$DIM_FORM_PACK_SUBSIDY_KEY[which(ANTI_PSYCHOTICS$type==4)])
lithium         <- unique(ANTI_PSYCHOTICS$DIM_FORM_PACK_SUBSIDY_KEY[which(ANTI_PSYCHOTICS$type==5)])
navalproate     <- unique(ANTI_PSYCHOTICS$DIM_FORM_PACK_SUBSIDY_KEY[which(ANTI_PSYCHOTICS$type==6)])
clozapine       <- unique(ANTI_PSYCHOTICS$DIM_FORM_PACK_SUBSIDY_KEY[which(ANTI_PSYCHOTICS$type==11)])
olanzapine      <- unique(ANTI_PSYCHOTICS$DIM_FORM_PACK_SUBSIDY_KEY[which(ANTI_PSYCHOTICS$type==12)])
olanzapine_dept <- unique(ANTI_PSYCHOTICS$DIM_FORM_PACK_SUBSIDY_KEY[which(ANTI_PSYCHOTICS$type==32)])
ruth_exclusion  <- unique(ANTI_PSYCHOTICS$DIM_FORM_PACK_SUBSIDY_KEY[which(ANTI_PSYCHOTICS$type==99)])

atypical_any    <- c(atypical_oral, atypical_depot)
typical_any     <- c(typical_oral, typical_depot) 
olanzapine_any  <- c(olanzapine, olanzapine_dept) 

all.mx.groups <- c("corticosteroid", "atypical_any", "typical_any", "lithium", "navalproate", "clozapine", "olanzapine_any", "ruth_exclusion")

# Standard Drug Capture 
ALL_PHARMS[, study_index_date := Baseline$view_visit_date[match(VSIMPLE_INDEX_MASTER, Baseline$VSIMPLE_INDEX_MASTER)]]

source("V:/bwu009/My Projects/Global Procedures/Standard Pharms Procedure - Primary Prevention.R")

# Merge
Baseline <- merge(Baseline, ALL_DRUG_VARS,
                  by = "VSIMPLE_INDEX_MASTER",
                  all.x = T)

write.fst(Baseline, "source_data/R/PREDICT/2018/Working/PP_Link_EndPart_DM5.fst", 75)


# Save to Stata
library(haven)

write_dta(Baseline, "rpyl001/Datasets/PREDICT2018_DM_PP_v2.dta", version = 14)





