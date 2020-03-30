
# ---5. Final Preparations and First Assessment ---

library(fst)
library(data.table)
library(dplyr)

ALL_PREDICT <- read.fst("source_data/R/PREDICT/2018/ALL_PREDICT_Part_4.fst", as.data.table = T)

DATA <- copy(ALL_PREDICT)


# ---- A. Final Variable Selection ----

# i.   Reorder
DATA <- setcolorder(DATA, order(names(DATA))) %>%
   select(VSIMPLE_INDEX_MASTER, VSIMPLE_INDEX_2NDARY, view_visit_date, 
          starts_with("view"),
          starts_with("nhi"),
          starts_with("pt"),
          starts_with("pt_en"))

DATA <- DATA[view_visit_date <= "2018-12-31"]

# ii.  Ensure DOD does not exceed end of study (2018-12-31)
#     Version 5.1 uses AUG 2019 NHI Update. Overwrites 8977 records to NA.
DATA[nhi_en_dod>"2018-12-31", nhi_en_dod:=NA]

setnames(DATA, "nhi_en_dod", "view_ag_dod")


# iii.  Exclude from core variables

# Global variable removal
# 1)  Remove variables that have been superceded with an enhanced version
# 2)  Remove administrative variables
# 3)  Remove other redudant variables

superceded.vars <- c("pt_ace_inhibitor", "pt_aspirin", "pt_at2", "pt_beta_blocker", "pt_bmi", "pt_calcium_antagonist", "pt_clopidogrel", 
                     "pt_diab_acr", "pt_diab_acrdate", "pt_diab_diet_referral", "pt_diab_edu_referral", "pt_diab_eye_lastret", 
                     "pt_diab_feet_date_last_check", "pt_ethnic_group_1", "pt_ethnic_group_2", "pt_ethnic_group_3", "pt_fibrate", "pt_glucose", 
                     "pt_glucosedate", "pt_hba1c_mm", "pt_hba1c_mmdate", "pt_hba1cdate", "pt_hdl", "pt_hdldate", "pt_height", "pt_last_diet_check", 
                     "pt_ldl", "pt_ldldate", "pt_other_hyp_drugs", "pt_other_lipid_drugs", "pt_referral_diet_given", 
                     "pt_serum_creatinine", "pt_serum_creatininedate", "pt_statin","pt_tchdl_ratio", "pt_tchdl_ratiodate", 
                     "pt_tcl", "pt_tcldate", "pt_thiazide", "pt_warfarin", "pt_weight", "pt_hba1c")

bloodpress.vars <- c("pt_bpd", "pt_bpd2", "pt_bps", "pt_bps2")

demographic.vars <- c("pt_age", "pt_dob", "pt_geocode", "pt_en_ethnic_group_1", "pt_en_ethnic_group_2", "pt_en_ethnic_group_3", 
                      "pt_gender", "pt_nzdep", "nhi_gender", "nhi_en_dob", "pt_dhbcatchment")

extra.remov.vars <- c("pt_serum_creatinine_umol", "pt_weightdate", "pt_cvd_risk",  "pt_first_screened", "pt_waist", "pt_blind", 
                      "pt_green_pres", "pt_phy_active", "pt_pho_name", "pt_en_referral_diet_given", "pt_en_last_diet_check", 
                      "pt_en_height", "pt_en_weight", "pt_smk_quit", "pt_diabetes_yrs_int", "pt_useforqa",
                      "pt_rundiab_eyes", "pt_rundiab_feet", "pt_metabolic_syndrome")

DATA <- DATA[, -c(superceded.vars, bloodpress.vars, demographic.vars, extra.remov.vars), with = F]


# ---- B. Observation Splitting ----  

# 1.  Remove pure duplicates
#     There are 50833 records that have been duplicated across two or more tablenames. 
#     While these records have different submission ID & tablenames, they share identical information across >95 variables.

data.vars <- names(DATA)[which(!names(DATA) %in% c("pt_submissions_id", "pt_tablename"))]

DATA <- DATA[, unique(.SD), 
             .SDcols = data.vars] # -50833

# 2.  Reduce multiple observations that occur on the same day, down to one per day
#     There are 10393 runcvd records that occur on same day as assessments
#     Nb: Order is to prioritise records that are runcvd

DATA <- DATA[order(-pt_runcvd_management, -pt_submissions_timestamp), 
             index := seq_len(.N),
             by = list(VSIMPLE_INDEX_MASTER, view_visit_date)][index == 1, -"index", with = F] # -10417 

# 3.  Sequence all visits
#     Detect runcvd management within 30 days of assessment
#     i. determine number of days to the next assessment
#     ii. mark sequence position where assessment preceeds cvdrun management

time.seq.vars <- c("days_to_next", "ass_to_mgt")

DATA[order(view_visit_date), 
     (time.seq.vars) := list({
             
             daysdiff <- as.numeric(shift(view_visit_date, type = "lead") - view_visit_date)
             replace(daysdiff, 
                     is.na(daysdiff), 
                     0)},
             
             +(zoo::rollapply(as.numeric(pt_runcvd_management),  
                              2, identical, c(0, 1), fill = FALSE))
             
     ), by = VSIMPLE_INDEX_MASTER]

# Benchmark: 5.537998 mins

# 4.  Remove records the meet the criteria
#     18838 assessment records removed where the subsequent record is a runcvd management which concurred within 30 days of the assessment
      # sum(DATA$days_to_next <30 & DATA$ass_to_mgt==1)
DATA <- DATA[days_to_next >= 30 | ass_to_mgt == 0][,-time.seq.vars, with = F]

# Sequence visit
DATA[order(view_visit_date), 
            view_visit_seq := seq_len(.N)
            , by = VSIMPLE_INDEX_MASTER]

ALL_PREDICT <- DATA %>%
   select(VSIMPLE_INDEX_MASTER, view_visit_date, view_visit_seq, 
          starts_with("view"),
          starts_with("pt"))

# Save
fst::write.fst(ALL_PREDICT, "source_data/R/PREDICT/2018/Cleaned_PREDICT_2018_All_Records_v1.fst", 75)

FIRST_PREDICT <- ALL_PREDICT[view_visit_seq==1]

fst::write.fst(FIRST_PREDICT, "source_data/R/PREDICT/2018/Cleaned_PREDICT_2018_1st_Record_v1.fst", 75)
