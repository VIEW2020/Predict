
# -- 2.  Invalid Data Removal 2019 v1 --

library(data.table)

ALL_PREDICT <- fst::read.fst("source_data/R/PREDICT/2019/ALL_PREDICT_Part_1.fst", as.data.table = T) # Starting 571586 individuals

# ---- A. Remove Invalid Entries ----

# 1.  Not Real Data or is an exception
# 2.  Not Primary Care Data
# 3.  Invalid PHOs
# 4.  Invalid Provider IDs
# 5.  Invalid Client IPs: Addresses associated with the Engima head office
# 6.  Invalid Health Status: Pregnant or have gestational diabetes
# 7.  Invalid acccessment: No CVD Risk generated

# Labels to drop
real.value <- 0
runexc.val <- 1
preg.value <- 1
diab.value <- 4

ip.address <- c("103.5.74.12", "43.254.20.130", "43.254.20.129", "210.48.114.90", "203.167.192.113")

table.name <- c("CMDHB_ACUTE_CVDDM", "WAIKATO_ACUTE_CVDDM")

unreal.pho <- c("Enigma Demo PHO","Enigma Test PHO","Placeholder","Enigma PHO","DHB User", "SuperUser (Enigma)", "Taranaki Reviewers", 
                "Cardiology Clinics", "Middlemore Hospital Admin", "Waikato Clinic Groups", "Cardiology Clinics", "Rotorua Reviewers")

unreal.pid <- grep("test|demo|^chris$|^nick$|practice|enigma", tolower(ALL_PREDICT$pt_provider_id), value = T)

no.cvdrisk <- NA


# Tag records for removal
ALL_PREDICT[, removal := +(pt_datareal_1 %in% real.value |
                             pt_runexception %in% runexc.val |
                             pt_tablename %in% table.name |
                             pt_pho_name %in% unreal.pho |
                             tolower(pt_provider_id) %in% unreal.pid |
                             pt_client_ip_addr %in% ip.address |
                             pt_pregnant %in% preg.value |
                             pt_diabetes %in% diab.value |
                             pt_cvd_risk %in% no.cvdrisk)]

ALL_PREDICT <- ALL_PREDICT[removal==0] # -6843 / 564743 remaining


# ---- B. Removes Duplicate Data ----

# 1.  Preparation

#     Remove redundant and administrative variables 
#     nb: includes variables with only 1 level
pt.admin.vars <- unique(c(names(ALL_PREDICT)[startsWith(names(ALL_PREDICT), "pt__")],
                             names(ALL_PREDICT)[endsWith(names(ALL_PREDICT), "_diff")],
                             "pt_client_ip_addr", "pt_hp_id", "pt_pmsver", "pt_provider_id", "pt_remaining_item_count", "pt_submitted_by", "pt_submitted",
                             "pt_datareal_1", "pt_submissions_type", "pt_runexception", "pt_pregnant"))

pt.demo.vars <- c(names(ALL_PREDICT)[startsWith(names(ALL_PREDICT), "nhi_")],
                  "pt_age", "pt_dob", "pt_ethnic_group_1", "pt_ethnic_group_2", "pt_ethnic_group_3",
                  "pt_gender", "pt_geocode", "pt_nzdep", "pt_dhbcatchment")
           
pt.other.vars <- c("VSIMPLE_INDEX_2NDARY", "pt_pho_name", "pt_useforqa", "removal")   
                         
DATA <- copy(ALL_PREDICT)[, -c(pt.admin.vars, pt.demo.vars, pt.other.vars), with = F] 


#     Identify multiple instances
DATA <- DATA[order(view_visit_date)][, by = "VSIMPLE_INDEX_MASTER"
                                     , instance_count := max(seq_len(.N))]

SINGLE_VISIT <- copy(DATA)[instance_count==1, -"instance_count", with = F]
MULTI_VISITS <- copy(DATA)[instance_count>1, -"instance_count", with = F]

LIST_MULTI <- split(MULTI_VISITS, by = "pt_tablename")

#  Select variables to test for duplicates
pt.run.vars  <- names(ALL_PREDICT)[startsWith(names(ALL_PREDICT), "pt_run")]

pt.check.vars <- c(pt.run.vars, "pt_cvd_risk", "pt_pure_framingham", "pt_submissions_timestamp", "pt_submissions_id")

data.for.dups <- names(DATA)[which(!names(MULTI_VISITS) %in% pt.check.vars)]

# Check all submission IDs in each table are unique
lapply(LIST_MULTI, function(x) nrow(x) == uniqueN(x$pt_submissions_id))

LIST_MULTI <- lapply(LIST_MULTI, function(x)
  x[, dup := +(.N>1), by = data.for.dups]
)

# # Example: VS07ApOSumzj from ProCare
# lapply(LIST_MULTI, function(x) sum(x$dup))
# 
# TEST <- as.data.table(LIST_MULTI$ProCare)[VSIMPLE_INDEX_MASTER %in% "VS07ApOSumzj", 
#                                           .(VSIMPLE_INDEX_MASTER,view_visit_date, pt_submissions_timestamp,pt_submissions_id,
#                                             pt_runcvd_management,pt_rundiab_management,pt_cvd_risk,dup)]


# Notes - The data might be duplicate but a new record is created hence non-duplicate submissions timestamp, and submissions ID.
#       - In most cases, a new record is created minutes later in which a runcvd or rundiabetes takes place
#       - The "keeper" should be the runcvd record - meaning we drop the earlier 'assessment only' record
#       - If there are multiple records in each day, then keep the record containing the runcvd management
#       - The cvdrun flag could be makred on both, neither, just one of the duplicate records. 

# Order of operation - first get rid of genuine duplicates and prioritise cvdrun records!
#                    - then reduce multiple daily entries to one record per day and prioritise cvdrun again! 

LIST_MULTI <- lapply(1:length(LIST_MULTI), function(i){
   
   DAT <- LIST_MULTI[[i]]; print(paste("Processing table", i))
   
   dups <- DAT[dup==1][order(-pt_runcvd_management, -pt_submissions_id), 
                       index := seq_len(.N),
                       by = list(VSIMPLE_INDEX_MASTER, view_visit_date)][index==1, -"index", with = F]
   
   DAT <- rbind(dups, DAT[dup==0])[order(view_visit_date, pt_submissions_id), -"dup", with = F]
   
   # If there are still records on same day, then they are not duplicates. 
   # Take the latest submission in which runcvd occured.
   DAT <- DAT[order(-pt_runcvd_management, -pt_submissions_id), 
              index := seq_len(.N),
              by = list(VSIMPLE_INDEX_MASTER, view_visit_date)][index==1, -"index", with = F]
   
   LIST_MULTI[[i]] <- DAT
   
})

# Recombine
# nb: Should be no loss in unique individuals: 564743 people remains but 350277 records removed
DATA <- rbind(SINGLE_VISIT, do.call("rbind", LIST_MULTI))

ALL_PREDICT <- merge(ALL_PREDICT[,c("VSIMPLE_INDEX_MASTER", "pt_submissions_id", "pt_tablename", pt.demo.vars, pt.other.vars), with = F],
                     DATA,
                     by = c("VSIMPLE_INDEX_MASTER", "pt_submissions_id", "pt_tablename"),
                     all.y = F)


fst::write.fst(ALL_PREDICT, "source_data/R/PREDICT/2019/ALL_PREDICT_Part_2.fst", 75)
