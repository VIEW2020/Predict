
# -- National Health Collection Data Preparation --
# --              for PREDICT 2018 v1            --

# Last updated August 2019

# Preparation for 2018 PREDICT COHORT v1 using National Data till end of 2018
# Read-in using .fst files
# Diagnosis and events data are merged
 
# NB - In the first instance, data is copied a local temp directory. This is done for speed and efficiency.
#      The data is the subsequently copied to i) VIEW Data Drive and ii) VIEW General Virtual Machine

library(data.table, quietly = T)
library(fst, quietly = T)

PREDICT <- read.fst("source_data/R/PREDICT/2018/Cleaned_PREDICT_2018_1st_Record_v1.fst", as.data.table = T)


# ---- A.   Public Hospitalisation ----

files.path <- c("source_data/R/HOSPITALISATION/SIMPLE/")

# 1. Year Extraction
files.list <- list.files(files.path, pattern="EVENTS")

for(file in files.list){
   
   EVENTS <- read.fst(paste0(files.path, file), as.data.table = T)
   EVENTS <- EVENTS[VSIMPLE_INDEX_MASTER %in% PREDICT$VSIMPLE_INDEX_MASTER]
   
   # To avoid events with ultra long LOS, remove records where EVSTDATE occurs >=2 years prior.
   # This cleaning process incurs very little record loss (eg. 23 / 161079 in 2018) and allows for a tidier dataset
   year <- as.numeric(unlist(strsplit(file, 
                                      fixed = F, 
                                      split = "[[:punct:]]"))[3])
   
   if(grepl("1988", file)){
      EVENTS <- EVENTS[year(EVSTDATE)==year]
   } else {
      EVENTS <- EVENTS[year(EVSTDATE)==year | year(EVSTDATE)==year-1]
   }
   
   file.diag <- gsub("EVENTS", "DIAGS", file)
   
   DIAGS <- read.fst(paste0(files.path, file.diag), as.data.table = T)
   DIAGS <- DIAGS[EVENT_ID %in% EVENTS$EVENT_ID, 
                  .(EVENT_ID, DIAG_TYP, DIAG_SEQ, CLIN_CD_10)]
   
   # Merge
   setkey(EVENTS, EVENT_ID)
   setkey(DIAGS, EVENT_ID)
   
   PUBLIC <- DIAGS[EVENTS, nomatch = 0]
   
   setcolorder(PUBLIC, 
               c("VSIMPLE_INDEX_MASTER", names(PUBLIC)[!grepl("VSIMPLE_INDEX_MASTER", names(PUBLIC))]))
   
   file.name <- paste0("VSIMPLE_PUBLIC_", year, "_v1.fst")
   
   write.fst(PUBLIC, paste0("D:/temp/", file.name), 75)
   
   print(paste0(file, " completed")); 
   
}


# ---- B.   Private Hospitalisation ----

PRIV_EVENTS <- read.fst("V:/source_data/R/HOSPITALISATION/PRIVATE/VSIMP_PRIVATE_EVENTS_2004_2017.fst", as.data.table = T)
PRIV_EVENTS <- PRIV_EVENTS[VSIMPLE_INDEX_MASTER %in% PREDICT$VSIMPLE_INDEX_MASTER]
   
PRIV_DIAGS <- read.fst("V:/source_data/R/HOSPITALISATION/PRIVATE/VSIMP_PRIVATE_DIAGS_2004_2017.fst", as.data.table = T)
PRIV_DIAGS <- PRIV_DIAGS[EVENT_ID %in% PRIV_EVENTS$EVENT_ID, .(EVENT_ID, CC_SYS, DIAG_TYP, DIAG_SEQ, CLIN_CD, CLIN_CD_10)]
   
# i.  Bundle Private Hospital Events
#     i) swap ENHI to numeric ID, ii) setkey to numeric ID
#       Remember to assign ID BY GROUP!
source("V:/bwu009/My Projects/Generic Functions/Hospitalisation Bundling v2.R")

# a. Organise data
PRIV_EVENTS_BUNDLED <- copy(PRIV_EVENTS)
PRIV_EVENTS_BUNDLED <- PRIV_EVENTS_BUNDLED[order(VSIMPLE_INDEX_MASTER)
                                           , ID := .GRP
                                           , by=VSIMPLE_INDEX_MASTER]

setkey(PRIV_EVENTS_BUNDLED, EVSTDATE)

# ii.  Apply function
PRIV_EVENTS_BUNDLED[, BUNDLE:=Bundle_Hosp_Dates(.SD)
                    , by=ID]

# d. Capture bundle admissions / discharge
vars <- c("BUNDLE_ADM_DATE", "BUNDLE_DIS_DATE")

PRIV_EVENTS_BUNDLED[, by=list(VSIMPLE_INDEX_MASTER, BUNDLE)
                    , (vars) := .(min(EVSTDATE)[1],
                                  max(EVENDATE)[1])]

# Merge
setkey(PRIV_EVENTS_BUNDLED, EVENT_ID)
setkey(PRIV_DIAGS, EVENT_ID)

PRIVATE <- PRIV_DIAGS[PRIV_EVENTS_BUNDLED, nomatch = 0]

setcolorder(PRIVATE, 
            c("VSIMPLE_INDEX_MASTER", names(PRIVATE)[!grepl("VSIMPLE_INDEX_MASTER", names(PRIVATE))]))

# Export 
write.fst(PRIVATE, "D:/temp/VSIMPLE_PRIVATE_v1.fst", 75)

# ---- C.   Mortality Collection ----

library(data.table)
library(fst)

MORTALITY <- read.fst("source_data/R/MORTALITY/VSIMPLE_MORTALITY_DEC2017_v2.fst"); setDT(MORTALITY)

MORTALITY <- MORTALITY[VSIMPLE_INDEX_MASTER %in% PREDICT$VSIMPLE_INDEX_MASTER
                       ,.(VSIMPLE_INDEX_MASTER,CC_SYS,ICDD,ICDF1,ICDF2,ICDF3,ICDF4,ICDF5,ICDF6)]

write.fst(MORTALITY, "D:/temp/VSIMPLE_DEATHS_v1.fst", 75)



# ---- D.   Pharms Dispensing -----
files.path <- "V:/source_data/R/PHARMS/SIMPLE/"

# Yearly File
for(year in 2005:2018){
   
   files.list <- list.files(files.path, pattern=paste0(year))
   
   DATA <- rbindlist(lapply(files.list, 
                               function(x){
                                  DATA <- read.fst(paste0(files.path, x), as.data.table = TRUE)
                                  DATA[VSIMPLE_INDEX_MASTER %in% PREDICT$VSIMPLE_INDEX_MASTER]
                               }))
   
   DATA[, c("FUNDING_DHB_CODE","MESH_BLOCK_2006") := NULL]
   
   file.name <- paste0("VSIMPLE_PHH_", year, "_v1.fst")
   
   write.fst(DATA, paste0("D:/temp/", file.name), 75)
   
   print(paste0(file.name, " completed")); rm(DATA)
}

# ---- E.   TestSafe -----
for(type in c("HBA1C", "TCHDL", "SCR")){
   
   files.path <- paste0("V:/source_data/R/TESTSAFE/", type, "/")
   files.list <- list.files(files.path, pattern = ".fst")
   files.list <- grep("2004_2018", files.list, invert = T, value = T)
   
   for(file in files.list){
      
      DATA <- read.fst(paste0(files.path, file), as.data.table = T)
      DATA <- DATA[VSIMPLE_INDEX_MASTER %in% PREDICT$VSIMPLE_INDEX_MASTER]
      
      file.name <- gsub("VSIMP", "VSIMPLE", file)
      
      write.fst(DATA, paste0("D:/temp/", file.name), 75)
      
      print(paste0(file.name, " completed")); rm(DATA)
      
   }
   
}

# Lipids
files.path <- paste0("V:/source_data/R/TESTSAFE/LIPIDS/")
files.list <- list.files(files.path, pattern = ".fst")
files.list <- grep("2004_2018", files.list, invert = T, value = T)

for(file in files.list){
   
   DATA <- read.fst(paste0(files.path, file), as.data.table = T)
   DATA <- DATA[VSIMPLE_INDEX_MASTER %in% PREDICT$VSIMPLE_INDEX_MASTER]
   
   file.name <- gsub("VSIMP", "VSIMPLE", file)
   
   write.fst(DATA, paste0("D:/temp/", file.name), 75)
   
   print(paste0(file.name, " completed")); rm(DATA)
   
}
   


