
# -- 4. Demographic variables 2019 v1 --

library(data.table)
library(lubridate)

ALL_PREDICT <- fst::read.fst("source_data/R/PREDICT/2018/ALL_PREDICT_Part_3.fst", as.data.table = T)

NHI <- fst::read.fst("source_data/R/NHI/VSIMPLE_NHI_LOOKUP_AUG2019.fts", as.data.table = T)

# Starting 1628059 observations / 564743 individuals 


# ---- A.   Age / Gender / DOD ----

# D1.   Age & DOB
# NB:   Using the agree NHI_DOB to generate an agreed age
#       If NHI Gender is unknown ("U"), then use pt_gender
ALL_PREDICT[, c("view_ag_dob", 
                "view_ag_age") := list(nhi_en_dob,
                                       as.numeric(floor((view_visit_date - nhi_en_dob) / 365.25)))]

#   Remove people < 18 years and > 110 years old 
#   NB: Removes 9 people, 564734 remaining
ALL_PREDICT <- ALL_PREDICT[view_ag_age>=18 & view_ag_age <=110]


# D2.   Gender 
#       Remove people whose gender is different in NHI and PREDICT
#       NB: Do no try to resolve this as gender is a major variable and there is no way to know which record is correct.
#           It is better to assume that its different people sharing the same NHI. 
#           Removes 399 people (564335 remaining)
ALL_PREDICT[, nhi_gender := NHI$GENDER[match(VSIMPLE_INDEX_2NDARY, NHI$VSIMPLE_INDEX_2NDARY)]]

ALL_PREDICT <- ALL_PREDICT[pt_gender == nhi_gender][, view_ag_sex := nhi_gender]


# D3.   Date of death
#       Remove records where NHI_DOD occured before PREDICT visit (removes 147 people, 564188 remaining)
ALL_PREDICT[, nhi_en_dod := NHI$EN_DOD[match(VSIMPLE_INDEX_2NDARY, NHI$VSIMPLE_INDEX_2NDARY)]]

ALL_PREDICT <- ALL_PREDICT[nhi_en_dod > view_visit_date | is.na(nhi_en_dod)]


# ---- B.   Ethnicity ----

# The following steps are ethnicity taken from PREDICT only
eth.vars <- c("pt_ethnic_group_1", "pt_ethnic_group_2", "pt_ethnic_group_3")
en.eth.vars <- gsub("pt", "pt_en", eth.vars)
  
DATA <- ALL_PREDICT[order(view_visit_date)][, by = "VSIMPLE_INDEX_MASTER"
                                            , instance_count := max(seq_len(.N))]

SINGLE_VISIT <- copy(DATA)[instance_count==1, -"instance_count", with = F]
MULTI_VISITS <- copy(DATA)[instance_count>1, -"instance_count", with = F]

# For Single-visit records
# NB: 54/61 (other), 90-99 (unknown)
SINGLE_VISIT[, (en.eth.vars) := lapply(.SD, function(x)
   
   replace(x, list = which(is.na(x) | x %in% c(54,61,94,95,97)), 
           values = 99)
), .SDcols = eth.vars]

# For multi-visit records, remove codes 54/61 (other), 90-99 (unknown)
MULTI_VISITS[, (en.eth.vars) := lapply(.SD, function(x)
   
   replace(x, list = which(x %in% c(54,61,90:99)), 
           values = NA)
), .SDcols = eth.vars]

# Use zoo::locf method for carrying values forward/backward
MULTI_VISITS[order(view_visit_date),
             by = VSIMPLE_INDEX_MASTER,
             (en.eth.vars) := lapply(.SD, function(x){
                
                x <- zoo::na.locf(x, na.rm = FALSE)
                x <- zoo::na.locf(x, fromLast = T)
                return(x)
             }), .SDcols = en.eth.vars]


MULTI_VISITS[, (en.eth.vars) := lapply(.SD, function(x)
   
  replace(x, list = which(is.na(x) | x %in% c(54,61,94,95,97)), 
          values = 99)
  ), .SDcols = en.eth.vars]

DATA <- rbind(SINGLE_VISIT, MULTI_VISITS)


# Aggregate Ethnicity
euro    <- c(10,11,12)
maori   <- 21
pacific <- c(30:37)
fijian  <- 36
oth.asn <- c(40,41,44, 442:444, 44411:44415)
chinese <- 42
indian  <- c(43, 441)
melaa   <- c(51,52,53)
other   <- 99 

# Prioritised Ethnicity
# nb: as prioritisation is currently the preferred method, this variable will be used as view_ag_eth
library(fastmatch)
library(future.apply); plan(multiprocess, workers = 8) 

DATA[, pt_en_prtsd_eth := future_mapply(function(x,y,z){
  
  v <- c(x,y,z)
  
  if(all(v == 99)){
    return(9)
  } else {
    dplyr::case_when(
      any(v %fin% maori) ~ 2,
      any(v %fin% fijian) & any(v %in% indian) ~ 43,
      any(v %fin% pacific) ~ 3,
      any(v %fin% indian) ~ 43,
      any(v %fin% chinese) ~ 42,
      any(v %fin% oth.asn) ~ 4,
      any(v %fin% melaa) ~ 5,
      TRUE ~ 1)
  }
}, x = pt_en_ethnic_group_1, y = pt_en_ethnic_group_2, z = pt_en_ethnic_group_3)]

DATA[, pt_en_prtsd_eth := view_ag_eth]


# Sole ethnicity
DATA[, pt_en_sole_eth := lapply(.SD, function(x){
  
  x <- replace(x, which(x %in% euro), 1)
  x <- replace(x, which(x %in% maori), 2)
  x <- replace(x, which(x %in% pacific), 3)
  x <- replace(x, which(x %in% oth.asn), 4)
  x <- replace(x, which(x %in% chinese), 42)
  x <- replace(x, which(x %in% indian), 43)
  x <- replace(x, which(x %in% melaa), 5)
  x <- replace(x, which(x %in% other), 9)
  
  return(x)
}), .SDcols = "pt_en_ethnic_group_1"]


# Sole / Combination Ethnicity
DATA[, pt_en_solecombo_eth := future_mapply(function(x,y,z){
   
   v <- c(x,y,z)
   v <- v[!v %in% 99]
   
   if(length(v)==0){
      return(9)
   } else {
      dplyr::case_when(
         all(v %fin% euro) ~ 1,
         all(v %fin% maori) ~ 2,
         all(v %fin% pacific) ~ 3,
         all(v %fin% oth.asn) ~ 4,
         all(v %fin% chinese) ~ 42,
         all(v %fin% indian) ~ 43,
         all(v %fin% melaa) ~ 5,
         TRUE ~ 99)
   }}, 
   x = pt_en_ethnic_group_1, 
   y = pt_en_ethnic_group_2, 
   z = pt_en_ethnic_group_3)]

DATA[pt_en_solecombo_eth == 99, 
     pt_en_solecombo_eth := future_mapply(function(x,y,z){
        
        v <- c(x,y,z)
        v <- v[!v %in% 99]
        
        dplyr::case_when(
           any(v %fin% fijian) & any(v %fin% indian) ~ 3,
           any(v %fin% maori) & any(v %fin% euro) ~ 2.1,
           any(v %fin% maori) & any(v %fin% pacific) ~ 2.3,
           any(v %fin% pacific) & any(v %fin% euro) ~ 3.1,
           length(v)==2 ~ 9.2,
           TRUE ~ 99)
     }, 
     x = pt_en_ethnic_group_1, 
     y = pt_en_ethnic_group_2, 
     z = pt_en_ethnic_group_3)]


# Prioritised ethnicity across NHI and PREDICT
# The ethnic distributions from NHI a similar though different to PREDICT (which has less missingness). 
# The NHI ethnicity coding is only used as an improvement to form "view_ag_eth".
DATA[, nhi_en_prtsd_eth := NHI$EN_PRTSD_ETH[match(VSIMPLE_INDEX_2NDARY, NHI$VSIMPLE_INDEX_2NDARY)]]

DATA[, view_ag_eth := future_mapply(function(x,y){
  
  v <- c(x,y)
  
  if(all(v == 9)){
    return(9)
  } else {
    dplyr::case_when(
      any(v %fin% 2) ~ 2,
      any(v %fin% 3) ~ 3,
      any(v %fin% 43) ~ 43,
      any(v %fin% 42) ~ 42,
      any(v %fin% 4) ~ 4,
      any(v %fin% 5) ~ 5,
      TRUE ~ 1)
  }
}, 
x = nhi_en_prtsd_eth, 
y = pt_en_prtsd_eth)]

ALL_PREDICT <- copy(DATA)

# fst::write.fst(ALL_PREDICT, "source_data/R/PREDICT/2018/ALL_PREDICT_Part_4.fst", compress = 75)


# ---- C.   Meshblock & NZDep Score  ----

# Direct method - keep only pt_nzdep 1 - 5
library(fastmatch)
library(future.apply); plan(multiprocess, workers = 8) 

LOOKUP <- read.fst("source_data/R/LOOKUP TABLES/MB NZDep/PHO_EN_MESHBLOCK_2004_2019.fst")

# Preparation
setkey(ALL_PREDICT, VSIMPLE_INDEX_2NDARY)
setkey(NHI, VSIMPLE_INDEX_2NDARY)

INDEX <- NHI[,.(VSIMPLE_INDEX_2NDARY, DEP01, DEP06, DEP13)][ALL_PREDICT[,.(VSIMPLE_INDEX_MASTER, VSIMPLE_INDEX_2NDARY, 
                                                                           view_visit_date, pt_submissions_id, pt_tablename)], nomatch = 0]

INDEX[, visit_year_quarter := paste0("MB_", year(view_visit_date), "_Q", quarter(view_visit_date))]


# i.  Find Meshblock 

# Capture MB using latest Meshblock lookup table
LIST <- split(INDEX, f = INDEX$visit_year_quarter)

LIST <- lapply(names(LIST), function(x){
   
   LOOKUP <- LOOKUP[,c("VSIMPLE_INDEX_MASTER",x)]
   names(LOOKUP)[2] <- "EN_MESHBLOCK"
   # browser()
   DAT <- as.data.table(LIST[[x]])
   
   DAT$EN_MESHBLOCK <- LOOKUP$EN_MESHBLOCK[match(DAT$VSIMPLE_INDEX_MASTER, LOOKUP$VSIMPLE_INDEX_MASTER)]
   
   return(DAT)
})

INDEX <- do.call("rbind", LIST)


# ---- ii.  Find NZDEP ----

# Find deprivation score using MB data
MB2001 <- readRDS("common_lookups/MB NZDep/MB2001_NZDep01_Concordance.rds")
MB2013 <- readRDS("common_lookups/MB NZDep/MB0613_NZDep0613_Concordance.rds")

INDEX[, visit_year := year(view_visit_date)]

LIST <- split(INDEX, f = INDEX$visit_year)

LIST <- lapply(names(LIST), function(x){
   
   DAT <- as.data.table(LIST[[x]])
   
   if(as.numeric(x) <= 2006){
      DAT[, view_en_nzdep := as.numeric(MB2001$NZDep2001[match(EN_MESHBLOCK, MB2001$MB_num_2001)])]
   } else if(as.numeric(x) %in% 2007:2013){
      DAT[, view_en_nzdep := as.numeric(MB2013$NZDep2006[match(EN_MESHBLOCK, MB2013$MB_2006)])]
   } else {
      DAT[, view_en_nzdep := as.numeric(MB2013$NZDep2013[match(EN_MESHBLOCK, MB2013$MB_2013)])]
   }
   
   return(DAT)
      
})

INDEX <- do.call("rbind", LIST)

# Some NZDep still missing
# Use different index to find MB match
INDEX[INDEX[,.I[is.na(view_en_nzdep)]],
      view_en_nzdep := as.numeric(MB2013$NZDep2006[match(EN_MESHBLOCK, MB2013$MB_2006)])]

INDEX[INDEX[,.I[is.na(view_en_nzdep)]],
      view_en_nzdep := as.numeric(MB2013$NZDep2013[match(EN_MESHBLOCK, MB2013$MB_2013)])]

# If still missing, use NHI DATA
INDEX[visit_year %in% 2004:2006 & is.na(view_en_nzdep), view_en_nzdep := as.numeric(DEP01)]
INDEX[visit_year %in% 2007:2013 & is.na(view_en_nzdep), view_en_nzdep := as.numeric(DEP06)]
INDEX[visit_year >= 2014 & is.na(view_en_nzdep), view_en_nzdep := as.numeric(DEP13)]

# Quintile
INDEX[, pho_en_nzdep_quintiles := ifelse(view_en_nzdep %in% c(1,3,5,7,9), 
                                          (view_en_nzdep+1)/2, 
                                          view_en_nzdep/2)]


# ----- iii. Find DHB Catchment ----
INDEX <- merge(INDEX, ALL_PREDICT[,.(VSIMPLE_INDEX_2NDARY, view_visit_date, pt_submissions_id, pt_dhbcatchment)],
               by = c("VSIMPLE_INDEX_2NDARY", "view_visit_date", "pt_submissions_id"),
               all.x = T)

# There are around 2000 records with invalid DHB codes (i.e. 2 and 5)
# Write them as NA
INDEX[, pt_en_dhbcatchment := replace(pt_dhbcatchment, pt_dhbcatchment %in% c(2,5), NA)]

# Capture DHB code using meshblock
MB_DHB <- data.table::fread("common_lookups/MB NZDep/Annual Areas 2018.txt")

INDEX[ , dhb_code := MB_DHB$DHB2015_code[match(EN_MESHBLOCK, MB_DHB$MB2013_code)]]

INDEX[is.na(dhb_code), dhb_code := MB_DHB$DHB2015_code[match(EN_MESHBLOCK, MB_DHB$MB2006_code)]]
INDEX[is.na(dhb_code), dhb_code := MB_DHB$DHB2015_code[match(EN_MESHBLOCK, MB_DHB$MB2001_code)]]

# Unify coding to match PREDICT / MOH coding system
INDEX[, dhb_code := dplyr::case_when(
  
  dhb_code == 1 ~ 11,
  dhb_code == 2 ~ 21,
  dhb_code == 3 ~ 22,
  dhb_code == 4 ~ 23,
  dhb_code == 5 ~ 31,
  dhb_code == 6 ~ 42,
  dhb_code == 7 ~ 47,
  dhb_code == 8 ~ 51,
  dhb_code == 9 ~ 71,
  dhb_code == 10 ~ 61,
  dhb_code == 11 ~ 82,
  dhb_code == 12 ~ 81,
  dhb_code == 13 ~ 92,
  dhb_code == 14 ~ 91,
  dhb_code == 15 ~ 93,
  dhb_code == 16 ~ 101,
  dhb_code == 17 ~ 111,
  dhb_code == 18 ~ 121,
  dhb_code == 19 ~ 123,
  dhb_code == 22 ~ 160,
  TRUE ~ 999
  
)]

# Improve PREDICT DHB code using Meshblock
INDEX[, view_en_dhbcatchment := ifelse(is.na(pt_en_dhbcatchment),
                                       dhb_code, 
                                       pt_en_dhbcatchment)]


# -- Final Linkage --
# In the end, 90 records are missing NZDEP from any source possible - remove completely
INDEX[, index := max(.N), by = list(VSIMPLE_INDEX_2NDARY, view_visit_date, pt_submissions_id)]

DATA <- merge(ALL_PREDICT, 
              INDEX[,.(VSIMPLE_INDEX_2NDARY, view_visit_date, pt_submissions_id, pho_en_nzdep_quintiles, view_en_dhbcatchment, EN_MESHBLOCK)],
              all.x = T,
              by = c("VSIMPLE_INDEX_2NDARY", "view_visit_date", "pt_submissions_id")) 

# Improve NZDEP using PREDICT
# Fill in missing pt_nzdep using PHO derived pho_en_nzdep
# 72424 filled in using pho_en_nzdep
DATA[, view_en_nzdep := as.numeric(ifelse(pt_nzdep %in% 1:5, 
                                          pt_nzdep, NA))]

DATA[is.na(view_en_nzdep),
     view_en_nzdep := pho_en_nzdep_quintiles]

# 38 will be removed as no possible enrichment was attainable
DATA <- DATA[!is.na(view_en_nzdep)]

ALL_PREDICT <- copy(DATA)

# save / 1626600 rows / 564167 people
fst::write.fst(ALL_PREDICT, "source_data/R/PREDICT/2018/ALL_PREDICT_Part_4.fst", compress = 75)

