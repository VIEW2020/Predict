
# Component Procedure: Charlson CMI 5 year Lookback

library(data.table, quietly = T)

setDT(Baseline)

#   i.   Find Diagnoses
chf.codes           <- c("^I50","^I110$","^I130$","^I132$")

dementia.codes      <- c("^F00","^F01","^F02","^F03","^F051$","^G30")

copd.codes          <- c("^J40","^J41","^J42","^J43","^J44","^J45","^J46","^J47",
                         "^J60","^J61","^J62","^J63","^J64","^J65","^J66","^J67","^J684$",
                         "^J701$","^J703$","^J841$","^J920$","^J961$","^J982$","^J983$")

ctd.codes           <- c("^M05","^M06","^M08","^M09","^M30","^M31","^M32","^M33","^M34","^M35","^M36","^D86","^J990$","^J991$","^N164")

liver.mild.codes    <- c("^B18","^K700$","^K701$","^K702$","^K709$","^K710$","^K713$","^K714$","^K715$","^K73","^K760$")

liver.severe.codes  <- c("^I850$","^I859$","^I982$","^B150$","^B160$","^B162$","^B190$",
                         "^K703$","^K704$","^K72","^K711$","^K717$","^K74","^K766$","^Z944$")

diab.eod.codes      <- c("^E102$","^E103$","^E104$","^E105$","^E106$","^E107$","^E108$",
                         "^E112$","^E113$","^E114$","^E115$","^E116$","^E117$","^E118$",
                         "^E122$","^E123$","^E124$","^E125$","^E126$","^E127$","^E128$",
                         "^E132$","^E133$","^E134$","^E135$","^E136$","^E137$","^E138$",
                         "^E142$","^E143$","^E144$","^E145$","^E146$","^E147$","^E148$") 

hemi.paraplegia.codes <- c("^G81","^G82")

severe.renal.codes    <- c("^I120$","^I131$","^I132$","^N03","^N04","^N05","^N07","^N11","^N14","^N18","^N19","^N250$","^P960$",
                           "^T824$","^T8571$","^T861$","^Y602$","^Y612$","^Y622$","^Y841$","^Z490$","^Z491$","^Z492$","^Z940$","^Z992$")

cancer.non.meta.codes <- c("^C00","^C01","^C02","^C03","^C04","^C05","^C06","^C07","^C08","^C09","^C10","^C11","^C12","^C13","^C14","^C15",
                           "^C16","^C17","^C18","^C19","^C20","^C21","^C22","^C23","^C24","^C25","^C26","^C27","^C28","^C29","^C30","^C31",
                           "^C32","^C33","^C34","^C35","^C36","^C37","^C38","^C39","^C40","^C41","^C42","^C43","^C44","^C45","^C46","^C47",
                           "^C48","^C49","^C50","^C51","^C52","^C53","^C54","^C55","^C56","^C57","^C58","^C59","^C60","^C61","^C62","^C63",
                           "^C64","^C65","^C66","^C67","^C68","^C69","^C70","^C71","^C72","^C73","^C74","^C75","^C76")

cancer.meta.codes   <- c("^C77","^C78","^C79","^C80")

leukaemia.codes     <- c("^C91","^C92","^C93","^C94","^C95")

lymphoma.codes      <- c("^C81","^C82","^C83","^C84","^C85","^C88","^C90","^C96")

aids.codes          <- c("^B21","^B22","^B23","^B24")


#  ii.  Capture Any Comorbidity History
#       NB: Use visit date and filter all NMDS events data by 5 year history

charlson.varnames <- ls(pattern=".codes$")

for(var in charlson.varnames){
  
  codes <- get(var)
  
  HX_EVENTS[, hx_event:= +(grepl(paste(codes, collapse = "|"), CLIN_CD_10))]
  
  EVENTS <- HX_EVENTS[hx_event==1, 
                      unique(.SD),
                      .SDcols = c("VSIMPLE_INDEX_MASTER", "hx_event")]
  
  ALL_CMI_VARS <- if(var==charlson.varnames[1]){
     
     merge(Baseline[,.(VSIMPLE_INDEX_MASTER)], setDF(EVENTS),
                    by="VSIMPLE_INDEX_MASTER",
                    all.x=TRUE)
  } else {
     
     merge(ALL_CMI_VARS, EVENTS,
            by="VSIMPLE_INDEX_MASTER",
            all.x=T)
     
  }
  
  ALL_CMI_VARS[, hx_event := +(!is.na(hx_event))]
  setnames(ALL_CMI_VARS, "hx_event", gsub("\\.", "_", toupper(gsub(".codes", "", var))))
  
  print(paste(var, " completed")); rm(EVENTS)
  
}

# iii.  Caclulate Score
#     NB: Adjust for severity (if both severe and mild are flagged, then count as severe only): Applies to Liver Disease, Cancer
setDT(ALL_CMI_VARS)

ALL_CMI_VARS[LIVER_SEVERE==1 & LIVER_MILD==1, LIVER_MILD:=0]
ALL_CMI_VARS[CANCER_META==1 & CANCER_NON_META==1, CANCER_NON_META:=0]

  weight.1 <- c("COPD","CTD","SEVERE_RENAL","DIAB_EOD")
  weight.2 <- c("CHF","DEMENTIA","LIVER_MILD","HEMI_PARAPLEGIA","CANCER_NON_META","LEUKAEMIA","LYMPHOMA")
  weight.4 <- c("AIDS","LIVER_SEVERE")
  weight.6 <- c("CANCER_META")
  
  ALL_CMI_VARS[, .SDcols=c(weight.1, weight.2, weight.4, weight.6)
               , cmi_charlson_score_5yr := apply(.SD, 1,
                                                  function(x)
                                                    sum(x[weight.1])*1
                                                  + sum(x[weight.2])*2
                                                  + sum(x[weight.4])*4
                                                  + sum(x[weight.6])*6)]

# # Tidy
# # ALL_CMI_VARS <- ALL_CMI_VARS[,.(VIEW_ENHI_MASTER, cmi_charlson_score_5yr)]
# 
# # 1B modification - removes diabetes with end organ damage
#   ALL_CMI_VARS$VAR <- NULL
# 
#   weight.1b <- c("COPD","CTD","SEVERE_RENAL")
#   weight.2 <- c("CHF","DEMENTIA","LIVER_MILD","HEMI_PARAPLEGIA","CANCER_NON_META","LEUKAEMIA","LYMPHOMA")
#   weight.4 <- c("AIDS","LIVER_SEVERE")
#   weight.6 <- c("CANCER_META")
#   
#   ALL_CMI_VARS[, .SDcols=c(weight.1b, weight.2, weight.4, weight.6)
#                , cmi_charlson_score_5yr_nodiab := apply(.SD, 1,
#                                                   function(x)
#                                                     sum(x[weight.1b])*1
#                                                   + sum(x[weight.2])*2
#                                                   + sum(x[weight.4])*4
#                                                   + sum(x[weight.6])*6)]
#   
  # Attach Prefix
  prefix.required <- names(ALL_CMI_VARS)[!startsWith(names(ALL_CMI_VARS), "cmi")][-1]
  
  names(ALL_CMI_VARS)[!startsWith(names(ALL_CMI_VARS), "cmi")][-1] <- paste("cmi", tolower(prefix.required), "5yr", sep = "_")
  