
# Componental Procedure Set: Mortality 

library(data.table, quietly = T)

setDT(Baseline)

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
     
   CVD_DEATHS <- merge(Baseline[,.(VSIMPLE_INDEX_MASTER)], DEATHS[,.(VSIMPLE_INDEX_MASTER, outcome)],
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




