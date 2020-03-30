
# Componental Procedure Set: Any History - for PREDICT

# Nb: Uses a merged EVENTS / DIAGS table

setDT(Baseline)

# Any History
for(var in history.vars){
  
  codes <- get(var)
  
  if(var %in% c("hx.af", "hx.atrial.fibrillation")){
    EVENTS <- HX_EVENTS[, hx_event:= +(grepl("^I48", CLIN_CD_10))]
  } else {
    EVENTS <- HX_EVENTS[, hx_event:= +(CLIN_CD_10 %in% codes)]
  }
  
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

