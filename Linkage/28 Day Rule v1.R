
# Componental Procedure Set: 28 rule and CVD Event type
library(data.table, quietly = T)

setDT(Baseline)


# i.  CVD in 28 days of death

# Find earliest CVD & record type
for(var in out.28d.vars){
  
  codes <- get(var)
  
  EVENTS <- OUT_EVENTS[, out_event:= +(CLIN_CD_10 %in% codes)]
  
  EVENTS <- OUT_EVENTS[out_event==1][order(EVSTDATE)
                                     , index:=seq_len(.N)
                                     , by=VSIMPLE_INDEX_MASTER][index==1, .(VSIMPLE_INDEX_MASTER, out_event, EVSTDATE)]
  
  ALL_28D_OUT_VARS <-  if(var == out.28d.vars[1]){
    
    merge(Baseline[,.(VSIMPLE_INDEX_MASTER, view_ag_dod)], EVENTS,
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


# Capture CVD type (28 day NMDS)
#     Reshape data to categorical variable + date 
#     Priority is in following order: 
#       i) Time priority (nearest to Baseline first). If there is a tie, 
#       ii) CVD type priority 
#     Apply ties method (takes first cvd type using [1] nb: this is why the vector ordering is very important!)
#     Take the date of the chosen CVD type
out.cvd.vars <- gsub("\\.", "_", out.28d.vars)
out.vardates <- paste0(out.cvd.vars, "_adm_date")

out.f28d.vars <- c("out_fatal_28d_cvd_adm", "out_fatal_28d_cvd_adm_typedate", "out_fatal_28d_cvd_adm_type")

setDT(ALL_28D_OUT_VARS)

ALL_28D_OUT_VARS[apply(ALL_28D_OUT_VARS[, out.vardates, with=F], 1, 
                       function(x) 
                         !all(is.na(x))) # condition: Must have atleast 1 CVD type
                 
                 , .SDcols=out.vardates
                 , (out.f28d.vars) := .(1L,
                                        as.Date(apply(.SD, 1, 
                                                      function(x)
                                                        min(x, na.rm = T)), origin="1970-01-01"),
                                        
                                        as.character(apply(.SD, 1,
                                                           function(x){
                                                             type <- names(x)[which(x==min(x, na.rm = T))][1] # priority method applied using [1]
                                                             gsub("out_|_adm_date", "", type)
                                                           })))]

ALL_28D_OUT_VARS[, out_fatal_28d_cvd_adm := +(!is.na(out_fatal_28d_cvd_adm))]
