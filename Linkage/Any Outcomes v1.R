
# Componental Procedure Set: Any Outcome to end of study

setDT(Baseline)

# Any Outcome
for(var in outcome.vars){
  
  codes <- get(var)
  
  EVENTS <- OUT_EVENTS[, out_event:= +(CLIN_CD_10 %in% codes)]

  EVENTS <- OUT_EVENTS[out_event==1][order(EVSTDATE)
                                    , index:=seq_len(.N)
                                    , by=VSIMPLE_INDEX_MASTER][index==1, .(VSIMPLE_INDEX_MASTER, out_event, EVSTDATE)]
    
  ALL_OUT_VARS <- if(var == outcome.vars[1]){
      
      merge(Baseline[,.(VSIMPLE_INDEX_MASTER)], EVENTS,
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

# # First CVD Outcome by earliest date & CVD type
# #  - Ensure there is only 1 CVD type per person
# #  - Prioritise by time nearest to index
# #  - If tied, prioritise by CVD type
# 
# # Specify outcome variables (do not include higher level composite groups!)
# # All event types will combine to form outcome broad CVD
# # CHD = MI + Unstable Angina + Other CHD
# # CeVD = Ischaemic Stroke + TIA + Other CeVD
# 
# #  Priority =  MI, Unstable Angina, Ischaemic Stroke, Haemorrhagic Stroke, Heart Failure, Other CHD, TIA, Other CeVD, PVD 
# #              signed off Rod, Sue, Katrina (Email March 28th 2018)
# 
# # NB: THIS ORDER IS VERY IMPORTANT as it prioritises the CVD event type when there is a tie date.
# #     eg 68e195febc7db3ece8855f9ecaff28e43e17e4409ee17ba - should be assigned ischaemic stroke
# outcome.vars <- c("out_mi", "out_unst_angina", "out_ischaemic_stroke", "out_haemorrhagic_stroke", "out_heart_failure",
#                   "out_other_chd", "out_tia", "out_other_cevd", "out_pvd_diags")    
# 
# out.vardates <- paste0(outcome.vars, "_adm_date");
# 
# #     Prioritise by min-date (ie. nearest event date to index)
# #     Select CVD type - apply ties method (takes first cvd type using [1] nb: this is why the above vector ordering is very important!)
# adm.vars <- c("out_1st_cvd_adm_typedate", "out_1st_cvd_adm_type")
# 
# setDT(ALL_OUT_VARS)
# 
# ALL_OUT_VARS[apply(ALL_OUT_VARS[, out.vardates, with=F], 1,
#                   function(x)
#                     !all(is.na(x)))
#    
#             , .SDcols=out.vardates
#             , (adm.vars) := .(as.Date(apply(.SD, 1,
#                                              function(x)
#                                                min(x, na.rm = T)), origin="1970-01-01"),
#    
#                                as.character(apply(.SD, 1,
#                                                   function(x){
#                                                     type <- names(x)[which(x==min(x, na.rm = T))][1] # priority method applied using [1]
#                                                              gsub("out_|_adm_date", "", type)
#                                                        })))]
