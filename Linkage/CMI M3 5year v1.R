


# M3 Multimorbidity Index - 5 year lookback

M3_EVENTS <- copy(ALL_EVENTS)[,.(VIEW_ENHI_MASTER, EVENT_ID, EVSTDATE)]

M3_EVENTS <- M3_EVENTS[, view_visit_date := Baseline$view_visit_date[match(VIEW_ENHI_MASTER, Baseline$VIEW_ENHI_MASTER)]] %>% 
  filter((EVSTDATE <= view_visit_date) & (EVSTDATE - view_visit_date >= -1825))

M3_DIAGS  <- ALL_DIAGS[EVENT_ID %in% M3_EVENTS$EVENT_ID] %>% 
  as.data.frame()

M3_DATA <- merge(M3_EVENTS[,c("VIEW_ENHI_MASTER", "EVENT_ID")], M3_DIAGS,
                 by="EVENT_ID",
                 all.y=T) %>% 
  select(VIEW_ENHI_MASTER, CLIN_CD) %>% 
  as.data.frame()

source("bwu009/Scripts/Functions/M3/M3index 00 M3 ICD codes v010.R")     # Definitions
source("bwu009/Scripts/Functions/M3/M3index 10 Code as Text Vector v010.R") # Run main engine

# Merge output back to COHORT
Baseline <- merge(Baseline, final_M3_scored[, c("VIEW_ENHI_MASTER", "M3Score")],
                  by="VIEW_ENHI_MASTER",
                  all.x=T) %>% 
  mutate(M3Score = ifelse(is.na(M3Score), 0, M3Score)) %>% 
  rename(cmi_m3_score_5yr_prior = M3Score) %>% 
  as.data.table()