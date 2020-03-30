
# M3 Multimorbidity Index - 5 year lookback v2

library(dplyr)

# Requires input using NMDS as:
# M3_DATA <- copy(ALL_EVENTS)[EVSTDATE < index_evstdate & EVSTDATE - index_evstdate >= -1825][,.(VSIMPLE_INDEX_MASTER, CLIN_CD_10)]
# setnames(M3_DATA, "CLIN_CD_10", "CLIN_CD")

source("V:/bwu009/My Projects/Generic Functions/M3index 00 M3 ICD codes v010.R")     # Definitions
source("V:/bwu009/My Projects/Generic Functions/M3index 10 Code as Text Vector v010.R") # Run main engine

final_M3_scored <- final_M3_scored %>% 
  select(VSIMPLE_INDEX_MASTER, M3Score) %>% 
  mutate(M3Score = ifelse(is.na(M3Score), 0, M3Score)) %>% 
  rename(cmi_m3_score_5yr_prior = M3Score) %>% 
  as.data.table()