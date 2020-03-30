
# -- 1.  Base Preparation 2019 v1 --

# Last Updated August 2019

# Uses VSIMPLE ENHI as ID

# Library
library(data.table)
library(fst)
library(RODBC)

setwd("V:/")

# ----- Getting Started -----

# The PREDICT dataset, in its source state, comes to you from ENGIMA in the form of a .bak file (MS SQL Server backup file)
# To import a .bak file and gain access to the database, you must install MSSQLServer onto your local machine (if not already done so). 
# The express version of MSSQLServer is free and will be suffice for PREDICT data.
# After you have installed MSSQLServer, you will need to create an ODBC data source in order to connect to the MSSQLServer using R (or Python or SAS). 
# Adding a new data source can be done using the Windows application called "Microsoft ODBC Data Source Administrator".
# Note: Make sure the 'Trust Server Certificate' option is checked!
# After you have added an ODBC data source, the R package "RODBC" is used to create a connection from R to MSSQLSERVER or any other database system (eg. Oracle)

# **  Establish Credentials
#     NB: I haved called the data source name "VIEW2020_PREDICT" and used the computer's D: drive as a local DB instance. There is no login/password. 
odbccon <- RODBC::odbcConnect(dsn="VIEW2020_PREDICT")

# **  Run Query
#     Nb: SQL syntax is used to run the query, which is wrapped in R syntax to assign the query result to an R object.
#     Observations = 2033282 / ETA = 12 mins
#     Patient ID   = 576,665 people (which includes invalid, unmatchable ENHIs, duplicates, secondaries and non-NHI matches).
Raw_PREDICT <- RODBC::sqlQuery(odbccon, paste0("SELECT * FROM UoA_Extract.dbo.allCombined"))
Raw_PREDICT <- setDT(Raw_PREDICT)

write.fst(Raw_PREDICT, "source_data/R/PREDICT/2019/Raw_PREDICT.fst", compress = 75)


# ----- Basic Field Converstion -----

library(fst)
library(data.table)

ALL_PREDICT <- read.fst("source_data/R/PREDICT/2018/Raw_PREDICT.fst", as.data.table = T)

# 1.  Factors o characters
factor.vars <- names(ALL_PREDICT)[which(sapply(ALL_PREDICT, class)=="factor")]

if(length(factor.vars)!=0){
  
   ALL_PREDICT[, (factor.vars) := lapply(.SD,
                                         function(x)
                                            as.character(x)), .SDcols = factor.vars]
  }

# 2.  POSIXct date/times to R Dates
#     NB: - Create a visit date and keep original submissions timestamp as actual timestamp
#         - Range of current dataset 2004-10-20 to 2019-07-10
posixct.vars <- names(ALL_PREDICT)[which(sapply(ALL_PREDICT, 
                                                    function(x) 
                                                      inherits(x, "POSIXct")))]

posixct.vars <- setdiff(posixct.vars, "submissions_timestamp")

if(length(posixct.vars)!=0){
  
   ALL_PREDICT[, (posixct.vars) := lapply(.SD,
                                          function(x)
                                             as.Date(as.character(x), 
                                                     format="%Y-%m-%d")), .SDcols = posixct.vars]
  
}

ALL_PREDICT[, view_visit_date := as.Date(as.character(submissions_timestamp),
                                         format="%Y-%m-%d")]


# 3.  Encypted NHI Matching
#     Swap VIEW_SECONDARY over to VSIMPLE_SECONDARY. 
#     NOMATCH=0 means that if a PREDICT records is not mergable with the ENHI Lookup list, then the record is dropped
#     NB: Removing non-NHI matches removes 6011 observations, leaving 2027272 Observations
#         After merge, there are 572294 unique people. Note: duplicate records still remain at this stage.
ENHI_Lookup <- read.fst("source_data/R/ENHI BOTH TABLES/VIEW_VSIMP_MOH__PS_AUG2019.fst", as.data.table = T)
ENHI_Lookup <- ENHI_Lookup[,.(VSIMPLE_INDEX_MASTER, VSIMPLE_INDEX_2NDARY, VIEW_ENHI_2NDARY)]

setkey(ENHI_Lookup, VIEW_ENHI_2NDARY)
setkey(ALL_PREDICT, patient_id)

ALL_PREDICT <- ENHI_Lookup[ALL_PREDICT, nomatch = 0]

ALL_PREDICT[, c("VIEW_ENHI_2NDARY","NHI") := NULL]


# 4.   Establish naming convention
var.to.rename <- names(ALL_PREDICT)[!grepl("VSIMPLE|view", 
                                           names(ALL_PREDICT), 
                                           ignore.case = T)]
renamed.vars  <- paste0("pt_", 
                        tolower(var.to.rename))

names(ALL_PREDICT)[which(names(ALL_PREDICT) %in% var.to.rename)] <- renamed.vars


# ---- DOB Syncroynisation ----

# Remove records that do not have matching DOB in NHI
# NB: Maintain the use of secondary ENHI for demographic merge with NHI --

NHI <- read.fst("source_data/R/NHI/VSIMPLE_NHI_LOOKUP_AUG2019.fts", as.data.table = T)

#       For some ENHIs, the repeated measures may in fact be a totally different people (who share the same ENHI).
#       NB: The DOB procedure is done here so that maximum DOB information can be used before any removal of observations in later steps,
ALL_PREDICT[, nhi_en_dob := NHI$EN_DOB[match(VSIMPLE_INDEX_2NDARY, NHI$VSIMPLE_INDEX_2NDARY)]]


# Identify records who's DOB is considered belong to another person in the NHI
# For most records, the mismatch in DOB is due to obvious input error (eg. month-day flipped, year or month or day slightly off)
# If the non-matching pt_dob contains 2 of 3 elements (ie. YYYY or mm or dd), then consider it same person as NHI albeit input error.

# NB:  You can't just overwrite PREDICT DOB using the NHI DOB because they might infact be totally different people.
# Eg. TEST <- copy(ALL_PREDICT[nhi_en_dob!=pt_dob])[,.(VSIMPLE_INDEX_MASTER, nhi_gender, pt_gender, nhi_en_dob, pt_dob)][1:100]

DateIdentityCheck <- function(x, y){
 
   library(lubridate, quietly = T)
   
   mapply(function(x, y){
      
      date.1 <- c(year(x), month(x), day(x))
      date.2 <- c(year(y), month(y), day(y))
      
      +(sum(date.1 %in% date.2)>=2)
      
   }, x=x, y=y)
}

ALL_PREDICT[, passed := DateIdentityCheck(x=nhi_en_dob, y=pt_dob)]

# 20620 records do not have an exact DOB match / 4635 records did not pass the DOB identity check 
# The failed records should be removed completely!
# 708 unique individuals removed completely
ALL_PREDICT <- ALL_PREDICT[passed==1][, passed := NULL]

# 2022637 records / 571586 unique individuals remaining 
# write.fst(ALL_PREDICT, "source_data/R/PREDICT/2019/ALL_PREDICT_Part_1.fst", compress = 75)
