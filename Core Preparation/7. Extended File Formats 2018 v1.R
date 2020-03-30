
# -- File Format Conversion  --
# --  for PREDICT 2018 v1    --

# Last updated October 2019

# Converts all PREDICT and PREDICT-subset source files to text file
# Converts all PREDICT and PREDICT-subset source files to feather file

library(data.table, quietly = T)
library(fst, quietly = T)
library(feather, quietly = T)
library(haven, quietly = T)


# 1.  PREDICT
files.path <- "V:/source_data/R/PREDICT/2018/"
files.list <- list.files(files.path, pattern = "Cleaned")

for(file in files.list){
  
  DATA <- read.fst(paste0(files.path, file))
  
  save.path <- "D:/temp/"
  save.name <- gsub(".fst", "", file)

  write_feather(DATA, paste0(save.path, save.name, ".feather"))
  write_sav(DATA, paste0(save.path, save.name, ".sav"))
  
  print(paste0(file, " completed")); rm(DATA)
  
}


# 2.  National Health Collection
#     nb. everything in the 'National Collection' sub-directory. 
#     inc. nmds, private, mortality, & pharms
files.path <- "V:/source_data/R/PREDICT/2018/National Collection/"
files.list <- list.files(files.path, pattern = "VSIMPLE")

for(file in files.list){
  
  DATA <- read.fst(paste0(files.path, file))
  
  save.path <- "D:/temp/"
  save.name <- gsub(".fst", "", file)
  
  write_feather(DATA, paste0(save.path, save.name, ".feather"))
  write_sav(DATA, paste0(save.path, save.name, ".sav"))
  
  print(paste0(file, " completed")); rm(DATA)
  
}

# 3.  TestSafe
#     nb. everything in the 'TestSafe' sub-directory. 
#     inc. all test types
files.path <- "V:/source_data/R/PREDICT/2018/TestSafe/Lipids/"
files.list <- list.files(files.path, pattern = "VSIMPLE")

for(file in files.list){
  
  DATA <- read.fst(paste0(files.path, file))
  
  save.path <- "D:/temp/"
  save.name <- gsub(".fst", "", file)
  
  write_feather(DATA, paste0(save.path, save.name, ".feather"))
  write_sav(DATA, paste0(save.path, save.name, ".sav"))
  
  print(paste0(file, " completed")); rm(DATA)
  
}

