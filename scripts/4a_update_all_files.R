library(tidyverse)
library(stringr)
print(getwd())
source("scripts/3_file_test.R")

write.csv(error_df, "errors_preventing_latest_commit.csv", na  ="", row.names = FALSE)
  
if(ncol(error_df) != 3) { 

for(i in 1:length(schemes)) {
  
  print(names(schemes[i]))
  species_checks(group = names(schemes[i]))
  
  if(file.exists(paste0("rules_as_csv/", names(schemes[i]), "/periodwithinyear.csv"))) {
    
    print("Exporting periodwithinyear.csv")
    periodwithinyear_rules(group = names(schemes[i]))
    
  }

  if(file.exists(paste0("rules_as_csv/", names(schemes[i]), "/id_difficulty.csv"))) {
    
    
    print("Exporting id_difficulty.csv")
    idifficulty_rules(group = names(schemes[i]))
    
  }
  
  if(file.exists(paste0("rules_as_csv/", names(schemes[i]), "/tenkm.csv"))) {
    
    print("Exporting tenkm.csv")
  tenkm_rules(group = names(schemes[i]))
    
  }
  
  if(file.exists(paste0("rules_as_csv/", names(schemes[i]), "/period.csv"))) {
    
    print("Exporting period.csv")
  period_rules(group = names(schemes[i]))
    
  }

  file_path <-  paste("rules/", names(schemes[i]), sep = "")
  file_list<-list.files(path = file_path, recursive = TRUE, include.dirs = FALSE)

  mainwd <- getwd()
  setwd(file_path)
  
  zip(paste(mainwd, "/", "rules_as_txt/", gsub("/", " - ", names(schemes[i])), ".zip", sep = ""), files = file_list)

  setwd(mainwd)
}
}
