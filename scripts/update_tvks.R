library(devtools)
load_all("W:/PYWELL_SHARED/Pywell Projects/BRC/_BRC_dataflow/BRC_Dataflow_Original_Data/BRCclean")
library(tidyverse)


dir <- list.dirs("rules_as_csv")
files <- c("additional.csv", "id_difficulty.csv", "period.csv", "periodwithinyear.csv", "tenkm.csv"  )

for(i in 1:length(dir)) {
  for(j in 1:length(files)) {
    if(file.exists(paste(dir[i], files[j], sep = "/"))){
      
      file <- read.csv(paste(dir[i], files[j], sep = "/")) %>%
        select(-taxon) %>%
        taxon_tvk_match(tvk = "tvk") %>%
        select(-c(preferred_authority, tvk, family_taxon, organism_key)) %>%
        rename(tvk = rtvk,
               taxon = preferred_taxon) %>%
        distinct(.keep_all = TRUE)
      
      write.csv(file, paste(dir[i], files[j], sep = "/"), na = "", row.names = FALSE)
      
    }
  }
}

