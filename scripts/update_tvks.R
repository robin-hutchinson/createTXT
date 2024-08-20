library(devtools)
load_all("W:/PYWELL_SHARED/Pywell Projects/BRC/_BRC_dataflow/BRC_Dataflow_Original_Data/BRCclean")
library(tidyverse)


dir <- list.dirs("rules_as_csv")
files <- c("additional.csv", "id_difficulty.csv", "period.csv", "periodwithinyear.csv", "tenkm.csv"  )

for(i in 1:length(dir)) {
  all_tvks <- data.frame()
  if(file.exists(paste(dir[i],"additional.csv" , sep = "/"))) {
    
    a_tvk <- read.csv(paste(dir[i],"additional.csv" , sep = "/")) %>%
      distinct(tvk)
    all_tvks <- bind_rows(all_tvks, a_tvk)%>%
      distinct(tvk)
  }
  
  if(file.exists(paste(dir[i],"period.csv" , sep = "/"))) {
    
    p_tvk <- read.csv(paste(dir[i],"period.csv" , sep = "/")) %>%
      distinct(tvk)
    all_tvks <- bind_rows(all_tvks, p_tvk)%>%
      distinct(tvk)
    
  }
  
  if(file.exists(paste(dir[i],"periodwithinyear.csv" , sep = "/"))) {
    
    py_tvk <- read.csv(paste(dir[i],"periodwithinyear.csv" , sep = "/")) %>%
      distinct(tvk)
    all_tvks <- bind_rows(all_tvks, py_tvk)%>%
      distinct(tvk)
    
  }
  
  if(file.exists(paste(dir[i],"tenkm.csv" , sep = "/"))) {
    
    t_tvk <- read.csv(paste(dir[i],"tenkm.csv" , sep = "/")) %>%
      distinct(tvk)
    
    all_tvks <- bind_rows(all_tvks, t_tvk)%>%
      distinct(tvk)
  }
  
  
  for(j in 1:length(files)) {
    
    if(file.exists(paste(dir[i], files[j], sep = "/"))){
      
      file <- read.csv(paste(dir[i], files[j], sep = "/")) %>%
        select(-taxon)
      
      if(files[j] == "id_difficulty.csv" & nrow(all_tvks) != 0){
        
        file <- full_join(file, all_tvks, by = "tvk") %>%
          mutate(code = case_when(is.na(code) ~ "6",
                                  TRUE ~ as.character(code)))
        
      }
      
      file <- file %>%
        taxon_tvk_match(tvk = "tvk") %>%
        select(-c(preferred_authority, tvk, family_taxon, organism_key)) %>%
        rename(tvk = rtvk,
               taxon = preferred_taxon) %>%
        distinct(.keep_all = TRUE)
      
      
      write.csv(file, paste(dir[i], files[j], sep = "/"), na = "", row.names = FALSE, quote = FALSE)
      
    }
  }
}

dir[26]
