library(tidyverse)
library(stringr)

source("scripts/2_load_functions.R")

error_df <- data.frame()
for(i in 1:length(schemes)) {
  
  print(names(schemes[i]))
  species_file = paste0("rules_as_csv/", schemes[[i]]$scheme, "/",schemes[[i]]$rule_group, "/id_difficulty.csv")

  if(file.exists(species_file)){
	  
	  species = read.csv(species_file)
	  
	  # Check file

	  flush.console()
	  
	  
	  # Check for duplicated rows
	  if(nrow(species) != length(unique(species$tvk))){
	    
	    dups <- species %>%
	      count(tvk) %>%
	      filter(n > 1)
      
      new_error <- data.frame(file = species_file,
                              error_msg = "TVKs are duplicated",
                              column_identifier = paste(dups$tvk, collapse="\n\t"))
	    error_df <- bind_rows(error_df, new_error)
	    species = unique(species)
	  }
	  
	  # Look for TVKs that are matched to more than one name
	  chk_inds = which(
	    tapply(as.character(species$taxon), species$tvk, length, simplify = FALSE) > 1
	  )
	  
	  if(length(chk_inds) > 0){
	    
	    new_error <- data.frame(file = species_file,
                              error_msg = "TVKs are linked to more than 1 name",
                              column_identifier = paste(names(chk_inds), collapse="\n\t"))
	    error_df <- bind_rows(error_df, new_error)
	  }
	  
	  # Look for names that are matched to more than one TVK
	  chk_inds = which(
	    tapply(as.character(species$tvk), species$taxon, length, simplify = FALSE) > 1)
	  
	  if(length(chk_inds) > 0){
	    
      new_error <- data.frame(file = species_file,
                              error_msg = "Names are linked to more than 1 TVK",
                              column_identifier = paste(names(chk_inds), collapse="\n\t"))
	    error_df <- bind_rows(error_df, new_error)
	  }
	  
	  # Print progress

	  flush.console()
	  
	  
	} else {
	  
	  new_error <- data.frame(file = species_file,
                            error_msg = "Species file does not exist")
	    
    error_df <- bind_rows(error_df, new_error)
	  flush.console()
	  
	}	
	
}

  if(file.exists(paste0("rules_as_csv/", names(schemes[i]), "/periodwithinyear.csv"))) {
    
    species_file = paste0("rules_as_csv/", schemes[[group]]$scheme, "/",schemes[[group]]$rule_group, "/id_difficulty.csv")
    species = read.csv(species_file)
	  period_file = paste0("rules_as_csv/", schemes[[group]]$scheme, "/",schemes[[group]]$rule_group, "/periodwithinyear.csv")
	  periods = read.csv(period_file)
	
	
	# Cross-check files

	flush.console()

    period_dups <- periods %>%
        select(taxon, tvk, stage)

	# Check for duplicated rows
	if(nrow(period_dups) != nrow(unique(period_dups))){
		
    dups <- period_dups %>%
        group_by(stage) %>%
	      count(tvk) %>%
	      filter(n > 1)
    
    new_error <- data.frame(file = period_file,
                              error_msg = "Contains duplicated tvk and stage combination",
                              column_identifier = paste(dups$tvk, collapse="\n\t"))
	  
    error_df <- bind_rows(error_df, new_error)

	}
			
	# Check all tvk referenced in periods are in species
	period_tvk = unique(periods$tvk)
	chk_inds = which(!period_tvk %in% species$tvk)
    
	if(length(chk_inds) > 0){
		
    new_error <- data.frame(file = period_file,
                              error_msg = "TVKs are present in periodwithinyear.csv but are not present in id_difficulty.csv",
                              column_identifier = paste(period_tvk[chk_inds], collapse="\n\t"))
	  
    error_df <- bind_rows(error_df, new_error)
    
	}
    
  }

  if(file.exists(paste0("rules_as_csv/", names(schemes[i]), "/id_difficulty.csv"))) {
    
    species_file = paste0("rules_as_csv/", schemes[[group]]$scheme, "/",schemes[[group]]$rule_group, "/id_difficulty.csv")
    species = read.csv(species_file)
    difficulty_file = paste0("rules_as_csv/", schemes[[group]]$scheme, "/",schemes[[group]]$rule_group, "/difficulty_codes.csv")
    difficulties = read.csv(difficulty_file)
    species = species[!is.na(species$code),]
	
    species_idiff = unique(species$code)
    chk_inds = which(!species_idiff %in% difficulties$code)

  if(length(chk_inds) > 0){
    
    new_error <- data.frame(file = species_file,
                              error_msg = "TCodes given below are present in species.csv but are not present in difficulty_codes.csv",
                              column_identifier = paste(species_idiff[chk_inds], collapse="\n\t"))
	  
    error_df <- bind_rows(error_df, new_error)
    
	}
    
  }
  
  if(file.exists(paste0("rules_as_csv/", names(schemes[i]), "/tenkm.csv"))) {
    
    species_file = paste0("rules_as_csv/", schemes[[group]]$scheme, "/",schemes[[group]]$rule_group, "/id_difficulty.csv")
  species = read.csv(species_file)
	tenkm_file = paste0("rules_as_csv/", schemes[[group]]$scheme, "/",schemes[[group]]$rule_group, "/tenkm.csv")
	tenkm = read.csv(tenkm_file)

	# Check all tvk referenced in periods are in species
	tenkm_tvk = unique(tenkm$tvk)
	chk_inds = which(!tenkm_tvk %in% species$tvk)
    
	if(length(chk_inds) > 0){
		
    new_error <- data.frame(file = tenkm_file,
                              error_msg = "TVKs are present in tenkm.csv but are not present in id_difficulty.csv",
                              column_identifier = paste(tenkm_tvk[chk_inds], collapse="\n\t"))
	  
    error_df <- bind_rows(error_df, new_error)
    
	}
    
  }
  
  if(file.exists(paste0("rules_as_csv/", names(schemes[i]), "/period.csv"))) {
    
    species_file = paste0("rules_as_csv/", schemes[[group]]$scheme, "/",schemes[[group]]$rule_group, "/id_difficulty.csv")
    species = read.csv(species_file)
    period_file = paste0("rules_as_csv/", schemes[[group]]$scheme, "/",schemes[[group]]$rule_group, "/period.csv")
    periods = read.csv(period_file)

    period_tvk = unique(periods$tvk)

    chk_inds = which(!period_tvk %in% species$tvk)
    
	if(length(chk_inds) > 0){
		
    new_error <- data.frame(file = period_file,
                              error_msg = "TVKs are present in period.csv but are not present in id_difficulty.csv",
                              column_identifier = paste(period_tvk[chk_inds], collapse="\n\t"))
	  
    error_df <- bind_rows(error_df, new_error)
    
	}
    
  }
