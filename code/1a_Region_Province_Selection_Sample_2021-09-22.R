#########################################################################   
#               INFO                                                    #   
#########################################################################  

# PROJECT: PhilSys process evaluation survey
# PURPOSE: Select provinces  
# INPUTS:  List of Provinces for Step 2 Registration Sampling.xlsx 
# OUTPUTS: selected_provinces_<date>.xlsx, province_selection_probs_<date>.xlsx

# Update from 2021-09-08 file
    # used new data sent by Fritzie (PSA) which includes additional provinces 
    # randomly select 2 
#########################################################################   
#               SETUP                                                   #   
######################################################################### 

  ##### CLEAR ENVIRONMENT
  
  rm(list = ls()) 
  
  ##### SET WORKING DIRECTORY - CHANGE TO YOUR WD 
  
  # setwd("C:/Users/wb436341/WBG/ID4D Initiative - 1-Team Files/1. Analytics and Good Practices/IEs and Research/Philippines/Process Evaluation Survey/Analysis")
 setwd("C:/Users/krishlim/OneDrive - UBC/Desktop/Cesi/philsys/sampling/")
  # setwd("/Users/krishalim/Desktop/Sampling")
  ##### LOAD PACKAGES
  
  need <- c("readxl", "writexl",  "tidyr", "dplyr") # packages required
  have <- need %in% rownames(installed.packages()) # check which already installed
  if(any(!have)) install.packages(need[!have]) # install any new packages
  invisible(lapply(need, library, character.only=T)) # load packages

#########################################################################   
# Step 1: Set Seed 
######################################################################### 

  set.seed(20210608)

#########################################################################   
# Step 2: Import Step 2 Reg Province-level data and clean  
######################################################################### 

  # Load list of provinces from PSA
  #psadata <- read_excel("Step 2 Registrations as of March 17th with NCR.xlsx")
  psadata <- read_excel("data_raw/List of Provinces for Step 2 Registration Sampling.xlsx", 
                        range = "Sheet1!D5:E46")
  
  # Fill down the regions since it's a merged cell in Excel, then rename vars
  psadata <- psadata %>% 
    fill(Region) %>%
    rename(region = Region, province = Province)
  
  # Export cleaner excel file
  # write.csv(psadata, "data_clean/sampling/Region-Province Level for Randomization with NCR.csv")

#########################################################################   
# Step 3: Select one province per region
######################################################################### 
  
  # Generate random number for each province
  psadata$rand1 <- runif(nrow(psadata))
  
  # Sort by region
  psadata <- psadata %>% arrange(region, rand1)

  # Keep first province within region
  # selectedprov <- psadata %>% 
  #   group_by(region) %>%
  #   filter(rand1 == max(rand1)) %>%
  #   mutate(selected = ifelse(rank(rand1) <= 5, 1, 0)) %>%
  #   select(-rand1) %>%
  #   ungroup()
  # selectedprov
  
  # Update: Keep first two provinces within the region to get 20 prov
  selectedprov <- psadata %>% 
    group_by(region) %>%
    mutate(selected = ifelse(rank(-rand1) <= 2, 1, 0)) %>%
    arrange( -selected,region, -rand1) %>% 
    select(-rand1)
  selectedprov 
  
#########################################################################   
# Step 4: Calculate selection probabilities for full sampling frame 
#########################################################################   
  
  # Calculate provinces per region
  count <- psadata %>%
    group_by(region) %>%
    count()
  
  # Merge in with data (BARMM excluded)
  frame <- full_join(psadata, count, by = "region") 
  
  # Calcultate pr(selection) for each province within region
  frame <- frame %>%
    mutate(prSel = 1/n) %>% 
    select(-rand1)
  sum(frame$prSel) # sums to 10 because 10 strata
  
  # Merge in selection status
  full <- full_join(frame, selectedprov, by = c("region", "province")) %>%
    mutate(selected = ifelse(is.na(selected), 0, selected)) %>%
    arrange(-selected, region) 
  
#########################################################################   
# SAVE FILES 
#########################################################################   

  # Today's date
  date <- Sys.Date()

  # Append to file path/name
  name_sample <- paste("data_clean/selected_provinces_",date,".xlsx", sep="")
  name_full <- paste("data_clean/province_selection_probs_",date,".xlsx", sep="")
  #name <- paste("selected_provinces_",date,".xlsx", sep="")
  
  # Save files
  write_xlsx(selectedprov, path = name_sample)
  write_xlsx(full, path = name_full)
  #write_xlsx(full, path = "province_selection_probs.xlsx")
  
  