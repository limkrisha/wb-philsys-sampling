#########################################################################   
#               INFO                                                    #   
#########################################################################  

  # PROJECT: PhilSys Step 1 Registration Survey
  # PURPOSE: Select PSUs (Barangays)  
  # INPUTS:  Randomly_Selected_Provinces.xlsx, Step 1 Targets Barangay.xlsx,
  #          master_barangay.xlsx, bgy_step1_7Nov2020.csv,
  #          Step 1 Registrations from 12-31 Oct in Sample Provinces.xlsx
  # OUTPUTS: barangays_in_selected_provinces.xlsx, psu_sample_selection.xlsx

#########################################################################   
#               SETUP                                                   #   
######################################################################### 
  
  ##### CLEAR ENVIRONMENT
  
  rm(list = ls()) 
  
  ##### SET WORKING DIRECTORY
  
  setwd("C:/Users/wb436341/WBG/ID4D Initiative - 1-Team Files/1. Analytics and Good Practices/Impact Evaluations (IEs)/IE Philippines/Analysis/")
  
  ##### LOAD PACKAGES
  
  need <- c("readxl", "writexl",  "tidyr", "dplyr", "survey", "table1",
            "stringr", "ggplot2", "viridis") # packages required
  have <- need %in% rownames(installed.packages()) # check which already installed
  if(any(!have)) install.packages(need[!have]) # install any new packages
  invisible(lapply(need, library, character.only=T)) # load packages

  ##### SET ALL TABLES TO SHOW "NA" BY DEFAULT
  
  table = function (..., useNA = 'ifany') base::table(..., useNA = useNA)  
  
  
#########################################################################   
#               LOAD DATA                                               #   
######################################################################### 

  # Selected provinces
  prov <- read_excel("data_clean/sampling/Randomly_Selected_Provinces.xlsx") 
  
  # Barangays with Step 1 registration levels
  psa <- read_excel("data_raw/sampling/Step 1 Registrations from 12-31 Oct in Sample Provinces.xlsx")
  psa_new <- read.csv("data_raw/sampling/bgy_step1_7Nov2020.csv", stringsAsFactors = F, encoding = "UTF-8")
  
  # Barangays with target level
  target <- read_excel("data_raw/sampling/Step 1 Targets Barangay.xlsx")
  
  # Master list of barangays with urban/rural status
  bgy <- read_excel("data_clean/admin/master_barangay.xlsx")

  
#########################################################################   
#               CLEAN DATA                                              #   
######################################################################### 
  
  nrow(prov) # 32
  nrow(psa)  # 3337
  nrow(psa_new)  # 13451
  nrow(bgy) # 42046
  nrow(target) # 17698
  
  ##### STEP 1 REGISTRATION DATA - OCTOBER 31
  
  # Rename variables
  names(psa) <- c("num_31oct", "pname", "mname", "bname", "registered_31oct")

  # Drop rows without barangays (a small handful) and total row
  psa <- filter(psa, !is.na(bname) & !is.na(num_31oct))
  nrow(psa) # 3160

  # Replace strings to match master data
  psa <- psa %>% 
    mutate(mname = str_replace(mname,  "Ò", "Ñ"),
           bname = str_replace(bname,  "Ò", "Ñ")) %>%
    mutate(bname = ifelse(bname == "Jose C. Payumo Jr.", "Jose C. Payumo, Jr.", bname),
           bname = ifelse(bname == "Juan Climaco Sr. (Magdugo)", "Juan Climaco, Sr. (Magdugo)", bname))
  
  # Change strings to uppercase
  psa[[2]] <- toupper(psa[[2]])
  psa[[3]] <- toupper(psa[[3]])
  psa[[4]] <- toupper(psa[[4]])
  
  ##### STEP 1 REGISTRATION DATA - NOVEMBER 7
  
  # Rename variables
  names(psa_new) <- c("num_7nov", "pname", "mname", "bname", "registered_7nov")
  
  # Drop empty rows and small number registered without barangay
  psa_new <- filter(psa_new, bname != "" & !is.na(num_7nov))
  nrow(psa_new) # 3617
  
  # Replace strings to match master data
  psa_new <- psa_new %>%
    mutate(bname = ifelse(bname == "Jose C. Payumo Jr.", "Jose C. Payumo, Jr.", bname),
           bname = ifelse(bname == "Juan Climaco Sr. (Magdugo)", "Juan Climaco, Sr. (Magdugo)", bname))
  
  # Change strings to uppercase
  psa_new[[2]] <- toupper(psa_new[[2]])
  psa_new[[3]] <- toupper(psa_new[[3]])
  psa_new[[4]] <- toupper(psa_new[[4]])
  
  ##### SELECTED PROVINCES
  
  # Select and rename columns and fix mismatched names
  prov <- prov %>% 
    select(Province, selected) %>%
    rename(pname = Province) %>%	
    mutate(selected = ifelse(selected == "Sampled", 1, 0)) %>%
    mutate(pname = ifelse(pname == "Compostela Valley", 
                                   "DAVAO DE ORO (COMPOSTELA VALLEY)", pname))
  
  # Change string to uppercase
  prov[[1]] <- toupper(prov[[1]])
  
  ##### BARANGAY DATA
  
  # Select relevant variables and recode
  bgy <- bgy %>% 
    select(mcode, mname, city_class, pname, bcode, bname, location, population) %>%
    mutate(urban = ifelse(location == "Urban", 1, 0))

  # Remove one barangay with no population or other info (not in PSA data either)
  bgy <- filter(bgy, bcode != "045812021") #  Madilay-dilay	in Rizal
  
  nrow(bgy) # 42045
  
  # Change string to uppercase
  bgy[[2]] <- toupper(bgy[[2]])
  bgy[[6]] <- toupper(bgy[[6]])
  
  ##### TARGET DATA
  
  # Select and rename columns
  target <- select(target, 2:6)
  names(target) <- c("pname", "mname", "bname", "pmt_hh", "notes")  
  nrow(target)
  
  # Remove empty and total rows
  target <- filter(target, !is.na(pname))
  nrow(target) # 17693
  
  # Change strings to match
  target <- target %>%
    mutate(pname = ifelse(pname == "COMPOSTELA VALLEY", 
                        "DAVAO DE ORO (COMPOSTELA VALLEY)", pname),
           mname = ifelse(mname == "PINAMUNGAHAN", "PINAMUNGAJAN", mname))

  # Change string to uppercase
  target[[2]] <- toupper(target[[2]])

#########################################################################   
#               MERGE DATA AND CREATE VARIABLES                         #   
#########################################################################  
  
  ##### MERGE DATA
  
  # Merge province selection and barangays, filter out bgy not doing Step 1
  all <- full_join(prov, bgy,  by = c("pname")) %>%
    filter(!is.na(selected))
  nrow(all) # 23203
  
  # Merge in target levels
  all <- full_join(all, target, by = c("pname", "mname", "bname"))
  nrow(all) # 23203
  
  # Merge in PSA data on step 1 completion for selected provinces (Oct 31)
  all <- full_join(all, psa, by = c("pname", "mname", "bname")) 
  nrow(all) #  23203
  
  # Merge in PSA data on step 1 completion for selected provinces (7 Nov)
  all <- full_join(all, psa_new, by = c("pname", "mname", "bname")) 
  nrow(all) #  23203
  
  ##### CREATE  VARIABLES
  
  # Create target variable
  all <- all %>% 
    mutate(target = ifelse(notes == "HH head only", pmt_hh, pmt_hh*2))
  
  # Calulate percentage registered and change
  all <- all %>%
    mutate(per_pop_31oct = registered_31oct/population*100,
           per_target_31oct = registered_31oct/target*100,
           per_pop_7nov = registered_7nov/population*100,
           per_target_7nov = registered_7nov/target*100,
           registered_change = registered_7nov - registered_31oct)
  
  # Create indicator for whether Barangay is in sampling frame,
  # limited to those in selected provinces with >50 registrations
  # NOTE: will need to merge in any no-go LGUs
  all <- all %>%
    mutate(frame = ifelse(selected == 0 | is.na(registered_7nov) | registered_7nov < 50, 0, 1))
  
  ##### SUBSET TO SELECTED PROVINCES ONLY
  
  # Barangays in selected provinces only 
  select <- filter(all, selected == 1)
  nrow(select) # 6547 total Barangays in sampled Provinces
  nrow(filter(select, !is.na(target))) # 5600 Barangays targeted 
  nrow(filter(select, !is.na(registered_7nov))) # 3617 with any registrations
  nrow(filter(select, frame == 1)) # 2481 have  more than 50 registrations (~50% of target)
  
  # Write all  Barangays in selected provinces
  write_xlsx(select, "data_clean/sampling/barangays_in_selected_provinces.xlsx")
  
#########################################################################   
#               DESCRIBE DATA                                           #   
#########################################################################   
  
  # Summary tables of key variables by province and urban/rural
  table1(~  population + registered_7nov + per_pop_7nov + per_target_7nov + registered_change | pname + location, 
         data = filter(select, selected == 1))
  
  table1(~  population + registered_7nov + per_pop_7nov + per_target_7nov + registered_change | pname + location, 
         data = filter(select, selected == 1 & location == "Urban"))
  
  table1(~  population + registered_7nov + per_pop_7nov + per_target_7nov + registered_change| pname, 
         data = filter(select, selected == 1), export = "summary")
  
  # Histogram of percent registered/target
  ggplot(data = filter(select, !is.na(per_target_7nov))) +
    geom_histogram(aes(x = per_pop_7nov), fill = "black") +
    facet_wrap(~pname) +
    labs( x = "Step 1 registrations per barangay as a percent of target (as of Nov 7)", y = "") + 
    scale_y_continuous(expand = c(0,0)) +
    theme_bw()
  ggsave("output/sampling/registrations_7nov.pdf", plot = last_plot())
  
  # Histogram of increase in registration since Oct 31
  ggplot(data = filter(select, !is.na(registered_change))) +
    geom_histogram(aes(x = registered_change), fill = "black") +
    facet_wrap(~pname) +
    scale_y_continuous(limits = c(0, 350)) +
    labs( x = "Step 1 registrations per Barangay since October 31", y = "") + 
    scale_y_continuous(expand = c(0,0)) +
    theme_bw()
  ggsave("output/sampling/registrations_since_31oct.pdf", plot = last_plot())
  
  
  # Calculate number of barangays per province and municipality
  bgy_prov <- select %>% 
    group_by(pname) %>% 
    summarise(Prov_Bgy = n()) 
  bgy_mun  <- select %>%
    group_by(pname, mname) %>% 
    summarise(Mun_Bgy = n()) 
  bgy_prov_frame <- select %>% 
    group_by(pname, frame) %>% 
    summarise(Prov_Bgy_Frame = n()) %>%
    filter(frame == 1)
  bgy_mun_frame  <- select %>%
    group_by(pname, mname, frame) %>% 
    summarise(Mun_Bgy_Frame = n()) %>%
    filter(frame == 1)
  
  # Summary table of Barangays stratified by Province, municipality and urban/rural
  pmb <- select %>%
    filter(selected == 1) %>%
    group_by(pname, mname, frame, location) %>% 
    summarise(
      N_Bgy = n(), 
      Population_Mean = round(mean(population, na.rm = TRUE),1),
      Population_SD = round(sd(population, na.rm = TRUE),1),
      Registered_Mean = round(mean(registered_7nov, na.rm = TRUE),1),
      Registered_SD = round(sd(registered_7nov, na.rm = TRUE),1),
      Percent_Target_Mean = round(mean(per_target_7nov, na.rm = TRUE),1),
      Percent_Target_SD = round(sd(per_target_7nov, na.rm = TRUE),1),
    ) %>%
    full_join(bgy_mun, by = c("pname", "mname")) %>%
    full_join(bgy_mun_frame, by = c("pname", "mname", "frame")) %>%
    mutate(Per_Bgy = round(N_Bgy/Mun_Bgy*100,1)) %>%
    mutate(Per_Bgy_Frame = round(N_Bgy/Mun_Bgy_Frame*100,1)) %>%
    select(pname, mname, frame, location, N_Bgy, Per_Bgy, Per_Bgy_Frame, everything(), -c(Mun_Bgy, Mun_Bgy_Frame))
  pmb
  
  # Summary table of Barangays stratified by Province and urban/rural
  pb <- select %>%
    filter(selected == 1) %>%
    group_by(pname, frame, location) %>% 
    summarise(
      N_Bgy = n(), 
      Population_Mean = round(mean(population, na.rm = TRUE),1),
      Population_SD = round(sd(population, na.rm = TRUE),1),
      Registered_Mean = round(mean(registered_7nov, na.rm = TRUE),1),
      Registered_SD = round(sd(registered_7nov, na.rm = TRUE),1),
      Percent_Target_Mean = round(mean(per_target_7nov, na.rm = TRUE),1),
      Percent_Target_SD = round(sd(per_target_7nov, na.rm = TRUE),1),
    ) %>%
    full_join(bgy_prov, by = c("pname")) %>%
    full_join(bgy_prov_frame, by = c("pname", "frame")) %>%
    mutate(Per_Bgy = round(N_Bgy/Prov_Bgy*100,1)) %>%
    mutate(Per_Bgy_Frame = round(N_Bgy/Prov_Bgy_Frame*100,1)) %>%
    select(pname, frame, location, N_Bgy, Per_Bgy, Per_Bgy_Frame, everything(), -c(Prov_Bgy, Prov_Bgy_Frame))
  pb
  
  
  # Save summary tables 
  write_xlsx(pmb, "data_clean/sampling/prov_mun_bgy_summary.xlsx")
  write_xlsx(pb, "data_clean/sampling/prov_bgy_summary.xlsx")
  
  
#########################################################################   
#               SAMPLING                                                #   
#########################################################################  
    
  
  
#########################################################################   
#               SAVE SELECTED PSUS                                      #   
#########################################################################   



  