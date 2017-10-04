# make_autism_reports.R

library(tidyverse); library(magrittr); library(stringr);library(lubridate);library(feather);library(readxl)

# Read in data ####

# Combine datasets as files to be read by subsequent .Rmd files 
# (to avoid running multiple times)

  # Read in .csv files as dataframes
  csv_path <- "C:/Users/joshh/OneDrive - TBD Solutions LLC/files/Region10/Autism/"
  genesee <- read_csv(paste0(csv_path,"Autism.Genesse.csv"), skip = 7)
  lapeer <- read_csv(paste0(csv_path,"Autism.Lapeer.csv"), skip = 7)
  sanilac <- read_csv(paste0(csv_path,"Autism.Sanilac.csv"), skip = 7)
  stclair <- read_csv(paste0(csv_path,"Autism.StClair.csv"), skip = 7)
  # Bind separate CMH dataframes together
  svs <- genesee %>% bind_rows(lapeer,sanilac,stclair)
  rm(genesee);rm(lapeer);rm(sanilac);rm(stclair)
  # Remove cols where all values are NA
  svs <- Filter(function(x)!all(is.na(x)), svs)
  
  # Clean colnames (rm spaces, other symbols, add underscore sep)
  names(svs) <- gsub(":", "", names(svs))
  names(svs) <- gsub(" |-", "_", names(svs))
  
  # Clean data to prepare for analysis
  svs %<>%
    # Clean Medicaid ID field
    mutate(
      # Trim lead / trail whitespace
      MEDICAID_ID = str_trim(MEDICAID_ID),
      # Remove alpha and special chars
      MEDICAID_ID = str_replace_all(MEDICAID_ID, "[[:alpha:]]", ""),
      MEDICAID_ID = str_replace_all(MEDICAID_ID, "[[:punct:]]", ""),
      # Convert blanks to NA
      MEDICAID_ID = ifelse(MEDICAID_ID == "", yes = NA, no = MEDICAID_ID), 
      # If string > 10 chars, include only last 10 chars
      MEDICAID_ID = ifelse(nchar(as.character(MEDICAID_ID)) > 10,
                           yes = substr(MEDICAID_ID, 
                                        start = nchar(as.character(MEDICAID_ID)) - 9, 
                                        stop = nchar(as.character(MEDICAID_ID))),
                           no = MEDICAID_ID),
      # If string < 10 chars, pad with leading zeroes
      MEDICAID_ID = ifelse(nchar(as.character(MEDICAID_ID)) < 10,
                           yes = sprintf("%010d", as.integer(MEDICAID_ID)),
                           no = MEDICAID_ID),
      # Make 'NA' & 0000000000 to NA
      MEDICAID_ID = ifelse(MEDICAID_ID %in% c("        NA","NA","0000000000"), 
                           yes = NA,
                           no = MEDICAID_ID),
      # Convert to factor
      MEDICAID_ID = as.factor(MEDICAID_ID)
    ) %>%
    # Change numeric ID vars to characters
    mutate_at(
      .vars = vars(PRV_ID,CON_ID,CLM_ID:PRIM_INS_ID),
      .funs = funs(as.character)
    ) %>%
    # Change all character columns to factors
    mutate_if(is.character,as.factor) %>%
    # Clean date fields to prepare for analysis
    mutate(
      FROM_DATE = mdy(FROM_DATE),
      THRU_DATE = mdy(THRU_DATE)
    ) %>%
    # Transform Y/N responses into logical vars
    mutate_at(
      .vars = vars(HAB_WAIVER:HMP_BUCKET),
      .funs = funs(. == "Y")
    ) %>%
    # Recode CMH names for consistent reference 
    mutate(
      PROVIDER_NAME = recode(
        PROVIDER_NAME,
        `6-Genesee Health System` = "Genesee Health System",
        `2-Lapeer County Community Mental Health` = "Lapeer County CMH",
        `3-St. Clair County Community Mental Health` = "St. Clair County CMH",
        `4-Sanilac County Community Mental Health` = "Sanilac County CMH"
      )
    )
  
  # Write to file
  # Note that this will overwrite any previous files with the same name
  write_feather(svs,"data/svs.feather")
  
# Read WSA 
  # Read wsa file
  wsa <- read_excel(paste0(csv_path,"WSA All Cases All Data 05-30-17.xls"), skip = 1)
  # Clean colnames (rm spaces, other symbols, add underscore sep)
  names(wsa) <- gsub(" |-|/", "_", names(wsa))
  names(wsa) <- gsub("[[:space:]]", "", names(wsa))
  
  wsa %<>%
    mutate(
      Case_ID = as.character(Case_ID)
    ) %>%
    mutate_at(
      .vars = vars(Days_Bet_Ref_Eval,Days_Bet_Elig_IPOS),
      .funs = funs(as.numeric)
    ) %>%
    # Change all character columns to factors
    mutate_if(is.character,as.factor) %>%
    # Transform Y/N responses into logical vars
    mutate_at(
      .vars = vars(
        IPOSExists,
        Telepractice_Authorization_Requested,
        Currently_Inactive,
        Has_Past_Inactive
      ),
      .funs = funs(. == "Yes")
    ) %>%
    # Recode CMH names for consistent reference
    mutate(
      PIHP_CMH_Name = recode(
        PIHP_CMH_Name,
        `Region 10 - Genesee` = "Genesee Health System",
        `Region 10 - Lapeer` = "Lapeer County CMH",
        `Region 10 - St. Clair` = "St. Clair County CMH",
        `Region 10 - Sanilac` = "Sanilac County CMH"
      )
    )
  
  write_feather(wsa,"data/wsa.feather")
  
  # Read in data related to authorized units
  wsa_ipos <- read_excel(paste0(csv_path,"06-08-17 WSA IPOS Data Report.xls"), skip = 1)
  
  # Clean colnames (rm spaces, other symbols, add underscore sep)
  names(wsa_ipos) <- gsub(" |-|/", "_", names(wsa_ipos))
  names(wsa_ipos) <- gsub("[[:space:]]", "", names(wsa_ipos))
  
  wsa_ipos %<>%
    mutate(
      Case_ID = as.character(Case_ID)
    ) %>%
    mutate_at(
      .vars = vars(Days_Without_IPOS,Next_IPOS_Due_In,ABA_Hours),
      .funs = funs(as.numeric)
    ) %>%
    # Change all character columns to factors
    mutate_if(is.character,as.factor) %>%
    # Transform Y/N responses into logical vars
    mutate_at(
      .vars = vars(IPOS_Exists),
      .funs = funs(. == "Yes")
    ) %>%
    mutate(
      PIHP_CHM = recode(
        PIHP_CHM,
        `Region 10 - Genesee` = "Genesee Health System",
        `Region 10 - Lapeer` = "Lapeer County CMH",
        `Region 10 - St. Clair` = "St. Clair County CMH",
        `Region 10 - Sanilac` = "Sanilac County CMH"
      )
    )
  
  write_feather(wsa_ipos,"data/wsa_ipos.feather")



# Run reports ####

  # Render reports per CMH (if this is run last, the data will remain filtered in the environment)
  for (cmh_name in unique(svs$PROVIDER_NAME)) {
    rmarkdown::render(
      input = "r10_autism_report_by_cmh.Rmd", 
      output_file = paste0("r10_autism_summary_",cmh_name,"_",max(svs$FROM_DATE),".html"),
      output_dir = "reports/by_cmh",
      params = list(cmh = cmh_name)
    )
  }
  
  # Render summary report for high-level review
  rmarkdown::render(
    input = "r10_autism_summary_report.Rmd",
    output_file = paste0("r10_autism_summary_",max(svs$FROM_DATE),".html"),
    output_dir = "reports"
  )
  
  # Render combined detail report for PIHP
  rmarkdown::render(
    input = "r10_autism_report.Rmd",
    output_file = paste0("r10_autism_report_",max(svs$FROM_DATE),".html"),
    output_dir = "reports"
  )
  
  
  