# make_autism_reports.R

library(tidyverse); library(magrittr); library(stringr);library(lubridate);library(feather);library(readxl)

# Read in service data ####

# Combine datasets as files to be read by subsequent .Rmd files 
# (to avoid running multiple times)

  source("prep/read_svs.R")
  
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
  
  
  