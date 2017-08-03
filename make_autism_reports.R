# make_autism_reports.R

library(tidyverse); library(magrittr)

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
  
  # Recode CMH names for consistent reference 
  svs %<>%
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
  write_csv(svs,paste0(csv_path,"svs.csv"))
  
# Read WSA 
  # Read wsa file
  wsa <- read_excel(paste0(csv_path,"WSA All Cases All Data 05-30-17.xls"), skip = 1)
  # Clean colnames (rm spaces, other symbols, add underscore sep)
  names(wsa) <- gsub(" |-|/", "_", names(wsa))
  names(wsa) <- gsub("[[:space:]]", "", names(wsa))
  
  # Recode CMH names for consistent reference 
  wsa %<>%
    mutate(
      PIHP_CMH_Name = recode(
        PIHP_CMH_Name,
        `Region 10 - Genesee` = "Genesee Health System",
        `Region 10 - Lapeer` = "Lapeer County CMH",
        `Region 10 - St. Clair` = "St. Clair County CMH",
        `Region 10 - Sanilac` = "Sanilac County CMH"
      )
    )
  
  write_csv(wsa,paste0(csv_path,"wsa.csv"))
  
  # Read in data related to authorized units
  wsa_ipos <- read_excel(paste0(csv_path,"06-08-17 WSA IPOS Data Report.xls"), skip = 1)
  
  # Clean colnames (rm spaces, other symbols, add underscore sep)
  names(wsa_ipos) <- gsub(" |-|/", "_", names(wsa_ipos))
  names(wsa_ipos) <- gsub("[[:space:]]", "", names(wsa_ipos))
  
  wsa_ipos %<>%
    mutate(
      PIHP_CHM = recode(
        PIHP_CHM,
        `Region 10 - Genesee` = "Genesee Health System",
        `Region 10 - Lapeer` = "Lapeer County CMH",
        `Region 10 - St. Clair` = "St. Clair County CMH",
        `Region 10 - Sanilac` = "Sanilac County CMH"
      )
    )
  
  write_csv(wsa_ipos,paste0(csv_path,"wsa_ipos.csv"))


# Run reports ####

  # Render combined detail report for PIHP
  rmarkdown::render("r10_autism_report.Rmd")
  
  # Render reports per CMH
  rmarkdown::render("r10_autism_report_by_cmh.Rmd")
  rmarkdown::render("r10_autism_report_by_cmh.Rmd", params = list(cmh = region))
  