
library(tidyverse); library(readxl)

# Read in .csv files as dataframes
csv_path <- "C:/Users/joshh/OneDrive - TBD Solutions LLC/files/Region10/Autism/"
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
  
# If closed and opened, client is given a new 'Case ID' 
# but Medicaid ID (i.e. 'ID') should be the same

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
