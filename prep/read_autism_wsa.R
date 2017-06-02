
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
    .cols = vars(Days_Bet_Ref_Eval,Days_Bet_Elig_IPOS),
    .funs = funs(as.numeric)
  ) %>%
  # Change all character columns to factors
  mutate_if(is.character,as.factor) %>%
  # Transform Y/N responses into logical vars
  mutate_at(
    .cols = vars(
      IPOSExists,
      Telepractice_Authorization_Requested,
      Currently_Inactive,
      Has_Past_Inactive
    ),
    .funs = funs(. == "Yes")
  )
  
# If closed and opened, client is given a new 'Case ID' 
# but Medicaid ID (i.e. 'ID') should be the same