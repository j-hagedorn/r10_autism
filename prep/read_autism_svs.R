
library(tidyverse); library(stringr); library(magrittr); library(lubridate)

# Read in .csv files as dataframes
csv_path <- "C:/Users/joshh/OneDrive - TBD Solutions LLC/files/Region10/Autism/"
genesee <- read_csv(paste0(csv_path,"Autism.Genesse.csv"), skip = 7)
lapeer <- read_csv(paste0(csv_path,"Autism.Lapeer.csv"), skip = 7)
sanilac <- read_csv(paste0(csv_path,"Autism.Sanilac.csv"), skip = 7)
stclair <- read_csv(paste0(csv_path,"Autism.StClair.csv"), skip = 7)

# Bind separate CMH dataframes together
svs <- genesee %>% bind_rows(lapeer,sanilac,stclair)

# Remove separate files from environment
rm(csv_path); rm(genesee); rm(lapeer); rm(sanilac); rm(stclair)

# Remove cols where all values are NA
svs <- Filter(function(x)!all(is.na(x)), autism_svs)

# Clean colnames (rm spaces, other symbols, add underscore sep)
names(svs) <- gsub(":", "", names(autism_svs))
names(svs) <- gsub(" |-", "_", names(autism_svs))

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
    .cols = vars(PRV_ID,CON_ID,CLM_ID:PRIM_INS_ID),
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
    .cols = vars(HAB_WAIVER:HMP_BUCKET),
    .funs = funs(. == "Y")
  ) 

