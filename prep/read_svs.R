
# Install packages if you need them
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("stringr")) install.packages("stringr")
if (!require("magrittr")) install.packages("magrittr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("plotly")) install.packages("plotly")

# Load packages
library(tidyverse); library(stringr); library(magrittr); library(lubridate)

# Define function to read and combine ####
combineServices <- function(directory) {
  ## 'directory' is a char vector of len 1 indicating location of CSV files
  files <- list.files(directory,full.names = TRUE) # make list of full file names
  n <- length(files)
  # Create empty data frame
  df <- tibble() 
  # Loop through files, binding them together
  for (i in 1:n) {
    x <- read_csv(files[i], skip = 7)
    df <- rbind(df, x)
  } 
  df
}

# Read in .csv files as dataframes
directory <- "C:/Users/joshh/OneDrive - TBD Solutions LLC/files/Region10/Autism/services"
# Bind separate CMH dataframes together
svs <- combineServices(directory)

# Remove cols where all values are NA
svs <- Filter(function(x)!all(is.na(x)), svs)

# Clean colnames (rm spaces, other symbols, add underscore sep)
names(svs) <- gsub(":", "", names(svs))
names(svs) <- gsub(" |-", "_", names(svs))

# Clean data to prepare for analysis
svs <-
svs %>%
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
    FROM_DATE = str_trim(FROM_DATE),
    FROM_DATE = case_when(
      # If it is formatted as a date (with /)
      grepl("/",FROM_DATE) == T        ~ mdy(FROM_DATE),
      # If it can be converted to numeric (from string).
      # Note: uses the Unix epoch of "1970-01-01"
      is.na(as.numeric(FROM_DATE)) == F ~ as.Date(as.numeric(FROM_DATE), origin = "1970-01-01")
    ),
    THRU_DATE = str_trim(THRU_DATE),
    THRU_DATE = case_when(
      # If it is formatted as a date (with /)
      grepl("/",THRU_DATE) == T        ~ mdy(THRU_DATE),
      # If it can be converted to numeric (from string).
      # Note: uses the Unix epoch of "1970-01-01"
      is.na(as.numeric(THRU_DATE)) == F ~ as.Date(as.numeric(THRU_DATE), origin = "1970-01-01")
    )
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
  
