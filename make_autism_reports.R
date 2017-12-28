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
  source("prep/read_autism_wsa.R")
  write_feather(wsa,"data/wsa.feather")
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
  
  
  