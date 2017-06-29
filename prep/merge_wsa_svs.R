# merge_wsa_svs.R #

library(tidyverse); library(lubridate)

# Merge WSA Autism enrollee data with related service encounters

# 1) % of autism waiver eligible individuals, active (enrolled in WSA) 
#    with a plan (can pull as an excel extract) 
#    and receiving services (they have this report now but requires calc) 

wsa %>%
  filter(Status == "Open") %>%
  group_by(PIHP_CMH_Name) %>%
  summarize(
    without_IPOS = n() - sum(IPOSExists, na.rm = T),
    all = n()
  ) %>%
  mutate(
    pct = round(without_IPOS / all * 100, digits = 1)
  ) %>%
  View()

# 2) For each individual who is autism waiver eligible and on the waitlist, 
#    what services are he/she receiving while they are waiting? At what frequency?

# The df created below shows services received since eligibility date (for those on the wait list) - 
# It includes one row for each service code by individual; 

setdiff(
  unique(wsa$ID[wsa$Status == "Open"]),
  unique(svs$MEDICAID_ID)
)

waitlist_svs <-
  wsa %>%
  # Filter individuals who are on waitlist
  # Is this an accurate way to get at these individuals?
  filter(
    Status == "Open"
    & IPOSExists == F
    & is.na(Eligibility_Date) == F
  ) %>%
  select(ID,PIHP_CMH_Name,Referral_Date:Eligibility_End_Date) %>%
  left_join(svs, by = c("ID" = "MEDICAID_ID")) %>%
  filter(
    # Only include services occurring after eligibility for autism
    FROM_DATE >= as.Date(Eligibility_Date) 
    # or if there is no service date (to keep people who didn't receive svs)
    | is.na(FROM_DATE) == T 
  ) %>%
  group_by(ID,PIHP_CMH_Name,CPT_CD,SERVICE_DESC,UNIT_TYPE,AUTISM_SRV,Eligibility_Date) %>%
  # Convert units to a common period
  mutate(
    # Recode unit type as numeric conversion factor
    hr_conv = recode(
      as.character(UNIT_TYPE),
      `15 Minutes` = '0.25', 
      `30 Minutes` = '0.5',
      `Hour` = '1', 
      `Up to 15 min` = '0.25',
      `Encounter` = 'NA'
    ),
    svs_conv = recode(
      as.character(SERVICE_DESC),
      `Psychotherapy, 60 min` = 1, 
      `Psychotherapy, 45 min` = 0.75,
      `Psychotherapy, 30 min` = 0.5
    ),
    hr_conv = ifelse(is.na(svs_conv) == F,svs_conv,as.numeric(hr_conv)),
    # Calculate hours
    hrs = UNITS * hr_conv
  ) %>%
  # What is the first and last date of each service?
  mutate(
    first = min(FROM_DATE),
    last = max(FROM_DATE)
  ) %>%
  # What is the number of units received?
  summarize(
    units = sum(UNITS, na.rm = T),
    hrs = sum(hrs, na.rm = T),
    first = min(FROM_DATE),
    last = max(FROM_DATE)
  )

# there is a need for the detailed report by individual by service - 
# last date of service received, one row per person 
# that shows a list of services received and the first and last 
# date of those services; 

waitlist_person <-
  waitlist_svs %>%
  group_by(ID) %>%
  summarize(
    first = min(first),
    last = max(last),
    Eligibility_Date = as.Date(max(Eligibility_Date)),
    svs_list = paste(SERVICE_DESC, collapse = ', '),
    hrs = sum(hrs)
  ) %>%
  mutate(
    # Most recent date in services data pulled
    max_date = max(svs$FROM_DATE),
    elig_interval = as.numeric(max_date - Eligibility_Date),
    svs_interval = as.numeric(last - first),
    pct_days_svd = round(svs_interval/elig_interval * 100, digits = 1)
  )
  
# and a summary report showing regional trends - 
# we don't want to smooth over individuals who have received 0 services by 
# just reporting total units of services provided, rather we need to show 
# if we made a good faith effort for everyone on the wait list (expected 
# benefit package while on wait list - type of service and frequency per 
# month or week: SC or TCM at least one unit monthly, CLS at least one unit 
# monthly, respite at least one unit monthly, family training at least 15 
# min unit monthly, at least one med review encounter monthly. 

wsa_elig_dt <- wsa %>% select(ID,Eligibility_Date)

min_waitlist_svs <-
  wsa %>%
  # Filter individuals who are on waitlist
  # Is this an accurate way to get at these individuals?
  filter(
    Status == "Open"
    & IPOSExists == F
    & is.na(Eligibility_Date) == F
  ) %>%
  select(ID,PIHP_CMH_Name,Referral_Date:Eligibility_End_Date) %>%
  left_join(svs, by = c("ID" = "MEDICAID_ID")) %>%
  filter(
    # Only include services occurring after eligibility for autism
    FROM_DATE >= as.Date(Eligibility_Date) 
    # or if there is no service date (to keep people who didn't receive svs)
    | is.na(FROM_DATE) == T 
  ) %>%
  filter(
    # Only include 'core' services defined by Region 10
    CPT_CD %in% c("T1016","T1017","H2015","T1005","0370T","90792",as.character(99201:99215))
  ) %>%
  mutate(
    # Create month dates for grouping
    month = floor_date(FROM_DATE, unit = "month")
  ) %>%
  droplevels() %>%
  ungroup() %>%
  group_by(month,ID,CPT_CD) %>%
  summarize(units = sum(UNITS, na.rm = T)) %>%
  ungroup() %>%
  # Create a dataframe with all combinations of the vars listed
  # in order to ensure that months without services are identified
  complete(month,ID,CPT_CD) %>%
  group_by(ID,month) %>%
  # Spread to show one row per person per month
  spread(CPT_CD,units) %>%
  # Join back to get eligibility date to filter months for each person that precede
  left_join(wsa_elig_dt, by = "ID") %>%
  filter(month >= floor_date(Eligibility_Date, unit = "month")) %>%
  # Convert NA values to zeroes
  mutate_at(
    .cols = vars(`0370T`:`T1017`), 
    .funs = funs(replace(., is.na(.), 0))
  ) %>%
  # Sum units for services to identify criteria
  mutate(
    cm = T1016 + T1017,
    cls = H2015,
    respite = T1005,
    med_rvw = `90792` + `99212` + `99213` + `99214` + `99215`,
    fam_svs = `0370T`
  ) %>%
  select(ID,month,cm,cls,respite,med_rvw,fam_svs) %>%
  mutate(
    min_svs = cm >= 1 & cls >= 1 & respite >= 1 & med_rvw >= 1 & fam_svs >= 1
  ) %>%
  arrange(ID, month)
  
# write.csv(min_waitlist_svs,"min_waitlist_svs.csv",row.names = F)

# 3) For individuals who are receiving autism waiver services, 
#    what is the amount of services and does it match their plan? 
#    What is % of authorized hours that have been provided per person?

#### Calculate ABA services per week per person ####

# Define ABA codes
aba_cpt <- c("S5108", # Old observation code
             "H2019", # Old treatment codes
             "0364T","0365T","0366T","0367T","0372T","0373T","0374T",
             "0368T","0369T") # Observation codes

aba_rx_hrs <- wsa_ipos %>% select(MEDICAID_ID = Beneficiary_ID,ABA_Hours)

ipos_start <- 
  wsa %>% 
  filter(Status == "Open" & Currently_Inactive == F) %>% 
  select(MEDICAID_ID = ID,IPOS_Start_Date)

aba_week <-
svs %>%
  filter(
    CPT_CD %in% aba_cpt
    & USED_MOD == "U5" # Only include U5 modifier for Autism services
  ) %>%
  droplevels() %>%
  mutate(
    # Recode unit type as numeric conversion factor
    hr_conv = recode(
      as.character(UNIT_TYPE),
      `15 Minutes` = '0.25', 
      `30 Minutes` = '0.5',
      `Hour` = '1', 
      `Up to 15 min` = '0.25',
      `Encounter` = 'NA'
    ),
    hr_conv = as.numeric(hr_conv),
    # Calculate hours
    hrs = UNITS * hr_conv,
    # Create week dates for grouping
    week = floor_date(FROM_DATE, unit = "week")
  ) %>%
  select(PROVIDER_NAME,MEDICAID_ID,CPT_CD:FROM_DATE,week,UNITS,hrs) %>%
  group_by(MEDICAID_ID,week) %>%
  summarize(hrs = sum(hrs, na.rm = T)) %>%
  # Get prescribed ABA hours from WSA IPOS
  left_join(aba_rx_hrs, by = "MEDICAID_ID") %>%
  # Get IPOS start date from WSA
  left_join(ipos_start, by = "MEDICAID_ID") %>%
  # Only include services if they occur after the IPOS Start Date
  filter(week >= IPOS_Start_Date) %>%
  mutate(
    # Calculate difference between actual and prescribed hours
    # Negative values indicate hours less than goal
    diff = hrs - ABA_Hours,
    # Calculate % of prescribed hours received
    pct = round(hrs / ABA_Hours * 100, digits = 1),
    # Tag Compliant and Non-Compliant weeks
    in_range_wk = ifelse(between(pct,75,125),T,F)
  ) %>%
  group_by(MEDICAID_ID) %>%
  mutate(
    pct_all = round(sum(hrs) / sum(ABA_Hours) * 100, digits = 1),
    in_range_all = ifelse(between(pct_all,75,125),T,F)
  ) 

rm(aba_rx_hrs);rm(ipos_start)

# For fun
library(plotly)

p <-
aba_week %>% 
  filter(in_range_all == T) %>%
  plot_ly(x = ~week, y = ~pct, colors = c("#F2300F","#0B775E")) %>% 
  add_lines(opacity = 0.5,color = ~in_range_wk) %>%
  add_trace(type = "scatter", mode = "markers", color = ~in_range_wk) 


  
#### Are we providing appropriate observation hours? ####

# According to the Autism Waiver requirements
# For every 10 (units,encounters,hours?) of a treatment code 
# from set {"0364T","0365T","0366T","0367T","0372T","0373T","0374T","H2019"} 
# which are provided to an individual client, 
# there should be 1 observation code from set {"S5108","0368T","0369T"}

# Note: uses filters defined above for service codes and clients included

observe <-
svs %>%
  filter(
    CPT_CD %in% aba_cpt
    & USED_MOD == "U5" # Only include U5 modifier for Autism services
  ) %>%
  droplevels() %>%
  mutate(
    # Recode unit type as numeric conversion factor
    hr_conv = recode(
      as.character(UNIT_TYPE),
      `15 Minutes` = '0.25', 
      `30 Minutes` = '0.5',
      `Hour` = '1', 
      `Up to 15 min` = '0.25',
      `Encounter` = 'NA'
    ),
    hr_conv = as.numeric(hr_conv),
    # Calculate hours
    hrs = UNITS * hr_conv,
    # Create week dates for grouping
    week = floor_date(FROM_DATE, unit = "week"),
    month = floor_date(FROM_DATE, unit = "month")
  ) %>%
  # Get prescribed ABA hours from WSA IPOS
  left_join(aba_rx_hrs, by = "MEDICAID_ID") %>%
  # Get IPOS start date from WSA
  left_join(ipos_start, by = "MEDICAID_ID") %>%
  # Only include services if they occur after the IPOS Start Date
  filter(week >= IPOS_Start_Date) %>%
  mutate(
    type = recode(
      CPT_CD,
      `S5108` = "Observation",
      `0368T` = "Observation",
      `0369T` = "Observation",
      `0364T` = "Treatment",
      `0365T` = "Treatment",
      `0366T` = "Treatment",
      `0367T` = "Treatment",
      `0372T` = "Treatment",
      `0373T` = "Treatment",
      `0374T` = "Treatment",
      `H2019` = "Treatment"
    )
  ) %>%
  select(
    MEDICAID_ID,PROVIDER_NAME,type,CPT_CD,SERVICE_DESC,
    month,week,FROM_DATE,UNITS,hrs
  ) %>%
  group_by(MEDICAID_ID,PROVIDER_NAME,type) %>%
  summarize(hrs = sum(hrs)) %>%
  ungroup() %>%
  spread(type, hrs) %>%
  mutate(
    obs_ratio = Observation / Treatment,
    meets_req = obs_ratio >= 0.1
  ) %>%
  group_by(PROVIDER_NAME) %>%
  summarize(
    meets_req = sum(meets_req),
    n = n()
  ) %>%
  mutate(pct = round(meets_req / n * 100, digits = 1))
