# merge_wsa_svs.R #

# Merge WSA Autism enrollee data with related service encounters

# 1) % of autism waiver eligible individuals, active (enrolled in WSA) 
#    with a plan (can pull as an excel extract) 
#    and receiving services (they have this report now but requires calc) 

wsa %>%
  filter(Status == "Open") %>%
  group_by(PIHP_CMH_Name) %>%
  summarize(
    with_IPOS = sum(IPOSExists, na.rm = T),
    all = n()
  ) %>%
  mutate(
    pct = round(with_IPOS / all * 100, digits = 1)
  ) %>%
  View()

# 2) For each individual who is autism waiver eligible and on the waitlist, 
#    what services are he/she receiving while they are waiting? At what frequency?

waitlist_svs <-
  wsa %>%
  # Filter individuals who are on waitlist
  # Is this an accurate way to get at these individuals?
  filter(
    Status == "Open"
    & IPOSExists == F
  ) %>%
  select(ID,PIHP_CMH_Name,Referral_Date:Eligibility_End_Date) %>%
  left_join(svs, by = c("ID" = "MEDICAID_ID")) %>%
  filter(
    # Only include services occurring after referral for autism
    FROM_DATE >= as.Date(Referral_Date) 
    # or if there is no service date (to keep people who didn't receive svs)
    | is.na(FROM_DATE) == T 
  ) %>%
  group_by(ID,PIHP_CMH_Name,CPT_CD,SERVICE_DESC,UNIT_TYPE,AUTISM_SRV) %>%
  # What is 
  summarize(units = sum(UNITS, na.rm = T))


# 3) For individuals who are receiving autism waiver services, 
#    what is the amount of services and does it match their plan? 
#    What is % of authorized hours that have been provided per person?

#### Calculate ABA services per week per person ####

# Define ABA codes
aba_cpt <- c("0364T","0365T","0366T","0367T","0368T","0369T",
             "0370T","0371T","0372T","0373T","0374T")

aba_rx_hrs <- wsa_ipos %>% select(MEDICAID_ID = Beneficiary_ID,ABA_Hours)

ipos_start <- wsa %>% filter(Status == "Open") %>% select(MEDICAID_ID = ID,IPOS_Start_Date)

aba_week <-
svs %>%
  filter(CPT_CD %in% aba_cpt) %>%
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
    diff = hrs - ABA_Hours
  ) 

rm(aba_rx_hrs);rm(ipos_start)

# For fun
library(plotly)
aba_week %>% plot_ly(x = ~week, y = ~diff, color = ~MEDICAID_ID) %>% add_lines()
