# merge_wsa_svs.R #

# Merge WSA Autism enrollee data with related service encounters


# 1) % of autism waiver eligible individuals, active (enrolled in WSA) 
#    with a plan (can pull as an excel extract) 
#    and receiving services (they have this report now but requires calc) 

filter(group_by(summarise(mutate(wsa))))

wsa %>%
  filter(Status == "Open") %>%
  group_by(PIHP_CMH_Name) %>%
  summarize(
    with_IPOS = sum(IPOSExists, na.rm = T),
    all = n()
  ) %>%
  mutate(
    pct = round(with_IPOS / all * 100, digits = 1)
  )

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


