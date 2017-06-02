# merge_wsa_svs.R #

# Merge WSA Autism enrollee data with related service encounters


# 1) % of autism waiver eligible individuals, active (enrolled in WSA) 
#    with a plan (can pull as an excel extract) 
#    and receiving services (they have this report now but requires calc) 
# 2) For each individual who is autism waiver eligible and on the waitlist, 
#    what services are he/she receiving while they are waiting? At what frequency?
# 3) For individuals who are receiving autism waiver services, 
#    what is the amount of services and does it match their plan? 
#    What is % of authorized hours that have been provided per person?

waitlist_svs <-
  wsa %>%
  # Filter individuals who are on waitlist
  filter(
    Status == "Open"
    & IPOSExists == F
  ) %>%
  select(ID,PIHP_CMH_Name,Referral_Date:Eligibility_End_Date) %>%
  left_join(svs, by = c("ID" = "MEDICAID_ID")) %>%
  # Only include services after referral for autism
  filter(FROM_DATE >= as.Date(Referral_Date)) %>%
  group_by(ID,PIHP_CMH_Name,CPT_CD,SERVICE_DESC,UNIT_TYPE,AUTISM_SRV) %>%
  summarize(units = sum(UNITS, na.rm = T))

