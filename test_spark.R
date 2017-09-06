
## test_spark.R

df <-
  wsa %>%
  # Filter individuals who are on waitlist
  filter(
    Status == "Open"
    & IPOSExists == F
    & is.na(Eligibility_Date) == F
  ) %>%
  select(ID,Case_ID,PIHP_CMH_Name,Referral_Date:Eligibility_End_Date) %>%
  left_join(svs, by = c("ID" = "MEDICAID_ID")) %>%
  filter(
    # Only include services occurring after eligibility for autism
    FROM_DATE >= as.Date(Eligibility_Date) 
    # or if there is no service date (to keep people who didn't receive svs)
    | is.na(FROM_DATE) == T 
  ) %>%
  mutate(
    week = floor_date(FROM_DATE, unit = "week")
  ) %>%
<<<<<<< HEAD
  group_by(Case_ID,month) %>%
  summarize(Encounters = n()) %>%
  ungroup()

r <- range(df$Encounters) # define min/max # of encounters for sparkline

df %<>%
=======
  group_by(Case_ID,week) %>%
  summarize(encounters = n(),
            days = n_distinct(FROM_DATE)) %>%
  mutate(any = encounters > 0) %>%
  ungroup() %>%
>>>>>>> parent of f1c50c9... Sparklines
  group_by(Case_ID) %>%
  summarize(
    any = sum(any),
    n = n()
  ) %>%
  mutate(cover = round(any/n, digits = 1))

# Define attributes of line spark
<<<<<<< HEAD
cd <- list(list(targets = 1, render = JS("function(data, type, full){ return '<span class=spark>' + data + '</span>' }")))

line_string <- "type: 'line', lineColor: 'black', fillColor: '#ccc', highlightLineColor: 'orange', highlightSpotColor: 'orange'"

cb <- JS(paste0("function (oSettings, json) { $('.spark:not(:has(canvas))').sparkline('html', { ", 
                    line_string, ", chartRangeMin: ", r[1], ", chartRangeMax: ", r[2], " }); }"), 
             collapse = "")

=======
  line_string <- "type: 'line', lineColor: 'black', fillColor: '#ccc', highlightLineColor: 'orange', highlightSpotColor: 'orange'"
  cd <- list(list(targets = 2, render = JS("function(data, type, full){ return '<span class=sparkSeries>' + data + '</span>' }")))
  cb <- JS(paste0("function (oSettings, json) {\n  $('.sparkSeries:not(:has(canvas))').sparkline('html', { ", 
                 line_string, " });\n}"), collapse = "")
  
  
  
  
tst <-
df %>%
  ungroup() %>%
  group_by(Case_ID) %>% 
  filter(is.nan(encounters) == F) %>%
  #nest(encounters, .key = trend) %>%
  summarize(trend = paste(encounters,collapse=" ")) %>%
  data.table::data.table()
>>>>>>> parent of f1c50c9... Sparklines

d1 <- datatable(tst, rownames = F, options = list(columnDefs = cd, fnDrawCallback = cb))
d1$dependencies <- append(d1$dependencies, htmlwidgets:::getDependency("sparkline"))
d1

<<<<<<< HEAD
##############################################################################################################
##############################################################################################################
##############################################################################################################

library(tidyverse); library(stringr); library(magrittr); library(lubridate); library(readxl)
library(DT); library(forcats); library(plotly); library(sparkline);library(feather)
library(data.table); library(reshape2); library(dplyr)

waitlist_svs <-
  wsa %>%
  # Filter individuals who are on waitlist
  filter(
    Status == "Open"
    & IPOSExists == F
    & is.na(Eligibility_Date) == F
  ) %>%
  select(ID,Case_ID,PIHP_CMH_Name,Referral_Date:Eligibility_End_Date) %>%
  left_join(svs, by = c("ID" = "MEDICAID_ID")) %>%
  filter(
    # Only include services occurring after eligibility for autism
    FROM_DATE >= as.Date(Eligibility_Date) 
    # or if there is no service date (to keep people who didn't receive svs)
    | is.na(FROM_DATE) == T 
  ) %>%
  group_by(Case_ID,PIHP_CMH_Name,CPT_CD,SERVICE_DESC,UNIT_TYPE,AUTISM_SRV,Eligibility_Date) %>%
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
    encounters = n(),
    units = sum(UNITS, na.rm = T),
    hrs = sum(hrs, na.rm = T),
    first = min(FROM_DATE),
    last = max(FROM_DATE)
  ) 


waitlist_person <-
  waitlist_svs %>%
  group_by(Case_ID) %>%
  summarize(
    first = min(first),
    last = max(last),
    Eligibility_Date = as.Date(max(Eligibility_Date)),
    svs_list = paste(SERVICE_DESC, collapse = ', '),
    encounters = sum(encounters),
    autism_waiver_svs = sum(AUTISM_SRV, na.rm = T)
  ) %>%
  mutate(
    # Most recent date in services data pulled
    max_date = max(svs$FROM_DATE),
    elig_interval = as.numeric(max_date - Eligibility_Date),
    svs_interval = as.numeric(last - first),
    pct_days_svd = round(svs_interval/elig_interval * 100, digits = 1),
    autism_waiver_svs = autism_waiver_svs > 0
  )


t1 <- waitlist_person %>%
  select(Case_ID,svs_list,elig_interval,svs_interval,pct_days_svd,encounters,autism_waiver_svs) %>%
  ungroup() %>%
  arrange(pct_days_svd) 


t2 <-
  wsa %>%
  # Filter individuals who are on waitlist
  filter(
    Status == "Open"
    & IPOSExists == F
    & is.na(Eligibility_Date) == F
  ) %>%
  select(ID,Case_ID,PIHP_CMH_Name,Referral_Date:Eligibility_End_Date) %>%
  left_join(svs, by = c("ID" = "MEDICAID_ID")) %>%
  filter(
    # Only include services occurring after eligibility for autism
    FROM_DATE >= as.Date(Eligibility_Date) 
    # or if there is no service date (to keep people who didn't receive svs)
    | is.na(FROM_DATE) == T 
  ) %>%
  mutate(
    week = floor_date(FROM_DATE, unit = "week"),
    month = floor_date(FROM_DATE, unit = "month")
  ) %>%
  group_by(Case_ID,month) %>%
  summarize(Encounters = n()) %>%
  ungroup() %>%
  group_by(Case_ID) %>%
  summarise(
    Encounters = paste(Encounters, collapse = ","),
    Encounters2 = paste(Encounters, collapse = ",")) # remove this is boxplot not needed
  

r <- range(t2$Encounters) # define min/max # of encounters for sparkline

t3 <- merge(x = t2, y = t1, by = "Case_ID", all.x = TRUE)


# Define attributes of line spark
line_string <- "type: 'line', lineColor: 'black', fillColor: '#ccc', highlightLineColor: 'orange', highlightSpotColor: 'orange'"

box_string <- "type: 'box', lineColor: 'black', whiskerColor: 'black', outlierFillColor: 'black', outlierLineColor: 'black', medianColor: 'black', boxFillColor: 'orange', boxLineColor: 'black'"

#line chart and boxplot
cd <- list(list(targets = 1, render = JS("function(data, type, full){ return '<span class=sparkSeries>' + data + '</span>' }")),
           list(targets = 2, render = JS("function(data, type, full){ return '<span class=sparkSamples>' + data + '</span>' }")))

cb = JS(paste0("function (oSettings, json) {
               \n  $('.sparkSeries:not(:has(canvas))').sparkline('html', { ", line_string, " });
               \n  $('.sparkSamples:not(:has(canvas))').sparkline('html', { ", box_string, " });
               \n}")
        , collapse = "")



# line chart only
# cd <- list(list(targets = 7, render = JS("function(data, type, full){ return '<span class=sparkSeries>' + data + '</span>' }")))
# 
# cb = JS(paste0("function (oSettings, json) {
#                \n  $('.sparkSeries:not(:has(canvas))').sparkline('html', { ", line_string, " });
#                \n}")
#         , collapse = "")

=======
sparkline(tst$trend[91])

waitlist_person %>%
  select(Case_ID,svs_list,elig_interval,svs_interval,pct_days_svd,encounters) %>%
  ungroup() %>%
  arrange(pct_days_svd) %>%
  datatable(
    rownames = FALSE,
    colnames = c('Enrollee ID','Services provided','Days on waitlist','Days receiving services','Percent days covered','Number of encounters'),
    caption = 'Service coverage for individuals on Autism Waiver waitlist, by Enrollee',
    extensions = c('Responsive','Buttons'),
    options = list(
      dom = 't',
      buttons = c('colvis')
    )
  )
>>>>>>> parent of f1c50c9... Sparklines

dt <- t3 %>%
  datatable(
    data.table(),
    rownames = FALSE,
    colnames = c('Enrollee ID','Services provided','Days on waitlist','Days receiving services',
                 'Percent days covered','Total Number of encounters','Contains waiver services',
                 'Encounters by month','Encounters by Month'),
    caption = 'Service coverage for individuals on Autism Waiver waitlist, by Enrollee',
    extensions = c('Responsive','Buttons'),
    options = list(
      columnDefs = cd,
      fnDrawCallback = cb,
      dom = 't',
      buttons = c('colvis'))) %>%
formatStyle(
  'autism_waiver_svs',
  color = styleEqual(c(0,1), c('white','red')),
  backgroundColor = styleEqual(c(0,1), c('white','red'))
)
dt$dependencies <- append(dt$dependencies, htmlwidgets:::getDependency("sparkline"))
dt
