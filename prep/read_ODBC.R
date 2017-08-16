
# install.packages("dbplyr")
library(DBI); library(odbc)

con <- 
  DBI::dbConnect(
    odbc::odbc(),
    Driver   = "SQL Server",
    Server   = "10.10.10.13",
    Database = "DATAMIX",
    UID      = rstudioapi::askForPassword("Database user"),
    PWD      = rstudioapi::askForPassword("Database password"),
    Port     = 1433
  )

tables <- as_tibble(dbListTables(con))

df <-
dbGetQuery(
  con, 
  "SELECT TOP 5000 *
   FROM [DATAMIX].[dbo].[EDICLMPF]"
)

claims_db <- tbl(con, "EDICLMPF")

# query <- claims_db %>% ...

query %>% show_query()

# To pull all the data down into a local tibble:

df <- query %>% collect()
