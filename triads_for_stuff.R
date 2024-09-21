library(tidyverse)


agency_triads <- read_csv("Assessor_-_Parcel_Universe_20240917.csv") |>
  filter(tax_year == 2022) |>
  select(-tax_year)

agency_triads_pivot <- agency_triads |>
  mutate(across(ends_with("_num"),
                # Get rid of brackets
                ~ str_replace_all(.x, "\\[(\\d+)\\]", "\\1") |>
                # Replace empty strings with NA
                  str_replace_all("\\[\\]", NA_character_))) |>
  # Pivot!
  pivot_longer(names_from = ends_with("_num"), values_from = "triad_name")

write_csv(agency_triads_pivot, "triads_pivot.csv")
  
library(dplyr)
library(tidyr)

# Pivot the data from wide to long format
agency_triads_pivot <- agency_triads %>%
  tidyr::pivot_longer(

    cols = dplyr::ends_with("_n"),                # Select all columns that end with "_n"
    names_to = "taxing_agency",                 # Name for the new column that will hold the names of the original columns
    values_to = "triad",                   # Name for the new column that will hold the values
    values_drop_na = TRUE                  # Drop rows where the value is NA
  )


##### API Attempt ############

library(DBI)
library(glue)
library(jsonlite)
library(httr)
library(ptaxsim)
library(tidyverse)

ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2022.0.0.db"
                                  #C:/Users/aleaw/OneDrive/Documents/PhD Fall 2021 - Spring 2022/Merriman RA/ptax/ptaxsim.db/ptaxsim-2022.0.0.db"
                                  )


alldistinct_pins <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT DISTINCT pin, tax_code_num
  FROM pin",
    .con = ptaxsim_db_conn
  )) #2475053 obs.


base_url <- "https://datacatalog.cookcountyil.gov/resource/tx2p-k2g9.json"

puniverse <- GET(
  base_url,
  query = list(
    tax_year = 2022,
    `$select` = paste0(c("distinct pin", "triad_name"#,# "township_code", "township_name",
                        # "nbhd_code",
                       #    "lat","lon"
                         ),
                       
    collapse = ","),
    `$limit` = 20000000L
  )
)

puniverse <- fromJSON(rawToChar(puniverse$content))


joined <-  dplyr::left_join(alldistinct_pins, puniverse, by = "pin")

triads_intaxcodes <- joined %>% 
  arrange(tax_code_num, triad_name) %>%
  group_by(tax_code_num) %>% 
  summarize(triad_name = first(triad_name))

taxing_agencies <- lookup_agency(2006:2022, triads_intaxcodes$tax_code_num) 

taxing_agencies <- left_join(taxing_agencies, triads_intaxcodes, by = c("tax_code"="tax_code_num"))


# has binary variable for if it was a reassessment year or not. 
# Manually created based on the 3 year rotation used for reassessments.
reassessment_years <- read_csv("./Necessary_Files/Triad_reassessment_years.csv")

reassessments_long <- reassessment_years %>% 
  pivot_longer(cols = c(`2006`:`2022`), names_to = "year", values_to = "reassess_year") %>% 
  mutate(year = as.numeric(year))

taxing_agencies <- left_join(taxing_agencies, reassessments_long, by = c("year", "triad_name" = "Triad"))

agency_triads <- taxing_agencies %>% distinct(year, agency_num, agency_name,  triad_name, reassess_year, agency_minor_type, agency_major_type)

#agency_triads %>% write_csv("agency_reassessmentyears.csv")

supp <- read_csv("needs_triads - needs_triads.csv")

df <- read_csv("model_data_Sept102024.csv")

# is this not a command? triad_supp <- col_bind