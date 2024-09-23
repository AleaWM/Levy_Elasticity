library(tidyverse)
library(DBI)
library(ptaxsim)
library(glue)
library(dplyr)

## Change database file path to match your computer's location 
## of the PTAXSIM database!
ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), 
#  "C:/Users/aleaw/OneDrive/Documents/PhD Fall 2021 - Spring 2022/Merriman RA/ptax/ptaxsim.db/ptaxsim-2022.0.0.db")
"./ptaxsim.db/ptaxsim-2022.0.0.db")

muni_agency_names <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  "SELECT DISTINCT agency_num, agency_name, minor_type
  FROM agency_info
  WHERE minor_type = 'MUNI'
  OR agency_num = '020060000'
  "
  ) %>% 
  mutate(first6 = str_sub(agency_num,1,6)) %>%
  select(-minor_type)


agency_dt <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  "SELECT *
  FROM agency
  "
) %>%
  mutate(first6 = str_sub(agency_num,1,6)) %>%
  mutate(year = as.character(year))


is.integer64 <- function(x){
  class(x)=="integer64"
}

agency_dt <- agency_dt %>%
  mutate_if(is.integer64, as.integer)


# has binary variable for if it was a reassessment year or not. 
# Manually created based on the 3 year rotation used for reassessments.
reassessment_years <- read_csv("./Necessary_Files/Triad_reassessment_years.csv")


reassessments_long <- reassessment_years %>% 
  pivot_longer(cols = c(`2006`:`2022`), names_to = "year", values_to = "reassess_year")


nicknames <- readxl::read_xlsx("./Necessary_Files/muni_shortnames.xlsx") %>% 
  select(-c(agency_number, short_name, `Column1`, `Most recent reassessed`))



all_taxing_agencies <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  "SELECT agency_num, agency_name, major_type, minor_type
  FROM agency_info
  "
  ) %>%
  mutate(first6 = str_sub(agency_num,1,6)  )


muni_agency_names <- muni_agency_names %>% 
  left_join(nicknames)

all_taxing_agencies <- all_taxing_agencies %>%
  left_join(muni_agency_names, by = c("first6")) %>%
  rename(muni_name =  agency_name.y,
         muni_num = agency_num.y,
         agency_name = agency_name.x,
         agency_num = agency_num.x)

raw_data_joined <- left_join(agency_dt, all_taxing_agencies, by = c("agency_num", "first6"))

raw_data_joined <- left_join(raw_data_joined, reassessments_long, by = c("Triad", "year")) %>%

  select(-c(cty_dupage_eav:cty_livingston_eav, clean_name_alt, shpfile_name, ORIGOID, township_code))   # drop extra columns
  
# for Stata and Excel, have NAs be blank:
#agency_dt %>% write_csv("agency_raw_joined.csv", na = "")


raw_data_joined %>% write_csv("agency_raw_joined.csv")

