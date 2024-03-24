### Helper File to Assemble Data for Levy Elasticity Paper ###
### MVH & AWM ###
### last updated 3/9/24 ###

## GOAL (3/12): GENERATE PANEL DATA FRAME ##

# Load Necessary Packages -------------------------------------------------

## Test if we actually need the "here" or "data.table" packages?

library(tidyverse)
library(DBI)
library(ptaxsim)

# Instantiate PTAXSIM DB Connection ---------------------------------------

## I don't see a better way to maintain relative paths without...creating another
## copy of the PTAXSIM db...

ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2022.0.0.db")

# Query PTAXSIM DB -------------------------------------------------

## Query agency_info table

### Pull municipality names, agency numbers, and agency minor types from PTAXSIM's
### "agency_info" table.
### Cicero is not categorized as a municipality but rather a township. 
### Thus, it is brought into the data frame through its agency number.

## MVH notes: 
## Do we need to pull the minor_type variable from PTAXSIM since
## we filter other minor_types out in the same command and then remove the
## variable in line 39?

## Maybe we move all the mutates to one section of the file and only put the
## DB queries in this section?

## 134 obs.

muni_agency_names <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  "SELECT DISTINCT agency_num, agency_name, minor_type 
  FROM agency_info
  WHERE minor_type = 'MUNI'
  OR agency_num = '020060000'
  "
) %>% 
  mutate(first6 = str_sub(agency_num,1,6)) %>% #move me!
  select(-minor_type)

## Query agency table minor_type

## Pull all data on all taxing agencies from PTAXSIM's DB's agency table

## MVH note: Why wouldn't we just query the DB for minor_type = muni and
## Cicero? Same as above re: mutate.

# 17294 obs.

agency_dt <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  "SELECT *
  FROM agency
  "
) %>%
  mutate(first6 = str_sub(agency_num,1,6)) %>%
  mutate(year = as.character(year))

## I think this is redundant with (but more limited than) the code on
## line 41 (Probably for a merge later)

all_taxing_agencies <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  "SELECT agency_num, agency_name, major_type, minor_type
  FROM agency_info
  "
) %>%
  mutate(first6 = str_sub(agency_num,1,6))

# Import other data -------------------------------------------------

## MVH note: can we put these all in an xlsx together?

# has binary variable for if it was a reassessment year or not. 
# Manually created based on the 3 year rotation used for reassessments.

reassessment_years <- read_csv("./Necessary_Files/Triad_reassessment_years.csv")

# Read in "cleaned" names of municipalities (e.g., "Cicero" 
# not "TOWNSHIP OF CICERO")

nicknames <- readxl::read_xlsx("./Necessary_Files/muni_shortnames.xlsx") %>% 
  select(-c(agency_number, short_name, `Column1`, `Most recent reassessed`))

# Prepare data for merge -------------------------------------------------

## MVH Note: Check if we still need this function.

is.integer64 <- function(x){
  class(x)=="integer64"
}

agency_dt <- agency_dt %>%
  mutate_if(is.integer64, as.integer)

## pivot_longer to allow incorpoation of reassesment year into broader agency df

reassessments_long <- reassessment_years %>% 
  pivot_longer(cols = c(`2006`:`2022`), names_to = "year", values_to = "reassess_year")

# Merge data -------------------------------------------------

muni_agency_names <- muni_agency_names %>% 
  left_join(nicknames)

all_taxing_agencies <- all_taxing_agencies %>%
  left_join(muni_agency_names, by = c("first6")) %>% #can't we use an 
  ##optional join argument to get rid of these lines of code?
  rename(muni_name =  agency_name.y,
         muni_num = agency_num.y,
         agency_name = agency_name.x,
         agency_num = agency_num.x)

## Merge agency levy/tax base data with agency names

raw_data_joined <- left_join(agency_dt, all_taxing_agencies, by = c("agency_num", "first6"))

raw_data_joined <- left_join(raw_data_joined, reassessments_long, by = c("Triad", "year")) %>%
  ## Surely Jahun put this random hard return here.
  select(-c(cty_dupage_eav:cty_livingston_eav, clean_name_alt, shpfile_name, ORIGOID, township_code))   # drop extra columns

# Finalize data for use in model -------------------------------------------------