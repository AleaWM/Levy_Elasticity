## Creates agency_reassessment.csv
## Which is just the agency_dt from ptaxsim with the triad and 
## reassessment year indicator added to it.


library(DBI)
library(glue)
library(jsonlite)
library(httr)
library(ptaxsim)
library(tidyverse)

ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), 
                                  #"./ptaxsim.db/ptaxsim-2022.0.0.db"
                                  "C:/Users/aleaw/OneDrive/Documents/PhD Fall 2021 - Spring 2022/Merriman RA/ptax/ptaxsim.db/ptaxsim-2022.0.0.db"
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
    `$select` = paste0(c("distinct pin", "triad_name"
                         #, "township_code", "township_name",
                        # "nbhd_code", "lat","lon"
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

agency_triads <- taxing_agencies %>% distinct(year, agency_num, agency_name,  
                                              triad_name, reassess_year, 
                                              agency_minor_type, agency_major_type
                                              )

#agency_triads %>% filter(is.na(triad_name)) %>% distinct(agency_name)
#agency_triads %>% write_csv("agency_reassessmentyears.csv")

agency_triads <- read_csv("agency_reassessmentyears.csv") %>% 
  select(year, agency_num, 
         agency_name,  
           triad_name, reassess_year, 
         #  agency_minor_type, agency_major_type
  )


agency_triads <- agency_triads %>%
  mutate(triad_name = case_when(
    triad_name == "SCHOOL DISTRICT CC 59" ~	"North",
    triad_name == "ARLINGTON HEIGHTS TOWNSHIP HIGH SCHOOL 214" ~	"North",
    triad_name == "SCHOOL DISTRICT 83" ~	"South",
    triad_name == "COMMUNITY HIGH SCHOOL 212"	~ "South",
    triad_name == "SCHOOL DISTRICT 104"	~ "South",
    triad_name == "COMMUNITY HIGH SCHOOL 217"	~ "South",
    triad_name == "SCHOOL DISTRICT 72" ~ "North",
    triad_name == "COMMUNITY HIGH SCHOOL 219"	~ "North",
    triad_name == "SCHOOL DISTRICT 70" ~	"North",
    triad_name == "SCHOOL DISTRICT 31" ~	"North",
    triad_name == "NORTHFIELD TOWNSHIP HIGH SCHOOL 225"	~ "North",
    triad_name == "SCHOOL DISTRICT CC 15"	~ "North",
    triad_name == "PALATINE TOWNSHIP HIGH SCHOOL 211"	~ "North",
    triad_name == "SCHOOL DISTRICT 152 1/2" ~	"South",
    triad_name == "THORNTON TOWNSHIP HIGH SCHOOL 205" ~	"South",
    triad_name == "SCHOOL DISTRICT 152"	~ "South",
    triad_name == "SCHOOL DISTRICT 21 WHEELING COMMUNITY CONSOLIDATED"	~ "South",
    triad_name == "SCHOOL DISTRICT 130"	~ "South",
    triad_name == "COMMUNITY HIGH SCHOOL 218"	~ "South",
    triad_name == "SCHOOL DISTRICT 144" ~	"South",
    triad_name == "COMMUNITY HIGH SCHOOL 228" ~	"South",
    triad_name == "SCHOOL DISTRICT 103" ~	"North",
    triad_name == "BERWYN CICERO STICKNEY HIGH SCHOOL 201" ~ "South",
    triad_name == "SCHOOL DISTRICT 162" ~	"South",
    triad_name == "RICH TOWNSHIP HIGH SCHOOL 227" ~	"South",
    triad_name == "BLOOM TOWNSHIP HIGH SCHOOL 206"~	"South",
    triad_name == "SCHOOL DISTRICT CC 54" ~	"North",
    triad_name == "SCHOOL DISTRICT 111" ~	"South",
    triad_name == "COMMUNITY HIGH SCHOOL 220" ~	"North",
    TRUE ~ triad_name)
  ) %>% 
  group_by(year, agency_num, agency_name) %>% arrange(triad_name) %>%
  summarize(triad_name = first(triad_name),
            reassess_year = first(reassess_year)) %>% ungroup() 

agency_triads %>% write_csv("agency_reassessmentyears.csv")


df <- read_csv("model_data_Sept102024.csv") %>%   
  ungroup() %>%
  select(-c(Triad, reassess_year)) %>%
  left_join(agency_triads, by = c("year", "agency_name"= "agency_name"))

  
df %>% write_csv("model_data_Sept212024.csv")
