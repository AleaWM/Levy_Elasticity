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


