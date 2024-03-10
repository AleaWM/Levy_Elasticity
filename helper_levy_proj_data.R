### Helper File to Assemble Data for Levy Elasticity Paper ###
### MVH & AWM ###
### last updated 3/9/24 ###

# Load Necessary Packages -------------------------------------------------

## Test if we actually need the "here" or "data.table" packages?

library(tidyverse)
library(DBI)
library(here)
library(data.table)
library(ptaxsim)

# Instantiate PTAXSIM DB Connection ---------------------------------------

## I don't see a better way to maintain relative paths without...creating another
## copy of the PTAXSIM db...

ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2022.0.0.db")


