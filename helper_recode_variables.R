library(tidyverse)
library(dplyr)
library(plm)

#16,354 obs
raw_data_joined <- read_csv("agency_raw_joined.csv")

# Tables from PTAXSIM
cpi <- read_csv("./Necessary_Files/cpi.csv") # has two year variables!!
eq_factor <- read_csv("./Necessary_Files/eq_factor.csv") %>% 
  select(-eq_factor_tentative)




dropped_munis <- c("030250000", "030270000", "030300000", "030585000", "030840000", "030890000", "031150000")


exclude_hr_change <- c("030770000", "030800000","030880000", "031070000", "031190000", "031250000" )


recoded_data <- raw_data_joined %>% 
  left_join(cpi, by = c("year" = "levy_year")) %>%
  left_join(eq_factor) %>%
  mutate(
    # year = as.factor(year) 
    agency_num = as.character(agency_num),
    
    agency_num = str_pad(agency_num, 9, "left", "0"), #add missing leading zeros
    first6 = str_pad(first6, 6, "left", "0"),
    home_rule_ind = as.character(home_rule_ind), #change reference category
    minor_type = as.factor(minor_type),
    reassess_year = as.character(reassess_year)) %>%
  
  mutate(cty_total_eav = as.numeric(cty_total_eav),    # taxable eav in cook and neighboring counties
         cty_cook_eav = as.numeric(cty_cook_eav),      # taxable EAV in cook county only
         pct_in_Cook = cty_cook_eav / cty_total_eav,   # to identify taxing agencies that cross county lines
         total_final_levy = as.numeric(total_final_levy), 
         total_non_cap_ext = as.numeric(total_non_cap_ext),
         total_capped_ext = ifelse(home_rule_ind ==0, (total_ext - total_non_cap_ext), NA),
         total_non_cap_ext = ifelse(home_rule_ind == 1, total_ext, total_non_cap_ext),
         total_reduced_levy = as.numeric(total_reduced_levy), # for non-HR agencies that have their levy reduced
         av = cty_cook_eav / eq_factor_final,          # backed out taxable Assessed Value of properties
         first6_w_hr = str_c(first6, "_", home_rule_ind),
         agency_w_hr = str_c(agency_num, "_", home_rule_ind)
         ) %>%
  group_by(year, cty_total_eav, home_rule_ind) %>%
  mutate(summed_levy_sharetaxbase = sum(total_final_levy)+1) %>% # grouped by tax base and year
  ungroup() %>%
  group_by(year, first6, home_rule_ind) %>%
  mutate(summed_levy_first6 =sum(total_final_levy)+1) %>% # grouped by first 6 digits in agency number
  ungroup()


recoded_data <- recoded_data %>% 
  
  filter(pct_in_Cook > 0.9) %>% #keep only agencies greater than 90% in Cook
  
  mutate(
    total_final_levy_4log = total_final_levy + 1,
    #Add 1 to total_final_levy to allow ln transformation.
    
    log_eav = log(cty_total_eav), # eav within Cook AND neighboring counties.
    log_levy = log(total_final_levy_4log),
    log_capped = log(total_capped_ext),
    log_noncapped = log(total_non_cap_ext),
    log_av = log(av),
    
    year = as.factor(year), # for plm()
    agency_num = as.factor(agency_num) # for plm()
    
  ) %>% 
  filter(minor_type != "SSA") # drops 5332 taxing agencies (agency-year combos)

## 7921 observations remain after removing SSAs ##
# 8391 with 2022

drop_me <- recoded_data %>% 
  group_by(agency_name, agency_num) %>%
  mutate(has_levy = ifelse(total_final_levy > 0, 1, 0)) %>%
  summarize(levies = sum(total_final_levy),
            has_levies = sum(has_levy)) %>%
  filter(levies == 0 | has_levies < 5)

drop_me

recoded_data <- recoded_data %>% 
  filter(!agency_num %in% drop_me$agency_num) %>% 
  arrange(agency_num, year)
# 7715 obs after removing
# 8178 with 2022

# turn it into panel data!
# two way fixed effects will be used for agency and year.
panel_data <-pdata.frame(recoded_data, index = c("agency_num", "year"))


# need to detach dplyr because conflict w/ plm lag command
detach("package:dplyr", unload = TRUE)

panel_data$lag_totallevy <- plm::lag(panel_data$total_final_levy, 1)
panel_data$lag_capped <- plm::lag(panel_data$total_capped_ext, 1)
panel_data$lag_non_capped <- plm::lag(panel_data$total_non_cap_ext, 1)

panel_data$lag_cty_total_eav <- plm::lag(panel_data$cty_total_eav, 1)
panel_data$lag_av <- plm::lag(panel_data$av, 1)


panel_data$eav_lag1 <- plm::lag(panel_data$cty_total_eav, 1)
panel_data$eav_lag2 <- plm::lag(panel_data$cty_total_eav, 2)
panel_data$eav_lag3 <- plm::lag(panel_data$cty_total_eav, 3)
panel_data$eav_lag4 <- plm::lag(panel_data$cty_total_eav, 4)

panel_data$reassess_lag1 <- plm::lag(panel_data$reassess_year, 1)
panel_data$reassess_lag2 <- plm::lag(panel_data$reassess_year, 2)

#boss said leave redundant code.


library(dplyr)

panel_data <- panel_data %>% 
  
  mutate(eav_pct_change = (cty_total_eav - lag_cty_total_eav)/ lag_cty_total_eav,
         tfl_pct_change = (total_final_levy - lag_totallevy) / lag_totallevy,
         capped_pct_change = (total_capped_ext - lag_capped) / lag_capped,
         noncapped_pct_change = (total_non_cap_ext - lag_non_capped) / lag_non_capped,
         av_pct_change = (av - lag_av) / lag_av,
         av_increase = ifelse(av_pct_change > 0, "AV increased", 
                              ifelse(av_pct_change == 0, "No Change", "AV Increase")),
         up_down  = ifelse(eav_pct_change > 0, "increased", "decreased"))  %>%
  
  mutate(# eav_pct_change = ifelse(is.na(eav_pct_change), 0, eav_pct_change),
         home_rule_ind = as.factor(home_rule_ind),
         minor_type = as.factor(minor_type),
         major_type = as.factor(major_type),
         first6 = as.factor(first6))

detach("package:dplyr", unload = TRUE)

panel_data$lag_eav_pct_change1 <- plm::lag(panel_data$eav_pct_change, 1)
panel_data$lag_eav_pct_change2 <- plm::lag(panel_data$eav_pct_change, 2)

panel_data$lag_av_pct_change1 <- plm::lag(panel_data$av_pct_change, 1)
panel_data$lag_av_pct_change2 <- plm::lag(panel_data$av_pct_change, 2)



library(dplyr)
schools_panel <- panel_data %>% filter(major_type == "SCHOOL") %>%  
  mutate(lag_totallevy = as.numeric(lag_totallevy))

governments_panel <- panel_data %>% 
  mutate(lag_totallevy = as.numeric(lag_totallevy)) %>% 
  filter( major_type != "COOK COUNTY" & major_type != "SCHOOL") 
#revised 11/26 to filter "COOK COUNTY"

table(governments_panel$minor_type)


governments_panel <- as.data.frame(governments_panel)
schools_panel <- as.data.frame(schools_panel)
all_agencies <- as.data.frame(panel_data)

#write.csv(all_agencies, "panel_data.csv")
