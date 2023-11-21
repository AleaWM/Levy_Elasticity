library(tidyverse)

#16,354 obs
raw_data_joined <- read_csv("agency_raw_joined.csv")

dropped_munis <- c("030250000", "030270000", "030300000", "030585000", "030840000", "030890000", "031150000")


exclude_hr_change <- c("030770000", "030800000","030880000", "031070000", "031190000", "031250000" )


recoded_data <- raw_data_joined %>% 
  
  filter(total_final_levy > 0) %>%  #### discuss 
  
  mutate(first2 = str_sub(agency_num, 1,2), #used to collapse agencies
         last2 = str_sub(agency_num,8,9), #ditto?
         year = as.factor(year),
         agency_num = str_pad(agency_num, 9, "left", "0"), #add missing leading zeros
         agency_num = as.factor(agency_num),
         home_rule_ind = as.factor(home_rule_ind), #change reference category
         minor_type = as.factor(minor_type)) %>%
  
  mutate(cty_total_eav = as.numeric(cty_total_eav),    # eav in cook and neighboring counties
         cty_cook_eav = as.numeric(cty_cook_eav),      # EAV after exemptions within cook county
         pct_in_Cook = cty_cook_eav / cty_total_eav,   # to identify taxing agencies that cross county lines
         total_final_levy = as.numeric(total_final_levy), 
         total_reduced_levy = as.numeric(total_reduced_levy) # for non-HR agencies that have their levy reduced
         ) 

recoded_data <- recoded_data %>% 
  
  filter(pct_in_Cook > 0.9) %>% #keep only agencies greater than 90% in Cook
  
  mutate(
    total_final_levy_4log = total_final_levy + 1
    #Add 1 to total_final_levy to allow ln transformation.
    #See question on line 14
    ) %>%
 
   mutate( 
    log_eav = log(cty_total_eav), # eav within Cook AND neighboring counties.
    log_totallevy = log(total_final_levy_4log)
  )


#two way fixed effects will be used for agency and year.

panel_data <-pdata.frame(recoded_data, index = c("agency_num", "year"))

#need to detach dplyr because conflict w/ plm lag command

detach("package:dplyr", unload = TRUE)

panel_data$lag_totallevy <- plm::lag(panel_data$total_final_levy, 1)
panel_data$lag_cty_total_eav <- plm::lag(panel_data$cty_total_eav, 1)

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
         totallevy_pct_change = (total_final_levy - lag_totallevy) /lag_totallevy)  %>%
  
  mutate(eav_pct_change = ifelse(is.na(eav_pct_change), 0, eav_pct_change),
         home_rule_ind = as.factor(home_rule_ind),
         minor_type = as.factor(minor_type))

detach("package:dplyr", unload = TRUE)

panel_data$lag_eav_pct_change1 <- plm::lag(panel_data$eav_pct_change, 1)
panel_data$lag_eav_pct_change2 <- plm::lag(panel_data$eav_pct_change, 2)
