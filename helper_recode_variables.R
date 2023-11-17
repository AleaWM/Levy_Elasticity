library(tidyverse)

#16,354 obs
raw_data_joined <- read_csv("agency_raw_joined.csv")

dropped_munis <- c("030250000", "030270000", "030300000", "030585000", "030840000", "030890000", "031150000")


exclude_hr_change <- c("030770000", "030800000","030880000", "031070000", "031190000", "031250000" )


recoded_data <- raw_data_joined %>% 
  
  filter(total_final_levy > 0) %>%
  
  mutate(first2 = str_sub(agency_num, 1,2),
         last2 = str_sub(agency_num,8,9),
         year = as.factor(year),
         agency_num = str_pad(agency_num, 9, "left", "0"),
         agency_num = as.factor(agency_num),
         home_rule_ind = as.factor(home_rule_ind),
         minor_type = as.factor(minor_type)) %>%
  
  mutate(cty_total_eav = as.numeric(cty_total_eav),    # eav in cook and neighboring counties
         cty_cook_eav = as.numeric(cty_cook_eav),      # EAV after exemptions within cook county
         pct_in_Cook = cty_cook_eav / cty_total_eav,   # to identify taxing agencies that cross county lines
         total_final_levy = as.numeric(total_final_levy), 
         total_reduced_levy = as.numeric(total_reduced_levy) # for non-HR agencies that have their levy reduced
         ) 



recoded_data <- recoded_data %>% 
  
  filter(pct_in_Cook > 0.9) %>%
  
  # In order to use logged variables, then adding 1 to all values allows the log() to work
  # log(0)=Inf
  mutate(
    total_final_levy_4log = ifelse(total_final_levy <= 0, 1, total_final_levy),
    total_non_cap_ext_4log = ifelse(total_non_cap_ext<= 0 | is.na(total_non_cap_ext), 1, total_non_cap_ext), # added Oct. 18 2023
    total_capped_ext_4log = ifelse(home_rule_ind == 0, (total_final_levy_4log - total_non_cap_ext_4log), NA )
    ) %>%
 
   mutate( 
    log_eav = log(cty_total_eav), # eav within Cook AND neighboring counties.
    log_capped = log(total_capped_ext_4log),
    log_nocap = log(total_non_cap_ext_4log),
    log_totallevy = log(total_final_levy_4log)
  ) %>%

  # if not using logged variables, then this matters less  
  mutate(total_final_levy = ifelse(total_final_levy <=0, 0, total_final_levy),
         total_non_cap_ext = ifelse(total_non_cap_ext<= 0 | is.na(total_non_cap_ext), 0, total_non_cap_ext),   
         total_capped_ext = (total_final_levy - total_non_cap_ext) 
  )
         




panel_data <-pdata.frame(recoded_data, index = c("agency_num", "year"))


detach("package:dplyr", unload = TRUE)

panel_data$lag_capped <- plm::lag(panel_data$total_capped_ext, 1)
panel_data$lag_totallevy <- plm::lag(panel_data$total_final_levy, 1)
panel_data$lag_nocap <- plm::lag(panel_data$total_non_cap_ext, 1)
panel_data$lag_cty_total_eav <- plm::lag(panel_data$cty_total_eav, 1)

panel_data$eav_lag1 <- plm::lag(panel_data$cty_total_eav, 1)
panel_data$eav_lag2 <- plm::lag(panel_data$cty_total_eav, 2)
panel_data$eav_lag3 <- plm::lag(panel_data$cty_total_eav, 3)
panel_data$eav_lag4 <- plm::lag(panel_data$cty_total_eav, 4)

panel_data$reassess_lag1 <- plm::lag(panel_data$reassess_year, 1)
panel_data$reassess_lag2 <- plm::lag(panel_data$reassess_year, 2)




library(dplyr)
panel_data<-panel_data %>% 
  mutate(capped_pct_change = ((total_capped_ext-lag_capped)/lag_capped),
         nocap_pct_change = ((total_non_cap_ext - lag_nocap) / lag_nocap),
         eav_pct_change = (cty_total_eav - lag_cty_total_eav)/ lag_cty_total_eav,
         totallevy_pct_change = (total_final_levy - lag_totallevy) /lag_totallevy)  %>%
  mutate(capped_pct_change = ifelse(is.na(capped_pct_change),0, capped_pct_change),
         nocap_pct_change = ifelse(is.na(nocap_pct_change),0, nocap_pct_change),
         eav_pct_change = ifelse(is.na(eav_pct_change), 0, eav_pct_change),
         home_rule_ind = as.factor(home_rule_ind),
         minor_type = as.factor(minor_type))

detach("package:dplyr", unload = TRUE)
panel_data$lag_eav_pct_change1 <- plm::lag(panel_data$eav_pct_change, 1)
panel_data$lag_eav_pct_change2 <- plm::lag(panel_data$eav_pct_change, 2)
