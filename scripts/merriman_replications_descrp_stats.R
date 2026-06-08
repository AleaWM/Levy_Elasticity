### Checking Stata-R Comparison from Merriman's 9/18 Email ###
### 9/18/2024 ###
### MVH ###

library(tidyverse)
library(fixest)

df <- read_csv("model_data_Sept102024.csv")

## From Stata by way of Merriman

# DF has 5372 obs
# 416 2010 obs. and 415 2011 obs. dropped due lagged av values
# Drops 16 obs. subsequently dropped due to missing values
# Total: 4525 for model's N

# Fixest starts w/ 5372
# Drops 868 obs. due to NA values (RHS: 868, FEs: 13)


# Variable: DM Coeffs/SE | MVH/AWM Coeffs/SE
# ln(av): 0.11978866/0.024116 | 0.199745/0.041573
# lag_1: | 0.137184/0.033913
# lag_2: | 0.134064/0.031168

# Merriman FEs: agency_num and year (but maybe a year dummy?)
#               418 groups, coefs on years 2013-2022

# Thought 1: agency_num is different from uniqueid

df |>
  select(year, agency_num) |>
  group_by(year) |>
  summarize(n = n())

### These values match DM's

### Let's try with uniqueid

df |>
  select(year, uniqueid) |>
  group_by(year) |>
  summarize(n = n())

### Same numbers!

# Thought 2: Does using agency_num instead of uniqueid FEs change the model output?

thought_2 <- fixest::feols(log(total_final_levy) ~ log(av) + l(log(av)) +
                             l(log(av), 2) | agency_num + year,
                           data = df,
                           vcov = "cluster",
                           panel.id = c("agency_num", "year")
)

summary(thought_2)

## Coefficients and N are now the same. Our within R2 is significantly smaller (.09
# vs. DM's )

# We got similar SEs to before, but twice the size of DM's SEs

# Let's modify our model's SEs!

thought_3 <- fixest::feols(log(total_final_levy) ~ log2(av) + l(log(av)) +
                             l(log(av), 2) | agency_num + year,
                           data = df,
                           vcov = "iid",
                           panel.id = c("agency_num", "year")
)

summary(thought_3)

### Slide 13 ####

df <- read_csv("panel_data.csv")   ## 10,374


df <- df |> filter(!minor_type %in% c("BOND", "UNIFIED", "COMM COLL",
                                      "COOK", "MISC", "SANITARY", "FIRE",
                                      "MOSQUITO", "WATER") ) # 9,051 obs
# MVH & AWM obs match


df <- df |>
   # filter(year > 2009)  |>   ## 6922 obs MVH: 6922

  # Change infinite values from tfl % delta to 0
  mutate(tfl_pct_change = if_else(is.na(tfl_pct_change) |
                                    is.infinite(tfl_pct_change),
                                  0, tfl_pct_change))


df <- df |>
  select(-c(#muni_name, muni_num,
            agency_w_hr, first6_w_hr,
            log_eav:reassess_lag2,
            lag_eav_pct_change1:lag_av_pct_change2)) %>%
  mutate_at(c("home_rule_ind"), as.character)  %>%
  # Code Cicero as a Municipality and not a township.
  mutate(minor_type == ifelse( agency_name %in% c("TOWN EVANSTON", "GENERAL ASSISTANCE EVANSTON",
                                                  "TOWN CICERO", "GENERAL ASSISTANCE CICERO"), "MUNI", minor_type)
         ) %>%

  # Rosemont and its school district have huge jump
  # in tax base and levy due to TIF expiring
  filter(!grepl('ROSEMONT|DISTRICT 78', agency_name)) #6883 obs. MVH & AWM match

df_pre_group <- df

df_pre_group |>
  filter(year == 2022) |>
 # MVH's total levy: 9464898504
  mutate(total_levy = sum(total_final_levy) ) %>%
  group_by(minor_type, total_levy) |>
  summarize(group_levy = sum(total_final_levy)) %>%
  mutate(group_pct = group_levy/total_levy)

groupies2 <- read_csv("grouped_labels.csv") %>%
  mutate(agency_num = str_pad(agency_num, 9, "left", "0"))

groupie_types <- groupies2 |>
  filter(agency_num %in% df_pre_group$agency_num)

df_group_types <- df_pre_group |>
  filter(agency_num %in% groupie_types$agency_num) |>
  select(agency_num, major_type, minor_type) |>
  distinct()

groupies2_w_types <- groupies2 |>
  left_join(df_group_types, by = "agency_num")

df_pg_types <- df_pre_group |>
  left_join(groupies2_w_types, by = "agency_num") |>
  rename(minor_type = minor_type.x, major_type = major_type.x, first6 = first6.x,
         agency_name = agency_name.x)

df_pg_types |>
  filter(year == 2022) |>
  group_by(minor_type) |>
  summarize(n = n(),
            sum(total_final_levy, na.rm = T))
# 531 total and 9464898504 total levy


### Let's look at what makes "munis" in abfm_grouped_panel next! ###
# df has 9000 obs.

munis <- df_pg_types %>%
  # filter schools - 6621 obs.
  filter(major_type != "SCHOOL") |>
  # filter townships - 4996 obs.
 filter(str_sub(agency_num, 1,2) != "02" & minor_type  != "TOWN") |>
  mutate(first6 = as.character(first6), # make character
    home_rule_ind = as.character(home_rule_ind),
    first6 = str_pad(first6, 6, "left", "0"), # add leading zeros
    lim_rate = ifelse(is.na(lim_rate), "NA", lim_rate)) |>

   filter(major_type == "MUNICIPALITY/TOWNSHIP" | minor_type %in% c("LIBRARY", "PARK")) %>%
  # filters in lines 141 and 143 render this filter wrong.

  group_by(year, grouped_label) %>%
  arrange(agency_num) %>%

  # will use home rule status in 2006 for munis.
  mutate(muni_hri = first(home_rule_ind)) |>
 # ungroup() |>
  # Filter out NHR agencies previously grouped with HR Munis
  # 4996 obs.
  filter(home_rule_ind == muni_hri) |>
#  group_by(year, grouped_label) |>
  # 2381 obs.
  summarize(#home_rule_ind = mean(as.numeric(home_rule_ind), na.rm=TRUE),
            agency_name = first(agency_name),
            types = paste(list(unique(minor_type)), sep = ", "),

            agency_num = first(agency_num),
            total_final_levy = sum(total_final_levy, na.rm=TRUE),
            min_group_eav = min(cty_cook_eav),
            max_group_eav = max(cty_cook_eav),
            cty_cook_eav = first(cty_cook_eav),
            cty_total_eav = first(cty_total_eav),
            av = first(av), ## mistake fixed: was sum, now is first()
            Triad = first(Triad),
            agency_count = n(),  # Agencies in Grouped
            reassess_year = first(reassess_year),
            home_rule_ind = first(home_rule_ind),
            rate = sum(total_final_rate, na.rm=TRUE),
            connected_count = n(),
            #lim_rate = mean(as.numeric(lim_rate), na.rm=TRUE),
            lim_rate = first(lim_rate),
            clean_name = first(clean_name),
            min_group_eav = min(cty_cook_eav),
            max_group_eav = max(cty_cook_eav)
  ) %>%
  mutate(
    dif_group_eav = max_group_eav - min_group_eav,
    conn_agency_flag = ifelse(connected_count > 1, 1, 0),
    log_eav = log(cty_total_eav),
    log_levy = log(total_final_levy),
    log_av =  log(av),
    bundled = ifelse(agency_count > 1, 1, 0),
    uniqueid = str_c(grouped_label, "_", home_rule_ind, "_", bundled),
    first2_dig = str_sub(agency_num, 1,2),
    town = ifelse(first2_dig == "03", "Muni",
                  ifelse(first2_dig == "02", "Township", "Other")),
    town = ifelse( agency_name %in% c("TOWN EVANSTON", "GENERAL ASSISTANCE EVANSTON",
                                    "TOWN CICERO", "GENERAL ASSISTANCE CICERO"), "Muni", town),) %>%
  # Filter zero-levy agencies. Should do nothing.
  filter(total_final_levy > 0) %>%
  # Filter out "non-munis" (e.g., unbundled park districts)
  # 1899 obs.
  filter(town == "Muni") |>
  select(year, uniqueid, everything()) %>% ungroup()

not_munis <- df_pg_types |>
#  filter(town != "Muni") |> #443 obs--makes sense!
  filter(!(agency_num%in% munis$agency_num)) |>
  filter(year == 2022) |>
  mutate(total_levy = sum(total_final_levy)) %>%
  group_by(minor_type, total_levy) |>
  summarize(n = n(),
            group_levy = sum(total_final_levy, na.rm = T)) %>%
  mutate(group_pct = group_levy / total_levy)

type_filter <- groupies2_w_types |>
  select(agency_num, major_type, minor_type) |>
  distinct()

not_munis <- not_munis |>
  left_join(type_filter, by = "agency_num")

not_munis |>
  filter(year == 2022) |>
  summarize(sum(total_final_levy, na.rm = T))

not_munis |>
  filter(year == 2022) |>
  group_by(minor_type) |>
  summarize(n = n(), sum(total_final_levy, na.rm = T), sum(total_final_levy, na.rm = T)/9464898504)

not_munis |>
  filter(year == 2022) |>
  filter(is.na(minor_type))

61240993 + 5906204

101591928/9464898504

munis |>
  filter(year == 2022) |>
  summarize(n = n(), sum(total_final_levy, na.rm = T))

##### SCHOOLS #####

schools <- df_pg_types %>%
  # filter schools - 6621 obs.
  filter(major_type == "SCHOOL") |>
 # filter(str_sub(agency_num, 1,2) == "02") |>
  mutate(first6 = as.character(first6), # make character
         home_rule_ind = as.character(home_rule_ind),
         first6 = str_pad(first6, 6, "left", "0"), # add leading zeros
         lim_rate = ifelse(is.na(lim_rate), "NA", lim_rate)) |>

  # filter(major_type == "MUNICIPALITY/TOWNSHIP" | minor_type %in% c("LIBRARY", "PARK")) %>%
  # filters in lines 141 and 143 render this filter wrong.

  group_by(year, agency_name) %>%
 # mutate(home_rule_ind = first(home_rule_ind)) |>
 # ungroup() |>
  # Filter out NHR agencies previously grouped with HR Munis
  # 4996 obs.
  #filter(home_rule_ind == muni_hri) |>
  #group_by(year, grouped_label) |>
  # 2381 obs.
  summarize(
    home_rule_ind = mean(as.numeric(home_rule_ind), na.rm=TRUE),
    agency_name = first(agency_name),
   types = paste(list(unique(minor_type)), sep = ", "),

    agency_num = first(agency_num),
    total_final_levy = sum(total_final_levy, na.rm=TRUE),
    min_group_eav = min(cty_cook_eav),
    max_group_eav = max(cty_cook_eav),
    cty_cook_eav = first(cty_cook_eav),
    cty_total_eav = first(cty_total_eav),
    av = first(av), ## mistake fixed: was sum, now is first()
    Triad = first(Triad),
    agency_count = n(),  # Agencies in Grouped
    reassess_year = first(reassess_year),
    home_rule_ind = first(home_rule_ind),
    rate = sum(total_final_rate, na.rm=TRUE),
    connected_count = n(),
    lim_rate = first(lim_rate),
    clean_name = first(clean_name),
    min_group_eav = min(cty_cook_eav),
    max_group_eav = max(cty_cook_eav)
  ) %>%
  mutate(
    dif_group_eav = max_group_eav - min_group_eav,
    conn_agency_flag = ifelse(connected_count > 1, 1, 0),
    log_eav = log(cty_total_eav),
    log_levy = log(total_final_levy),
    log_av =  log(av),
    bundled = ifelse(agency_count > 1, 1, 0),
    uniqueid = str_c(agency_name, "_", home_rule_ind, "_", bundled),
    first2_dig = str_sub(agency_num, 1,2),
    #
   town = "School",
  ) %>%
  select(year, uniqueid, everything())

schools |>
  filter(year == 2022) |>
  summarize(n = n())

df |>
  filter(year == 2022) |>
  filter(minor_type %in% c("ELEMENTARY", "SECONDARY")) |>
  summarize(n = n(), sum(av, na.rm = T), sum(total_final_levy, na.rm = T))

munis |>
  filter(year == 2022) |>
  summarize(sum(av, na.rm = T), sum(total_final_levy, na.rm = T))

df_pre_group |>
  filter(year == 2022) |>
  filter(str_sub(agency_num, 1,2) == "02") |>
  summarize(n = n(), sum(av, na.rm = T), sum(total_final_levy, na.rm = T))

townships <- df %>%
  filter(!agency_num %in% schools$agency_num) %>%
  mutate(agency_num = str_pad(agency_num, 9, "left", "0"),
         first6 = as.character(first6), # make character
         home_rule_ind = as.character(home_rule_ind),
         first6 = str_pad(first6, 6, "left", "0"), # add leading zeros
         lim_rate = ifelse(is.na(lim_rate), "NA", lim_rate)) |>

  left_join(groupies2,
            by = c("agency_name", "agency_num")) %>%
  filter(!grouped_label %in% munis$grouped_label)

  filter(major_type == "MUNICIPALITY/TOWNSHIP"  | major_type == "MISCELLANEOUS") %>%
  arrange(agency_num) %>%

  group_by(year, grouped_label) %>%
  summarize(agency_name = first(agency_name),
            types = paste(list(unique(minor_type)), sep = ", "),
            agency_num = first(agency_num),
            total_final_levy = sum(total_final_levy, na.rm=TRUE),
            min_group_eav = min(cty_cook_eav),
            max_group_eav = max(cty_cook_eav),
            cty_cook_eav = first(cty_cook_eav),
            cty_total_eav = first(cty_total_eav),
            av = first(av), ## mistake fixed: was sum, now is first()
            Triad = first(Triad),
            agency_count = n(),  # Agencies in Grouped
            reassess_year = first(reassess_year),
            home_rule_ind = first(home_rule_ind),
            rate = sum(total_final_rate, na.rm=TRUE),
            connected_count = n(),
            lim_rate = mean(as.numeric(lim_rate), na.rm=TRUE),
            clean_name = first(clean_name),
            min_group_eav = min(cty_cook_eav),
            max_group_eav = max(cty_cook_eav)
  ) %>%
  mutate(
    dif_group_eav = max_group_eav - min_group_eav,
    conn_agency_flag = ifelse(connected_count > 1, 1, 0),
    log_eav = log(cty_total_eav),
    log_levy = log(total_final_levy),
    log_av =  log(av),
    bundled = ifelse(agency_count > 1, 1, 0),
    uniqueid = str_c(grouped_label, "_", home_rule_ind, "_", bundled),
    first2_dig = str_sub(agency_num, 1,2),
    town =  ifelse(first2_dig == "02", "Township", NA)
    #       ifelse(first2_dig == "03", "Muni", "Other")),
    # town = ifelse( agency_name == "TOWN CICERO", "Muni", town)
  ) %>%
  select(year, uniqueid, everything()) %>%
  ungroup() %>%
  mutate(town = ifelse(grepl("Cicero|Evanston", uniqueid), "Muni", town)) %>%
  filter(town == "Township")

townships |>
  filter(year == 2022) |>
  summarize(av = sum(av, na.rm = TRUE),
            levy = sum(total_final_levy, na.rm = TRUE)) %>%
  mutate(levy_pct = levy / 9464898504)


### EMAIL RE: SUMMED AV #####

df_2 <- read_csv("model_data_Sept242024.csv")

df_2 |>
  filter(year == 2022) |>
  group_by(town) |>
  summarize(n = n(), sum(av, na.rm = T), sum(total_final_levy, na.rm = T))


### replication take 2 ######

df <- read_csv("panel_data.csv")   ## 10,374


df <- df |> filter(!minor_type %in% c("BOND", "UNIFIED", "COMM COLL",
                                      "COOK", "MISC", "SANITARY", "FIRE",
                                      "MOSQUITO", "WATER") ) # 9,051 obs

groupies2 <- read_csv("grouped_labels.csv") %>%
  mutate(agency_num = str_pad(agency_num, 9, "left", "0"))


schools <- df %>% # 9051 obs.

  ## HEY CAN WE APPLY THIS MUTATE TO DF STRAIGHT UP?
  mutate(agency_num = str_pad(agency_num, 9, "left", "0"),
         first6 = as.character(first6), # make character
         home_rule_ind = as.character(home_rule_ind),
         first6 = str_pad(first6, 6, "left", "0"), # add leading zeros
         lim_rate = ifelse(is.na(lim_rate), "NA", lim_rate)) |>

  filter(major_type == "SCHOOL") %>% # 2396 obs.
  mutate(grouped_label = agency_name) %>%
  group_by(year, grouped_label, home_rule_ind) %>%
  summarize(agency_name = first(agency_name),
            types = paste(list(unique(minor_type)), sep = ", "),

            agency_num = first(agency_num),
            total_final_levy = sum(total_final_levy, na.rm=TRUE),
            min_group_eav = min(cty_cook_eav),
            max_group_eav = max(cty_cook_eav),
            cty_total_eav = first(cty_total_eav),
            cty_cook_eav = first(cty_cook_eav),
            av = first(av),
            Triad = first(Triad),
            agency_count = n(),
            reassess_year = first(reassess_year),
            home_rule_ind = first(home_rule_ind),
            rate = sum(total_final_rate, na.rm=TRUE),
            connected_count = n(),
            lim_rate = sum(as.numeric(lim_rate), na.rm=TRUE),
            clean_name = first(clean_name),
  ) %>%
  mutate(
    dif_group_eav = max_group_eav - min_group_eav,
    conn_agency_flag = ifelse(connected_count > 1, 1, 0),
    log_eav = log(cty_total_eav),
    log_levy = log(total_final_levy),
    log_av =  log(av),
    bundled = ifelse(agency_count > 1, 1, 0),
    uniqueid = str_c(grouped_label, "_", home_rule_ind, "_", bundled),
    first2_dig = str_sub(agency_num, 1,2),
    town = "School") %>%
  #filter(total_final_levy > 1) %>% ## Has no effect
  select(year, uniqueid, everything()) %>% ungroup()


townships <- df %>%
  filter(!agency_num %in% schools$agency_num) %>%
  # Here's that mutate again! =)
  mutate(agency_num = str_pad(agency_num, 9, "left", "0"),
         first6 = as.character(first6), # make character
         home_rule_ind = as.character(home_rule_ind),
         first6 = str_pad(first6, 6, "left", "0"), # add leading zeros
         lim_rate = ifelse(is.na(lim_rate), "NA", lim_rate)) |>

  left_join(groupies2,
            by = c("agency_name", "agency_num")) %>%
  # We should think about this filter some more.
  filter(major_type == "MUNICIPALITY/TOWNSHIP"  | major_type == "MISCELLANEOUS") %>%
  arrange(agency_num) %>%

  group_by(year, grouped_label) %>%
  summarize(agency_name = first(agency_name),
            types = paste(list(unique(minor_type)), sep = ", "),
            agency_num = first(agency_num),
            total_final_levy = sum(total_final_levy, na.rm=TRUE),
            min_group_eav = min(cty_cook_eav),
            max_group_eav = max(cty_cook_eav),
            cty_cook_eav = first(cty_cook_eav),
            cty_total_eav = first(cty_total_eav),
            av = first(av), ## mistake fixed: was sum, now is first()
            Triad = first(Triad),
            agency_count = n(),  # Agencies in Grouped
            reassess_year = first(reassess_year),
            home_rule_ind = first(home_rule_ind),
            rate = sum(total_final_rate, na.rm=TRUE),
            connected_count = n(),
            lim_rate = mean(as.numeric(lim_rate), na.rm=TRUE),
            clean_name = first(clean_name),
            min_group_eav = min(cty_cook_eav),
            max_group_eav = max(cty_cook_eav)
  ) %>%
  mutate(
    dif_group_eav = max_group_eav - min_group_eav,
    conn_agency_flag = ifelse(connected_count > 1, 1, 0),
    log_eav = log(cty_total_eav),
    log_levy = log(total_final_levy),
    log_av =  log(av),
    bundled = ifelse(agency_count > 1, 1, 0),
    uniqueid = str_c(grouped_label, "_", home_rule_ind, "_", bundled),
    first2_dig = str_sub(agency_num, 1,2),
    town =  ifelse(first2_dig == "02", "Township", NA)
    #       ifelse(first2_dig == "03", "Muni", "Other")),
    # town = ifelse( agency_name == "TOWN CICERO", "Muni", town)
  ) %>%
  select(year, uniqueid, everything()) %>%
  ungroup() %>%
  mutate(town = ifelse(grepl("Cicero|Evanston", uniqueid), "Muni", town)) %>%
  filter(town == "Township")

munis <- df %>%
  filter(!agency_num %in% schools$agency_num) %>%
  filter(!agency_num %in% townships$agency_num) %>%

  filter(! agency_name %in% c("ALSIP MERRIONETTE PARK PUBLIC LIBRARY DISTRICT", "CENTRAL STICKNEY PARK DISTRICT", 	"CITY OF COUNTRYSIDE", "BARRINGTON PUBLIC LIBRARY DISTRICT", "EISENHOWER PUBLIC LIBRARY DISTRICT", "GLENWOOD LYNWOOD PUBLIC LIBRARY DISTRICT",
                              "GOLF MAINE PARK DISTRICT", "GRANDE PRAIRIE PUBLIC LIBRARY DISTRICT", "GREEN HILLS PUBLIC LIBRARY DISTRICT",
                              "HOMEWOOD FLOSSMOOR PARK DISTRICT","LAN OAK PARK DISTRICT", "NANCY L MCCONATHY PUBLIC LIBRARY DISTRICT",
                              "SALT CREEK RURAL PARK DISTRICT","STICKNEY FOREST VIEW PUBLIC LIBRARY DISTRICT", "RIVER TRAILS PARK DISTRICT",
                              "VETERANS PARK DISTRICT", "WINNETKA - NORTHFIELD PUBLIC LIBRARY DISTRICT",
                              "WESTDALE PARK DISTRICT")) %>%


  mutate(#agency_num = str_pad(agency_num, 9, "left", "0"),
    first6 = as.character(first6), # make characte
    home_rule_ind = as.character(home_rule_ind),
    first6 = str_pad(first6, 6, "left", "0"), # add leading zeros
    lim_rate = ifelse(is.na(lim_rate), "NA", lim_rate)) |>

  left_join(groupies2, by = c("agency_name", "agency_num")) %>%

  filter(!grouped_label %in% townships$grouped_label) %>%
  filter(grouped_label != "drop") %>%
  filter(!grouped_label %in% c("Deer Park", "Westdale", "Homer Glen", "Norridge")) %>%
 # We should return to this filter.
  filter(major_type == "MUNICIPALITY/TOWNSHIP"  | minor_type %in% c("LIBRARY", "PARK")
  ) %>%


  group_by(year, grouped_label) %>%
  arrange(agency_num) %>%

  mutate(muni_homeruleind = first(home_rule_ind)) %>%
  filter(home_rule_ind == muni_homeruleind) %>%

  summarize(
    agency_name = first(agency_name),
    types = paste(list(unique(minor_type)), sep = ", "),

    agency_num = first(agency_num),
    total_final_levy = sum(total_final_levy, na.rm=TRUE),
    min_group_eav = min(cty_cook_eav),
    max_group_eav = max(cty_cook_eav),
    cty_cook_eav = first(cty_cook_eav),
    cty_total_eav = first(cty_total_eav),
    av = first(av), ## mistake fixed: was sum, now is first()
    Triad = first(Triad),
    agency_count = n(),  # Agencies in Grouped
    reassess_year = first(reassess_year),
    home_rule_ind = first(home_rule_ind),
    rate = sum(total_final_rate, na.rm=TRUE),
    connected_count = n(),
    lim_rate = mean(as.numeric(lim_rate), na.rm=TRUE),
    clean_name = first(clean_name),
    min_group_eav = min(cty_cook_eav),
    max_group_eav = max(cty_cook_eav)
  ) %>%
  mutate(
    dif_group_eav = max_group_eav - min_group_eav,
    conn_agency_flag = ifelse(connected_count > 1, 1, 0),
    log_eav = log(cty_total_eav),
    log_levy = log(total_final_levy),
    log_av =  log(av),
    bundled = ifelse(agency_count > 1, 1, 0),
    uniqueid = str_c(grouped_label, "_", home_rule_ind, "_", bundled),
    first2_dig = str_sub(agency_num, 1,2),
    town = "Muni"
    #ifelse(first2_dig == "03", "Muni",
    #       ifelse(first2_dig == "02", "Township", "Other")),
    #  town = ifelse( agency_name == c("TOWN EVANSTON", "GENERAL ASSISTANCE EVANSTON",                                  "TOWN CICERO", "GENERAL ASSISTANCE CICERO"), "Muni", NA),
  ) %>%
  select(year, uniqueid, everything())



rejects <- df %>%
  filter(!agency_num %in% schools$agency_num) %>%
  filter(!agency_num %in% townships$agency_num) %>%
  filter(!agency_num %in% munis$agency_num) %>%

  mutate(agency_num = str_pad(agency_num, 9, "left", "0"),
         first6 = as.character(first6), # make character
         home_rule_ind = as.character(home_rule_ind),
         first6 = str_pad(first6, 6, "left", "0"), # add leading zeros
         lim_rate = ifelse(is.na(lim_rate), "NA", lim_rate)) |>

  left_join(groupies2,
            by = c("agency_name", "agency_num")) %>%
  arrange(agency_num) %>%
  filter(!grouped_label %in% townships$grouped_label) %>%
  group_by(year, grouped_label, home_rule_ind) %>%
  summarize(agency_name = first(agency_name),
            types = paste(list(unique(minor_type)), sep = ", "),
            agency_num = first(agency_num),
            total_final_levy = sum(total_final_levy, na.rm=TRUE),
            min_group_eav = min(cty_cook_eav),
            max_group_eav = max(cty_cook_eav),
            cty_cook_eav = first(cty_cook_eav),
            cty_total_eav = first(cty_total_eav),
            av = first(av), ## mistake fixed: was sum, now is first()
            Triad = first(Triad),
            agency_count = n(),  # Agencies in Grouped
            reassess_year = first(reassess_year),
            home_rule_ind = first(home_rule_ind),
            rate = sum(total_final_rate, na.rm=TRUE),
            connected_count = n(),
            lim_rate = mean(as.numeric(lim_rate), na.rm=TRUE),
            clean_name = first(clean_name),
            min_group_eav = min(cty_cook_eav),
            max_group_eav = max(cty_cook_eav)
  ) %>%
  mutate(
    dif_group_eav = max_group_eav - min_group_eav,
    conn_agency_flag = ifelse(connected_count > 1, 1, 0),
    log_eav = log(cty_total_eav),
    log_levy = log(total_final_levy),
    log_av =  log(av),
    bundled = ifelse(agency_count > 1, 1, 0),
    uniqueid = str_c(grouped_label, "_", home_rule_ind, "_", bundled),
    first2_dig = str_sub(agency_num, 1,2),
    town = "Other") %>%
  select(year, uniqueid, everything()) %>% ungroup()

rejects %>% ungroup() %>%
  filter(year == 2022) %>%
  left_join(groupies2_w_types, by = "agency_num") %>%
#  group_by(minor_type) %>%
  summarize(n=n(),
            levy = sum(total_final_levy),
            av = sum(av)) %>%
  mutate(pct_levy = levy/9464898504)

rejects %>% left_join()filter(year == 2022) %>%
  summarize(n=n(),
            levy = sum(total_final_levy),
            av = sum(av)) %>%
  mutate(pct_levy = levy/9464898504)

munis %>% filter(year==2022) %>%
  summarize(n=n(),
            levy = sum(total_final_levy),
            av = sum(av)) %>%
  mutate(pct_levy = levy/9464898504)

townships %>% filter(year == 2022) %>%
  summarize(n=n(),
            levy = sum(total_final_levy),
            av = sum(av)) %>%
  mutate(pct_levy = levy/9464898504)

schools %>% filter(year == 2022) %>%
  summarize(n=n(),
            levy = sum(total_final_levy),
            av = sum(av)) %>%
  mutate(pct_levy = levy/9464898504)

