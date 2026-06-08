fund_data <- read_csv("agency_funds.csv")
agency_withcapped <- read_csv("panel_data_forcapped.csv")


## Capped

all_ols_1 <- feols(capped_pct_change ~ eav_pct_change,
  data = agency_withcapped, cluster = ~agency_num)

summary(all_ols_1)

all_ols_2 <- feols(
  capped_pct_change ~ eav_pct_change | year^home_rule_ind,
  data = agency_withcapped, cluster = ~agency_num)

all_ols_3 <- feols(capped_pct_change ~ eav_pct_change | year^home_rule_ind + agency_num,
  data = agency_withcapped, cluster = ~agency_num)

all_ols_4 <- feols(capped_pct_change ~ eav_pct_change * factor(home_rule_ind) | year + agency_num,
  data = agency_withcapped, cluster = ~agency_num)


all_ols_models_v22 <- list(M1 = all_ols_1, M2 = all_ols_2, M3 = all_ols_3, M4 = all_ols_4)

etable(all_ols_models_v22)


main_df |> group_by(unique_id, home_rule_ind) |> distinct()

fund_data |> filter(capped_ind == 1) |> group_by(year) |> summarize(not_capped = sum(levy, na.rm = TRUE))


agency_withcapped |>
  # filter(capped_ind == 1) |>
  group_by(year) |>
  summarize(
    # in millions
    Total_Ext = sum(total_ext, na.rm = TRUE) / 1000000,
    Capped_Ext = sum(total_capped_ext, na.rm = TRUE) / 1000000,
    Uncapped_Ext = sum(total_non_cap_ext, na.rm = TRUE) / 1000000)


fund_data |> 
  filter(max_levy != 0 & final_levy > max_levy)


fund_data |>
  # filter(capped_ind == 1) |>
  group_by(year, capped_ind) |>
  summarize(
    # in millions
    Total_Levy = sum(levy, na.rm = TRUE) / 1000000,
    Total_Final_Levy = sum(final_levy, na.rm = TRUE) / 1000000,
    Capped_Ext = sum(total_capped_ext, na.rm = TRUE) / 1000000,
    Uncapped_Ext = sum(total_non_cap_ext, na.rm = TRUE) / 1000000)
