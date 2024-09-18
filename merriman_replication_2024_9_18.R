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

thought_3 <- fixest::feols(log(total_final_levy) ~ log(av) + l(log(av)) +
                             l(log(av), 2) | agency_num + year,
                           data = df,
                           vcov = "iid",
                           panel.id = c("agency_num", "year")
)

summary(thought_3)
