# Setup ----------------------------------------------------------------------
# descriptive_stats_15_lags_cluster.R
# Rough R translation of: descriptive stats_15_lags_cluster.do
# Converted from Stata to R on 2026-03-06.
#
# Notes:
# - This keeps the overall logic and model structure, because apparently
#   suffering should at least be reproducible.
# - Update the directory paths below before running.
# - Stata's esttab output is translated to modelsummary output. You can
#   swap to gt / flextable / tinytable if you want prettier tables.
# - Some table formatting is approximate rather than byte-for-byte identical.

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
  library(fixest)
  library(modelsummary)
  library(broom)
  library(glue)
  library(readr)
})

options(scipen = 999)

# ================================================================
# 0. PATHS
# ================================================================

# location <- "box"  # "home", "office", or "box"
#
# paths <- list(
#   home = list(
#     data_main   = "C:/Users/dmerrim/OneDrive - University of Illinois at Chicago/igpa/fiscal futures budget project/2023_2024/elasticity of levy/data from Micheal",
#     data_census = "C:/Users/dmerrim/OneDrive - University of Illinois Chicago/igpa/fiscal futures budget project/2023_2024/elasticity of levy/data from census",
#     willamette  = "C:/Users/dmerrim/OneDrive - University of Illinois Chicago/igpa/fiscal futures budget project/2023_2024/elasticity of levy/willamete university data",
#     post_nta    = "C:/Users/dmerrim/OneDrive - University of Illinois Chicago/igpa/fiscal futures budget project/2023_2024/elasticity of levy/post nta",
#     output      = "C:/Users/dmerrim/OneDrive - University of Illinois Chicago/igpa/fiscal futures budget project/2023_2024/elasticity of levy/post nta/output"
#   ),
#   office = list(
#     data_main   = "C:/Users/dmerrim/OneDrive - University of Illinois Chicago/igpa/fiscal futures budget project/2023_2024/elasticity of levy/data from Micheal",
#     data_census = "C:/Users/dmerrim/OneDrive - University of Illinois Chicago/igpa/fiscal futures budget project/2023_2024/elasticity of levy/data from census",
#     willamette  = "C:/Users/dmerrim/OneDrive - University of Illinois Chicago/igpa/fiscal futures budget project/2023_2024/elasticity of levy/willamete university data",
#     post_nta    = "C:/Users/dmerrim/OneDrive - University of Illinois Chicago/igpa/fiscal futures budget project/2023_2024/elasticity of levy/post nta",
#     output      = "C:/Users/dmerrim/OneDrive - University of Illinois Chicago/igpa/fiscal futures budget project/2023_2024/elasticity of levy/post nta/output"
#   ),
#   box = list(
#     data_main   = "C:/Users/dmerrim/Box/Super Cool RA Tree Fort & Club House/LevyEAV Elasticity/elasticity of levy/data from Micheal",
#     data_census = "C:/Users/dmerrim/Box/Super Cool RA Tree Fort & Club House/LevyEAV Elasticity/elasticity of levy/data from census",
#     willamette  = "C:/Users/dmerrim/Box/Super Cool RA Tree Fort & Club House/LevyEAV Elasticity/elasticity of levy/willamete university data",
#     post_nta    = "C:/Users/dmerrim/Box/Super Cool RA Tree Fort & Club House/LevyEAV Elasticity/elasticity of levy/post nta",
#     output      = "C:/Users/dmerrim/Box/Super Cool RA Tree Fort & Club House/LevyEAV Elasticity/elasticity of levy/post nta/output"
#   )
# )
#
# p <- paths[[location]]

# ================================================================
# 1. READ + MERGE DATA
# ================================================================

message(Sys.Date())
message(format(Sys.time(), "%H:%M:%S"))



# file DM used:  #"NTA_data_2024_10_14.csv"
# email shows that he had qustions about this file and then michael sent one for 10_16
main_df <- read_csv("NTA_data_2024_10_14.csv")


# File MVH sent him:
# main_df <- read_csv("NTA_data_2024_10_16.csv") |>
#   arrange(type)


# Most recent file AWM had on her computer:
# main_df <- read_csv("NTA_data_2024_11_08.csv") |>
#   arrange(type)


agency_lookup <- read_dta("fips_all_agency_name.dta")

main_df <- main_df |>
  left_join(agency_lookup, by = "agency_group")

# Stata listed unmatched rows and dropped merge==1 rows.
# In dplyr terms: drop observations from master that did not match.
main_df <- main_df |>
  filter(!is.na(fipsid))

census_df <- read_dta("census_data.dta")

main_df <- main_df |>
  left_join(census_df, by = c("fipsid", "year"))

# ================================================================
# 2. MERGE EQUALIZATION FACTORS + CLEAN NUMERIC FIELDS
# ================================================================

# Stata used a .dta called eq_factor after previously creating it from CSV.
# Adjust extension if your stored file is actually .csv.

eq_factor_path_dta <- file.path("eq_factor.dta")
eq_factor_path_csv <- file.path("Necessary_Files/eq_factor.csv")

eq_factor <- if (file.exists(eq_factor_path_dta)) {
  read_dta(eq_factor_path_dta)
} else if (file.exists(eq_factor_path_csv)) {
  read_csv(eq_factor_path_csv, show_col_types = FALSE)
} else {
  stop("Could not find eq_factor.dta or eq_factor.csv in post nta folder.")
}

main_df <- main_df |>
  left_join(eq_factor, by = "year")

string_num_vars <- c("assess_year_av", "av_true", "rate_smooth", "total_final_levy")

main_df <- main_df |>
  mutate(
    across(
      all_of(string_num_vars),
      ~ readr::parse_number(as.character(.x), na = c("NA", "", ".")),
      .names = "n_{.col}"
    )
  )

# ================================================================
# 3. PANEL SETUP + CONSTRUCTED VARIABLES
# ================================================================

main_df <- main_df |>
  mutate(
    n_agency_group = as.integer(factor(agency_group)),
    n_uniqueid     = as.integer(factor(uniqueid))
  ) |>
  arrange(n_agency_group, year) |>
  filter(!is.na(n_agency_group)) |>
  group_by(n_agency_group) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    lag_av = lag(av, 1),
    lag2_av = lag(av, 2),
    lag3_av = lag(av, 3),
    lead1_av = lead(av, 1),
    lead2_av = lead(av, 2),
    lead3_av = lead(av, 3),
    lag_eq_factor_final = lag(eq_factor_final, 1),
    lag2_eq_factor_final = lag(eq_factor_final, 2),
    lag3_eq_factor_final = lag(eq_factor_final, 3),
    lag_reassess_year = lag(reassess_year, 1),
    lag2_reassess_year = lag(reassess_year, 2)
  ) |>
  ungroup() |>
  filter(year >= 2008)

main_df <- main_df |>
  mutate(
    t = case_when(
      reassess_year == 1 ~ 1,
      lag_reassess_year == 1 ~ 2,
      lag2_reassess_year == 1 ~ 3,
      TRUE ~ 0
    ),
    r = case_when(
      t == 1 ~ ((lead3_av / av)^(1 / 3)) - 1,
      t == 2 ~ ((lead2_av / lag_av)^(1 / 3)) - 1,
      t == 3 ~ ((lead1_av / lag2_av)^(1 / 3)) - 1,
      TRUE ~ 0
    ),
    EstV = case_when(
      t == 1 ~ av,
      t == 2 ~ lag_av * (1 + r),
      t == 3 ~ lag2_av * (1 + r)^2,
      TRUE ~ 0
    ),
    ln_EstV = log(EstV),
    d_av = 100 * log(av / lag_av)) |>
  group_by(n_agency_group) |>
  mutate(
    d_eav = 100 * log((av * eq_factor_final) / (lag_av * lag(eq_factor_final))),
    d_levy = 100 * log(total_final_levy / lag(total_final_levy)),
    d_total_ig_revenue = 100 * log(total_ig_revenue / lag(total_ig_revenue)),
    d_enrollment = 100 * log(enrollment / lag(enrollment)),
    has_ig_data = if_else(is.na(d_total_ig_revenue), 0, 1),
    type_2 = case_when(
      type == "Muni" & home_rule_ind == 1 ~ "HR_muni",

      ## Added this row below!!
      type == "Muni" & home_rule_ind == 0 ~ "NonHR_muni",
      TRUE ~ as.character(type)
    )
  ) |> ungroup()

# ================================================================
# 4. HELPERS
# ================================================================

vcov_uid <- ~n_uniqueid

tidy_plus <- function(model, extra = NULL) {
  out <- broom::tidy(model)
  if (!is.null(extra)) {
    attr(out, "extra") <- extra
  }
  out
}

safe_wald_p <- function(model, hypothesis) {
  out <- tryCatch(fixest::wald(model, hypothesis), error = function(e) NULL)
  if (is.null(out)) return(NA_real_)
  out$p
}

safe_lincom <- function(model, combo) {
  out <- tryCatch(fixest::lincom(model, combo), error = function(e) NULL)
  if (is.null(out)) {
    return(tibble(estimate = NA_real_, ci_low = NA_real_, ci_high = NA_real_, p.value = NA_real_))
  }
  tibble(
    estimate = out$estimate,
    ci_low   = out$ci_low,
    ci_high  = out$ci_high,
    p.value  = out$p.value
  )
}

save_table <- function(models, file_stub, title = NULL, notes = NULL, coef_map = NULL, gof_omit = "IC|Log|Adj|RMSE|Std.Errors") {
  out_file <- file.path(paste0("merriman_file_output/", file_stub, ".html"))
  modelsummary(
    models,
    output = out_file,
    title = title,
    notes = notes,
    coef_map = coef_map,
    stars = c("*" = .10, "**" = .05, "***" = .01),
    gof_omit = gof_omit
  )
}
# Appendix A1 ---------------------------------------------------------------
# ================================================================
# 5. INSTRUMENT VALIDITY CHECKS
# ================================================================

instrument_df <- main_df |>
  filter(year > 2008)

table(instrument_df$year) # 2009 through 2023

feols(d_eav ~ reassess_year,
  data = instrument_df,
  cluster = vcov_uid,
  fsplit = ~type)

feols(d_eav ~ reassess_year,
  data = instrument_df |> filter(type == "Muni"),
  cluster = vcov_uid,
  fsplit = ~home_rule_ind)


models <- list(
  "all_instrument"      = feols(d_eav ~ reassess_year,
    data = instrument_df, cluster = vcov_uid),
  "muni_instrument"     = feols(d_eav ~ reassess_year,
    data = filter(instrument_df, type == "Muni"),
    cluster = vcov_uid),
  "other_instrument"    = feols(d_eav ~ reassess_year,
    data = filter(instrument_df, type == "Other"),
    cluster = vcov_uid),
  "school_instrument"   = feols(d_eav ~ reassess_year,
    data = filter(instrument_df, type == "School"),
    cluster = vcov_uid),
  "township_instrument" = feols(d_eav ~ reassess_year,
    data = filter(instrument_df, type == "Township"),
    cluster = vcov_uid)
)

save_table(
  list(
    "All" = all_instrument,
    "Muni" = muni_instrument,
    "Other" = other_instrument,
    "School" = school_instrument,
    "Township" = township_instrument
  ),
  file_stub = "instrument_validity_checks",
  title = "Table X: Predict assessments by reassessment year by type",
  notes = "Cook County, Illinois data from 2008 through 2023"
)

# ================================================================
# 6. BASE SAMPLE FOR OLS / IV
# ================================================================

reg_df <- main_df |>
  filter(year > 2008)


# Matches Table 2 -----------------------------------------------------------

# ================================================================
# 7. ALL-AGENCY OLS
# ================================================================


all_ols_1 <- feols(d_levy ~ d_eav, data = reg_df, cluster = vcov_uid)
all_ols_2 <- feols(d_levy ~ d_eav | year, data = reg_df, cluster = vcov_uid)
all_ols_3 <- feols(d_levy ~ d_eav + d_total_ig_revenue | year, data = reg_df, cluster = vcov_uid)

used_in_reg <- model.frame(all_ols_3) |> rownames() |> as.integer()
reg_df$used_in_reg <- FALSE
reg_df$used_in_reg[used_in_reg] <- TRUE

all_ols_4 <- feols(
  d_levy ~ d_eav + d_total_ig_revenue | year + n_uniqueid,
  data = reg_df,
  # data = filter(reg_df, used_in_reg),
  cluster = vcov_uid
)

save_table(
  list(M1 = all_ols_1, M2 = all_ols_2, M3 = all_ols_3, M4 = all_ols_4
  ),
  file_stub = "OLS_all_agencies",
  title = "Table X: OLS Predict levy using d_eav",
  notes = c(
    "Cook County, Illinois data from 2008 through 2023.",
    "Columns 2, 3, and 4 include year fixed effects; column 4 includes unit fixed effects."
  )
)

table(reg_df$year)

# Matches Table 3 ----------------------------------------------------------
# ================================================================
# 8. ALL-AGENCY IV
# ================================================================

all_iv_1 <- feols(d_levy ~ 1  | d_eav ~ reassess_year, data = reg_df, cluster = vcov_uid)
all_iv_2 <- feols(d_levy ~ 1 | year | d_eav ~ reassess_year, data = reg_df, cluster = vcov_uid)
all_iv_3 <- feols(d_levy ~ d_total_ig_revenue | year | d_eav ~ reassess_year, data = reg_df, cluster = vcov_uid)

used_in_reg2 <- model.frame(all_iv_3) |> rownames() |> as.integer()
reg_df$used_in_reg2 <- FALSE
reg_df$used_in_reg2[used_in_reg2] <- TRUE

all_iv_4 <- feols(
  d_levy ~ d_total_ig_revenue | year + agency_group | d_eav ~ reassess_year,
  # data = (reg_df |> filter(used_in_reg2 == TRUE)) ,
  data = reg_df,
  cluster = vcov_uid
)

save_table(
  list(M1 = all_iv_1, M2 = all_iv_2, M3 = all_iv_3, M4 = all_iv_4
  ),
  file_stub = "IV_all_agencies",
  title = "Table X: IV Predict levy using d_eav",
  notes = c(
    "Cook County, Illinois data from 2008 through 2023.",
    "Columns 2, 3, and 4 include year fixed effects; column 4 includes unit fixed effects.",
    "d_eav is treated as endogenous and instrumented by reassessment year."
  )
)

# Table 4 ------------------------------------------------------------------
# ================================================================
# 9. BY GOVERNMENT TYPE: OLS + IV
# ================================================================

models <- list(
  m1 <- feols(d_levy ~ d_eav, data = reg_df,
    cluster = vcov_uid, fsplit = ~type_2),
  m2 <- feols(d_levy ~ d_eav | year, data = reg_df,
    cluster = vcov_uid, fsplit = ~type_2),
  m3 <- feols(d_levy ~ d_eav + d_total_ig_revenue | year,
    data = reg_df, cluster = vcov_uid, fsplit = ~type_2),
  m4 <- feols(d_levy ~ d_eav + d_total_ig_revenue | year + n_uniqueid,
    data = reg_df, cluster = vcov_uid)
)

gov_types <- c("NonHR_muni", "HR_muni", "Other", "School", "Township")

run_type_models <- function(df, gov, iv = FALSE) {
  d <- df |> filter(type_2 == gov)

  if (!iv) {
    m1 <- feols(d_levy ~ d_eav, data = d,
      cluster = vcov_uid)
    m2 <- feols(d_levy ~ d_eav | year, data = d,
      cluster = vcov_uid)
    m3 <- feols(d_levy ~ d_eav + d_total_ig_revenue | year,
      data = d, cluster = vcov_uid)

    # idx <- model.frame(m3) |> rownames() |> as.integer()
    # d$used <- FALSE
    # d$used[idx] <- TRUE

    m4 <- feols(d_levy ~ d_eav + d_total_ig_revenue | year + n_uniqueid,
      # data = filter(d, used),
      data = d,
      cluster = vcov_uid)
  } else {
    m1 <- feols(d_levy ~ 1 | 0 | d_eav ~ reassess_year, data = d, cluster = vcov_uid)
    m2 <- feols(d_levy ~ 1 | year | d_eav ~ reassess_year, data = d, cluster = vcov_uid)
    m3 <- feols(d_levy ~ d_total_ig_revenue | year | d_eav ~ reassess_year, data = d, cluster = vcov_uid)

    # idx <- model.frame(m3) |> rownames() |> as.integer()
    # d$used <- FALSE
    # d$used[idx] <- TRUE

    m4 <- feols(d_levy ~ d_total_ig_revenue | year + n_uniqueid | d_eav ~ reassess_year,
      # data = filter(d, used),
      data = d,

      cluster = vcov_uid)
  }

  list(M1 = m1, M2 = m2, M3 = m3, M4 = m4)
}

ols_by_type <- purrr::map(gov_types, ~ run_type_models(reg_df, .x, iv = FALSE))
names(ols_by_type) <- gov_types

iv_by_type <- purrr::map(gov_types, ~ run_type_models(reg_df, .x, iv = TRUE))
names(iv_by_type) <- gov_types

# Optional: save one combined file per spec.
walk(1:4, function(i) {
  save_table(
    purrr::map(ols_by_type, ~ .x[[i]]),
    file_stub = glue("ols_regressions_M{i}"),
    title = "OLS Regressions by Government Type"
  )

  save_table(
    purrr::map(iv_by_type, ~ .x[[i]]),
    file_stub = glue("iv_regressions_M{i}"),
    title = "IV Regressions by Government Type"
  )
})

# ================================================================
# 10. SCHOOL SUBTYPE MODELS
# ================================================================

minor_types <- c("ELEMENTARY", "SECONDARY")
school_df <- reg_df |>
  filter(type == "School", minor_type %in% minor_types)

run_school_models <- function(df, subtype, iv = FALSE) {
  d <- df |> filter(minor_type == subtype)

  if (!iv) {
    m1 <- feols(d_levy ~ d_eav, data = d, cluster = vcov_uid)
    m2 <- feols(d_levy ~ d_eav | year, data = d, cluster = vcov_uid)
    m3 <- feols(d_levy ~ d_eav + d_enrollment | year, data = d, cluster = vcov_uid)
    m4 <- feols(d_levy ~ d_eav + d_total_ig_revenue | year, data = d, cluster = vcov_uid)

    # idx <- model.frame(m4) |> rownames() |> as.integer()
    # d$used <- FALSE
    # d$used[idx] <- TRUE

    m5 <- feols(d_levy ~ d_eav + d_total_ig_revenue | year + n_uniqueid,
      # data = filter(d, used),
      data = d,

      cluster = vcov_uid)
  } else {
    m1 <- feols(d_levy ~ 1 |  d_eav ~ reassess_year, data = d, cluster = vcov_uid)
    m2 <- feols(d_levy ~ 1 | year | d_eav ~ reassess_year, data = d, cluster = vcov_uid)
    m3 <- feols(d_levy ~ d_enrollment | year | d_eav ~ reassess_year, data = d, cluster = vcov_uid)
    m4 <- feols(d_levy ~ d_total_ig_revenue | year | d_eav ~ reassess_year, data = d, cluster = vcov_uid)

    # idx <- model.frame(m4) |> rownames() |> as.integer()
    # d$used <- FALSE
    # d$used[idx] <- TRUE

    m5 <- feols(d_levy ~ d_total_ig_revenue | year + n_uniqueid | d_eav ~ reassess_year,
      # data = filter(d, used),
      data = d,
      cluster = vcov_uid)
  }

  list(M1 = m1, M2 = m2, M3 = m3, M4 = m4, M5 = m5)
}

school_ols <- purrr::map(minor_types, ~ run_school_models(school_df, .x, iv = FALSE))
names(school_ols) <- minor_types

school_iv <- purrr::map(minor_types, ~ run_school_models(school_df, .x, iv = TRUE))
names(school_iv) <- minor_types

walk(1:5, function(i) {
  save_table(
    purrr::map(school_ols, ~ .x[[i]]),
    file_stub = glue("school_ols_regressions_M{i}"),
    title = "OLS School Regressions by Type"
  )

  save_table(
    purrr::map(school_iv, ~ .x[[i]]),
    file_stub = glue("school_iv_regressions_M{i}"),
    title = "IV School Regressions by Type"
  )
})

# Table 5 & 6 --------------------------------------------------------------------
# ================================================================
# 11. ASYMMETRY VARIABLES
# ================================================================

reg_df <- reg_df |>
  mutate(
    eav_growth = if_else(d_eav > 0, 1, 0),
    pos_d_eav = d_eav * eav_growth,
    neg_d_eav = d_eav * (1 - eav_growth)
  )

# fitted value for interaction-style instrument setup
first_stage_asym <- feols(d_eav ~ reassess_year | year, data = reg_df)
reg_df$d_eav_hat <- fitted(first_stage_asym)
reg_df$pos_d_eav_hat <- reg_df$pos_d_eav * reg_df$d_eav_hat

# ================================================================
# 12. ALL-AGENCY ASYMMETRIC OLS + IV
# ================================================================

reg_A <- feols(d_levy ~ pos_d_eav + neg_d_eav, data = reg_df, cluster = vcov_uid)
reg_B <- feols(d_levy ~ pos_d_eav + neg_d_eav | year, data = reg_df, cluster = vcov_uid)
reg_C <- feols(d_levy ~ pos_d_eav + neg_d_eav + d_total_ig_revenue | year, data = reg_df, cluster = vcov_uid)
#
# idx_A <- model.frame(reg_C) |> rownames() |> as.integer()
# reg_df$used_asym <- FALSE
# reg_df$used_asym[idx_A] <- TRUE

reg_D <- feols(
  d_levy ~ pos_d_eav + neg_d_eav + d_total_ig_revenue | year + n_uniqueid,
  # data = filter(reg_df, used_asym),
  data = reg_df,

  cluster = vcov_uid
)

reg_E <- feols(d_levy ~ 1 | neg_d_eav + pos_d_eav ~ reassess_year + pos_d_eav_hat, data = reg_df, cluster = vcov_uid)
reg_F <- feols(d_levy ~ 1 | year | neg_d_eav + pos_d_eav ~ reassess_year + pos_d_eav_hat, data = reg_df, cluster = vcov_uid)
reg_G <- feols(d_levy ~ d_total_ig_revenue | year | neg_d_eav + pos_d_eav ~ reassess_year + pos_d_eav_hat, data = reg_df, cluster = vcov_uid)

# idx_B <- model.frame(reg_G) |> rownames() |> as.integer()
# reg_df$used_asym_iv <- FALSE
# reg_df$used_asym_iv[idx_B] <- TRUE

reg_H <- feols(
  d_levy ~ d_total_ig_revenue | year + n_uniqueid | neg_d_eav + pos_d_eav ~ reassess_year + pos_d_eav_hat,
  # data = filter(reg_df, used_asym_iv),
  data = reg_df,
  cluster = vcov_uid
)

asym_ols_models <- list(M1 = reg_A, M2 = reg_B, M3 = reg_C, M4 = reg_D)
asym_iv_models  <- list(M1 = reg_E, M2 = reg_F, M3 = reg_G, M4 = reg_H)

save_table(asym_ols_models, "OLS_all_agencies_asym", title = "Table X: OLS Predict levy using d_eav")
save_table(asym_iv_models,  "IV_all_agencies_asym",  title = "Table X: IV Predict levy using d_eav")



# Table 7 ---------------------------------------------------------------------
# ================================================================
# 13. ASYMMETRIC MODELS BY GOVERNMENT TYPE
# ================================================================

run_asym_type_models <- function(df, gov, iv = FALSE) {
  d <- df |> filter(type_2 == gov)

  if (!iv) {
    m1 <- feols(d_levy ~ neg_d_eav + pos_d_eav, data = d, cluster = vcov_uid)
    m2 <- feols(d_levy ~ neg_d_eav + pos_d_eav | year, data = d, cluster = vcov_uid)
    m3 <- feols(d_levy ~ neg_d_eav + pos_d_eav + d_total_ig_revenue | year, data = d, cluster = vcov_uid)

    m4 <- feols(d_levy ~ neg_d_eav + pos_d_eav + d_total_ig_revenue | year + n_uniqueid,
      data = d,  cluster = vcov_uid)

  } else {
    m1 <- feols(d_levy ~ 1 | neg_d_eav + pos_d_eav ~ reassess_year + pos_d_eav_hat, data = d, cluster = vcov_uid)
    m2 <- feols(d_levy ~ 1 | year | neg_d_eav + pos_d_eav ~ reassess_year + pos_d_eav_hat, data = d, cluster = vcov_uid)
    m3 <- feols(d_levy ~ d_total_ig_revenue | year | neg_d_eav + pos_d_eav ~ reassess_year + pos_d_eav_hat, data = d, cluster = vcov_uid)
    m4 <- feols(d_levy ~ d_total_ig_revenue | year + n_uniqueid | neg_d_eav + pos_d_eav ~ reassess_year + pos_d_eav_hat,
      data = d, cluster = vcov_uid)
  }

  list(M1 = m1, M2 = m2, M3 = m3, M4 = m4)
}

asym_ols_by_type <- purrr::map(gov_types, ~ run_asym_type_models(reg_df, .x, iv = FALSE))
names(asym_ols_by_type) <- gov_types

asym_iv_by_type <- purrr::map(gov_types, ~ run_asym_type_models(reg_df, .x, iv = TRUE))
names(asym_iv_by_type) <- gov_types

walk(1:4, function(i) {
  save_table(
    purrr::map(asym_ols_by_type, ~ .x[[i]]),
    file_stub = glue("asym_ols_regressions_M{i}"),
    title = "OLS Regressions by Government Type"
  )

  save_table(
    purrr::map(asym_iv_by_type, ~ .x[[i]]),
    file_stub = glue("asym_iv_regressions_M{i}"),
    title = "IV Regressions by Government Type"
  )
})

# ================================================================
# 14. ASYMMETRIC SCHOOL SUBTYPE MODELS
# ================================================================

run_asym_school_models <- function(df, subtype, iv = FALSE) {
  d <- df |> filter(type == "School", minor_type == subtype)

  if (!iv) {
    m1 <- feols(d_levy ~ neg_d_eav + pos_d_eav, data = d, cluster = vcov_uid)
    m2 <- feols(d_levy ~ neg_d_eav + pos_d_eav | year, data = d, cluster = vcov_uid)
    m3 <- feols(d_levy ~ neg_d_eav + pos_d_eav + d_enrollment | year, data = d, cluster = vcov_uid)
    m4 <- feols(d_levy ~ neg_d_eav + pos_d_eav + d_total_ig_revenue | year, data = d, cluster = vcov_uid)

    m5 <- feols(d_levy ~ neg_d_eav + pos_d_eav + d_total_ig_revenue | year + n_uniqueid,
      data = d, cluster = vcov_uid)
  } else {
    m1 <- feols(d_levy ~ 1 | neg_d_eav + pos_d_eav ~ reassess_year + pos_d_eav_hat, data = d, cluster = vcov_uid)
    m2 <- feols(d_levy ~ 1 | year | neg_d_eav + pos_d_eav ~ reassess_year + pos_d_eav_hat, data = d, cluster = vcov_uid)
    m3 <- feols(d_levy ~ d_enrollment | year | neg_d_eav + pos_d_eav ~ reassess_year + pos_d_eav_hat, data = d, cluster = vcov_uid)
    m4 <- feols(d_levy ~ d_total_ig_revenue | year | neg_d_eav + pos_d_eav ~ reassess_year + pos_d_eav_hat, data = d, cluster = vcov_uid)
    m5 <- feols(d_levy ~ d_total_ig_revenue | year + n_uniqueid | neg_d_eav + pos_d_eav ~ reassess_year + pos_d_eav_hat,
      data = d, cluster = vcov_uid)
  }

  list(M1 = m1, M2 = m2, M3 = m3, M4 = m4, M5 = m5)
}

asym_school_ols <- purrr::map(minor_types, ~ run_asym_school_models(reg_df, .x, iv = FALSE))
names(asym_school_ols) <- minor_types

asym_school_iv <- purrr::map(minor_types, ~ run_asym_school_models(reg_df, .x, iv = TRUE))
names(asym_school_iv) <- minor_types

walk(1:5, function(i) {
  save_table(
    purrr::map(asym_school_ols, ~ .x[[i]]),
    file_stub = glue("asym_school_ols_regressions_M{i}"),
    title = "OLS School Regressions by Type"
  )

  save_table(
    purrr::map(asym_school_iv, ~ .x[[i]]),
    file_stub = glue("asym_school_iv_regressions_M{i}"),
    title = "IV School Regressions by Type"
  )
})


# Table 8 ---------------------------------------------------------------------
# ================================================================
# 15. LAGGED EFFECTS
# ================================================================

reg_df <- reg_df |>
  group_by(n_agency_group) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    d_2_eav = 100 * log((lag_av * lag_eq_factor_final) / (lag2_av * lag2_eq_factor_final)),
    d_3_eav = 100 * log((lag2_av * lag2_eq_factor_final) / (lag3_av * lag3_eq_factor_final))
  ) |>
  ungroup()

lag_all_ols_1 <- feols(d_levy ~ d_eav + d_2_eav + d_3_eav, data = reg_df, cluster = vcov_uid)
lag_all_ols_2 <- feols(d_levy ~ d_eav + d_2_eav + d_3_eav | year, data = reg_df, cluster = vcov_uid)
lag_all_ols_3 <- feols(d_levy ~ d_eav + d_2_eav + d_3_eav + d_total_ig_revenue | year, data = reg_df, cluster = vcov_uid)
lag_all_ols_4 <- feols(
  d_levy ~ d_eav + d_2_eav + d_3_eav + d_total_ig_revenue | year + n_uniqueid,
  data = reg_df,
  cluster = vcov_uid
)

save_table(
  list(M1 = lag_all_ols_1, M2 = lag_all_ols_2, M3 = lag_all_ols_3, M4 = lag_all_ols_4),
  file_stub = "lagged_ols_regressions",
  title = "Table X: OLS Predict levy using d_eav and lags",
  notes = c(
    "Cook County, Illinois data from 2009 through 2023.",
    "Columns 2, 3, and 4 include year dummies; column 4 includes unit dummies.",
    "Check lagged combined effects with fixest::lincom(model, 'd_2_eav + d_3_eav = 0')."
  )
)

run_lag_type_models <- function(df, gov) {
  d <- df |> filter(type_2 == gov)

  m1 <- feols(d_levy ~ d_eav + d_2_eav + d_3_eav, data = d, cluster = vcov_uid)
  m2 <- feols(d_levy ~ d_eav + d_2_eav + d_3_eav | year, data = d, cluster = vcov_uid)
  m3 <- feols(d_levy ~ d_eav + d_2_eav + d_3_eav + d_total_ig_revenue | year, data = d, cluster = vcov_uid)
  m4 <- feols(d_levy ~ d_eav + d_2_eav + d_3_eav + d_total_ig_revenue | year + n_uniqueid,
    data = d, cluster = vcov_uid)

  list(M1 = m1, M2 = m2, M3 = m3, M4 = m4)
}

lag_by_type <- purrr::map(gov_types, ~ run_lag_type_models(reg_df, .x))
names(lag_by_type) <- gov_types

walk(1:4, function(i) {
  save_table(
    purrr::map(lag_by_type, ~ .x[[i]]),
    file_stub = glue("lagged_ols_regressions_by_type_M{i}"),
    title = "Lagged Regressions by Government Type"
  )
})

run_lag_school_models <- function(df, subtype) {
  d <- df |> filter(type == "School", minor_type == subtype)

  m1 <- feols(d_levy ~ d_eav + d_2_eav + d_3_eav, data = d, cluster = vcov_uid)
  m2 <- feols(d_levy ~ d_eav + d_2_eav + d_3_eav | year, data = d, cluster = vcov_uid)
  m3 <- feols(d_levy ~ d_eav + d_2_eav + d_3_eav + d_enrollment | year, data = d, cluster = vcov_uid)
  m4 <- feols(d_levy ~ d_eav + d_2_eav + d_3_eav + d_total_ig_revenue | year, data = d, cluster = vcov_uid)
  m5 <- feols(d_levy ~ d_eav + d_2_eav + d_3_eav + d_total_ig_revenue | year + n_uniqueid,
    data = d, cluster = vcov_uid)

  list(M1 = m1, M2 = m2, M3 = m3, M4 = m4, M5 = m5)
}

lag_school <- purrr::map(minor_types, ~ run_lag_school_models(reg_df, .x))
names(lag_school) <- minor_types

walk(1:5, function(i) {
  save_table(
    purrr::map(lag_school, ~ .x[[i]]),
    file_stub = glue("lagged_school_regressions_M{i}"),
    title = "Lagged School Regressions by Type"
  )
})
