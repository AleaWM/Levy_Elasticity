# Setup ----------------------------------------------------------------------
# descriptive_stats_22_lags_cluster_corrected_v7_4_1.R
# Rough R translation of: descriptive stats_15_lags_cluster.do
# Converted from Stata to R on 2026-03-06.
# Updated on May 11th 2026 using 22_lags_cluster.do
#
# Notes:
# - This keeps the overall model structure but applies corrected, consistent inference.
# - Update the directory paths below before running.
# - Stata's esttab output is translated to modelsummary output. You can
#   swap to gt / flextable / tinytable if you want prettier tables.
# - Some table formatting is approximate rather than byte-for-byte identical.
# - v7.3 adds significance stars to summary-table estimates and shows the
#   estimate and clustered SE alongside each reported upper bound.
# - v7.4 adds conditional weak-IV diagnostics for asymmetric models,
#   by-type IV diagnostics, and joint lag tests.
# - v7.4.1 fixes a dplyr data-mask name collision in the asymmetric by-type
#   diagnostics and retains agency-clustered standard errors throughout.

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
  library(fixest)
  library(modelsummary)
  library(broom)
  library(glue)
  library(readr)
  library(lfe) # Sanderson-Windmeijer conditional F statistics for the asymmetric IV models are computed with lfe::condfstat().

})



options(scipen = 999)

# Used by Table A1 before the longer inference-helper section is reached.
fmt <- function(x, digits = 3) {
  ifelse(is.na(x), "", formatC(as.numeric(x), digits = digits, format = "f"))
}

# 1. READ + MERGE DATA ----

message(Sys.Date())
message(format(Sys.time(), "%H:%M:%S"))



# file DM used:  #"NTA_data_2024_10_14.csv"
# email shows that he had questions about this file and then michael sent one for 10_16
# main_df <- read_csv("NTA_data_2024_10_14.csv")

# Inspect agency groups after main_df is loaded, not before a fresh-session import.

# File MVH sent him:
# main_df <- read_csv("NTA_data_2024_10_16.csv") |>
#   arrange(type)


# Most recent file AWM had on her computer:
# main_df <- read_csv("NTA_data_2024_11_08.csv") #|>
# arrange(type)

main_df <- read_csv("./NTA_2026_Pittsburg/NTA_data_2026_10_02.csv")

agency_lookup <- read_dta("Necessary_Files/fips_all_agency_name.dta")

anti_join(main_df, agency_lookup)

main_df <- main_df |>
  left_join(agency_lookup, by = "agency_group")
# 6593 obs for NTA_data_2026_10_02.csv, as of October 9th 2026 (6592 after dropping NAs in next line of code)
# 7470 observations before dropping NAs, 7416 after dropping NAs for 2024 dataset
# Stata listed unmatched rows and dropped merge==1 rows.
# In dplyr terms: drop observations from master that did not match.
main_df <- main_df |>
  filter(!is.na(fipsid))

census_df <- read_dta("Necessary_Files/census_data.dta")

anti_join(main_df, census_df)

main_df <- main_df |>
  left_join(census_df, by = c("fipsid", "year"))

# 2. MERGE EQUALIZATION FACTORS + CLEAN NUMERIC FIELDS ----

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

string_num_vars <- c("assess_year_eav",  "assess_year_av", "av_true", "rate_smooth", "total_final_levy")

main_df <- main_df |>
  mutate(
    across(
      any_of(string_num_vars),
      ~ readr::parse_number(as.character(.x), na = c("NA", "", ".")),
      .names = "n_{.col}"
    )
  )

# 3. PANEL SETUP + CONSTRUCTED VARIABLES ----

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

# 4. HELPERS ----

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

# output folder ----

output_dir <- file.path("results", "NTA_tables_10_02")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

save_table <- function(models, file_stub, title = NULL, notes = NULL, coef_map = NULL,
                       gof_omit = "IC|Log|Adj|RMSE", statistic = "({std.error})",
                       add_rows = NULL) {
  out_file <- file.path(output_dir, paste0(file_stub, ".html"))
  modelsummary(
    models,
    output = out_file,
    title = title,
    notes = notes,
    coef_map = coef_map,
    statistic = statistic,
    add_rows = add_rows,
    stars = c("*" = .10, "**" = .05, "***" = .01),
    gof_omit = gof_omit
  )
  invisible(out_file)
}

agency_column_order <- c(
  "Home-rule municipalities",
  "Non-home-rule municipalities",
  "Schools",
  "Townships",
  "Other"
)

make_wide_point_table <- function(df, value_col) {
  value_col <- rlang::ensym(value_col)

  df |>
    select(row, agency, !!value_col) |>
    tidyr::pivot_wider(
      names_from = agency,
      values_from = !!value_col
    ) |>
    select(
      row,
      any_of(agency_column_order),
      everything()
    )
}

make_wide_component_point_table <- function(df, value_col) {
  value_col <- rlang::ensym(value_col)

  df |>
    mutate(row = paste(row, component, sep = " - ")) |>
    select(row, agency, !!value_col) |>
    tidyr::pivot_wider(
      names_from = agency,
      values_from = !!value_col
    ) |>
    select(
      row,
      any_of(agency_column_order),
      everything()
    )
}
# Appendix A1 ---------------------------------------------------------------
# 5. INSTRUMENT RELEVANCE CHECKS (not validity checks) ----

first_stage_F <- function(model, term = "reassess_year") {
  td <- broom::tidy(model)

  td |>
    dplyr::filter(.data$term == term) |>
    dplyr::summarise(F_statistic = (.data$statistic[[1]])^2) |>
    dplyr::pull(F_statistic)
}

instrument_df <- main_df |>
  filter(year > 2008)

table(instrument_df$year) # 2009 through 2023

feols(d_eav ~ reassess_year | year^home_rule_ind + uniqueid,
  data = instrument_df,
  cluster = vcov_uid,
  fsplit = ~type)

feols(d_eav ~ reassess_year | year + uniqueid,
  data = instrument_df |> filter(type == "Muni"),
  cluster = vcov_uid,
  fsplit = ~home_rule_ind)



all_instrument <- feols(
  d_eav ~ reassess_year |
    year^home_rule_ind + n_uniqueid,
  data = instrument_df,
  cluster = vcov_uid
)

muni_instrument <- feols(
  d_eav ~ reassess_year |
    year + n_uniqueid,
  data = instrument_df |>
    filter(type == "Muni"),
  cluster = vcov_uid
)

hr_muni_instrument <- feols(
  d_eav ~ reassess_year |
    year + n_uniqueid,
  data = instrument_df |>
    filter(type == "Muni" & home_rule_ind == 1),
  cluster = vcov_uid
)

nonhr_muni_instrument <- feols(
  d_eav ~ reassess_year |
    year + n_uniqueid,
  data = instrument_df |>
    filter(type == "Muni" & home_rule_ind == 0),
  cluster = vcov_uid
)
other_instrument <- feols(
  d_eav ~ reassess_year |
    year + n_uniqueid,
  data = instrument_df |>
    filter(type == "Other"),
  cluster = vcov_uid
)

school_instrument <- feols(
  d_eav ~ reassess_year |
    year + n_uniqueid,
  data = instrument_df |>
    filter(type == "School"),
  cluster = vcov_uid
)

township_instrument <- feols(
  d_eav ~ reassess_year |
    year + n_uniqueid,
  data = instrument_df |>
    filter(type == "Township"),
  cluster = vcov_uid
)

instrument_models <- list(
  "All agencies" = all_instrument,
  "All Munis" = muni_instrument,
  "HR Munis" = hr_muni_instrument,
  "Non-HR Munis" = nonhr_muni_instrument,
  "Other" = other_instrument,
  "Schools" = school_instrument,
  "Townships" = township_instrument
)
instrument_F <- vapply(
  instrument_models,
  first_stage_F,
  numeric(1)
)
instrument_add_rows <- tibble::tibble(
  term = c(
    "Excluded-instrument F",
    "Year-by-home-rule FE",
    "Year FE",
    "Agency FE"
  ),
  `All agencies` = c(
    fmt(instrument_F[["All agencies"]], 1),
    "Yes", "Yes", "Yes"
  ),
  `All Munis` = c(
    fmt(instrument_F[["All Munis"]], 1),
    "No", "Yes", "Yes"
  ),

  `HR Munis` = c(
    fmt(instrument_F[["HR Munis"]], 1),
    "No", "Yes", "Yes"
  ),
  `Non-HR Munis` = c(
    fmt(instrument_F[["Non-HR Munis"]], 1),
    "No", "Yes", "Yes"
  ),
  Other = c(
    fmt(instrument_F[["Other"]], 1),
    "No", "Yes", "Yes"
  ),
  Schools = c(
    fmt(instrument_F[["Schools"]], 1),
    "No", "Yes", "Yes"
  ),
  Townships = c(
    fmt(instrument_F[["Townships"]], 1),
    "No", "Yes", "Yes"
  )
)

## Table A1: Instrument relevance - export ----
save_table(
  instrument_models,
  file_stub = "instrument_validity_checks",
  title = paste(
    "Table A1: Instrument relevance:",
    "EAV changes predicted by reassessment year"
  ),
  notes = c(
    "Standard errors are clustered by taxing agency.",
    paste(
      "The excluded-instrument F statistic tests whether",
      "reassessment year predicts the change in EAV."
    )
  ),
  coef_map = c(
    "reassess_year" = "Reassessment year"
  ),
  add_rows = instrument_add_rows,
  gof_omit = "IC|Log|Adj|RMSE|Std.Errors"
)
# save_table(
#   list(
#     "All agencies" = all_instrument,
#     "All Munis" = muni_instrument,
#     "HR Munis" = hr_muni_instrument,
#     "Non-HR Munis" = nonhr_muni_instrument,
#     "Other" = other_instrument,
#     "Schools" = school_instrument,
#     "Townships" = township_instrument
#   ),
#   file_stub = "instrument_validity_checks",
#   title = "Table X: Predict assessments by reassessment year by type",
#   notes = "Cook County, Illinois data from 2008 through 2023"
# )

# 6. BASE SAMPLE FOR OLS / IV ----

reg_df <- main_df |>
  filter(year > 2008)



# V22 MODEL UPDATES TRANSLATED FROM STATA ----
# Source Stata comparison:
# - descriptive stats_15_lags_cluster(2).do
# - descriptive stats_22_lags_cluster.do
#
# Core v22 changes:
# 1. Keep non-home-rule municipalities separate as type_2 == "NonHR_muni"; label combined municipal models as "All Munis". ----
# 2. Use year-by-home-rule fixed effects in the all-agency models. ----
# 3. Report upper bounds: estimate + 1.645 * clustered SE. ----
# 4. Add same-sample rows for Census-control models. ----
# 5. Add appendix-style point-estimate summaries with p-values and significance stars. ----
# 6. v6 adds positive- and negative-EAV upper-bound summaries for asymmetric models and fixes F_pos/F_neg extraction by estimating explicit first stages on the IV sample. ----

suppressPackageStartupMessages({
  library(knitr)
})

# output helpers ----
html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

save_df_table <- function(df, file_stub, title = NULL, digits = 3, notes = NULL) {
  out_file <- file.path(output_dir, paste0(file_stub, ".html"))

  f_stat_columns <- c(
    "first_stage_F",
    "F_neg",
    "F_pos",
    "SW_F_neg",
    "SW_F_pos",
    "joint_lag_F"
  )

  df_out <- df |>
    mutate(
      across(where(is.numeric), ~ round(.x, digits)),
      across(any_of(f_stat_columns), ~ round(.x, 1))
    )

  # Add an inference note automatically when a table contains starred estimates
  # or the compact upper-bound display: UB [Est. coefficient; SE standard error].
  has_inference_display <- any(vapply(
    df_out,
    function(x) is.character(x) && any(grepl("\\*|\\[Est\\.", x), na.rm = TRUE),
    logical(1)
  ))

  if (is.null(notes) && has_inference_display) {
    notes <- paste0(
      "Where shown: * p < 0.10; ** p < 0.05; *** p < 0.01. ",
      "Stars refer to the underlying estimate, not the upper bound. ",
      "Upper-bound cells are 95% one-sided upper confidence bounds."
    )
  }

  html <- knitr::kable(df_out, format = "html", caption = title, escape = TRUE)

  if (!is.null(notes)) {
    note_html <- paste0(
      "<div class=\"table-note\" style=\"text-align: left;\"><small>",
      html_escape(paste(notes, collapse = " ")),
      "</small></div>"
    )
    html <- paste(html, note_html, sep = "\n")
  }

  writeLines(html, out_file)
  invisible(df_out)
}

# Table 1: Distribution of levy and EAV changes ----
# Reuses the current merged panel and growth variables; no model-control filter.
# Table 1 follows old_files/levy_paper_tables_simple.qmd, using R's default
# quantile type 7 (not a claim of identical interpolation to Stata centile).
descriptive_df <- reg_df |>
  filter(between(year, 2009, 2023))
stopifnot(
  nrow(descriptive_df) > 0L,
  !anyNA(descriptive_df$agency_group),
  !anyDuplicated(descriptive_df[c("agency_group", "year")]),
  all(descriptive_df$type %in% c("Muni", "Other", "School", "Township"))
)

table1_long <- descriptive_df |>
  select(type, d_levy, d_eav) |>
  pivot_longer(c(d_levy, d_eav), names_to = "variable", values_to = "value")
table1_quantile <- function(x, probability) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(NA_real_)
  unname(quantile(x, probability, type = 7))
}
table1 <- bind_rows(table1_long, mutate(table1_long, type = "All")) |>
  group_by(type, variable) |>
  summarise(
    N = sum(is.finite(value)),
    p5 = table1_quantile(value, .05),
    p25 = table1_quantile(value, .25),
    p50 = table1_quantile(value, .50),
    p75 = table1_quantile(value, .75),
    p95 = table1_quantile(value, .95),
    .groups = "drop"
  ) |>
  arrange(match(type, c("Muni", "Other", "School", "Township", "All")),
          match(variable, c("d_levy", "d_eav"))) |>
  rename(agency = type)
write_csv(table1, file.path(output_dir, "v22_table_01_distribution.csv"))
## Table 1: Distribution of levy and EAV changes - export distribution ----
save_df_table(
  table1, "v22_table_01_distribution",
  "Table 1. Distribution of annual percentage changes in levies and EAV",
  digits = 1,
  notes = paste(
    "2009-2023. Changes are 100 times annual log ratios.",
    "N counts finite agency-year observations separately for each variable.",
    "Percentiles use the untrimmed sample and R quantile type 7."
  )
)

# Figure 1: Observed levy versus EAV changes ----
# Six overlapping panels, matching the supplied historical figure's layout.
# Reconstruction assumption: trim BOTH coordinates to +/-20 before fitting OLS.
# Annual agency counts describe complete pairs BEFORE trimming, not plotted N.
figure1_levels <- c("All local gov't types", "Muni", "Home Rule Muni",
                    "Other", "School", "Township")
figure1_pairs <- descriptive_df |>
  filter(is.finite(d_eav), is.finite(d_levy))
figure1_panels <- bind_rows(
  mutate(figure1_pairs, panel = figure1_levels[1]),
  mutate(figure1_pairs, panel = as.character(type)),
  figure1_pairs |>
    filter(type == "Muni", home_rule_ind == 1) |>
    mutate(panel = "Home Rule Muni")
) |>
  mutate(panel = factor(panel, levels = figure1_levels))
figure1_counts <- figure1_panels |>
  distinct(panel, year, agency_group) |>
  count(panel, year, name = "agencies") |>
  complete(panel, year = 2009:2023, fill = list(agencies = 0L)) |>
  group_by(panel) |>
  summarise(annual_min = min(agencies), annual_max = max(agencies),
            .groups = "drop") |>
  mutate(label = paste0(
    panel, "\nn=", annual_min,
    if_else(annual_min == annual_max, "", paste0("-", annual_max)),
    " per year (before trimming)"
  ))
figure1_plot_data <- figure1_panels |>
  filter(between(d_eav, -20, 20), between(d_levy, -20, 20))
figure1_audit <- figure1_plot_data |>
  group_by(panel) |>
  summarise(plotted_N = n(), unique_agencies = n_distinct(agency_group),
            .groups = "drop") |>
  right_join(figure1_counts, by = "panel") |>
  mutate(
    plotted_N = replace_na(plotted_N, 0L),
    unique_agencies = replace_na(unique_agencies, 0L),
    label = paste0(panel, "\nN = ", format(unique_agencies, big.mark = ",", trim = TRUE),
                   " unique agencies")
  )
write_csv(figure1_audit, file.path(output_dir, "v22_figure_01_counts.csv"))
figure1 <- ggplot(figure1_plot_data, aes(d_eav, d_levy)) +
  geom_point(color = "#2185FF", size = 0.65, alpha = 0.85) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              color = "red", linewidth = 0.6, fullrange = TRUE) +
  geom_abline(intercept = 0, slope = 1, color = "#A38B38", linewidth = 0.6) +
  facet_wrap(~panel, ncol = 3, drop = FALSE,
             labeller = labeller(panel = setNames(figure1_audit$label,
                                                  figure1_audit$panel))) +
  scale_x_continuous(limits = c(-20, 20), breaks = seq(-20, 20, 10)) +
  scale_y_continuous(breaks = seq(-20, 20, 10)) +
  coord_cartesian(ylim = c(-20, 20)) +
  labs(
    title = "Observed % change levy wrt % change in assessments",
    subtitle = "No controls: 2009 through 2023",
    x = "% change EAV", y = "% change levy",
    caption = paste(
      "Changes are 100 times log ratios. Both coordinates restricted to [-20,20].",
      "Red: OLS fitted on the displayed sample. Brown: levy growth equals EAV growth.",
      "Above/below the diagonal: the levy/EAV ratio rises/falls. N counts unique agencies represented after trimming.",
      sep = "\n"
    )
  ) +
  theme_bw(base_size = 12) +
  theme(strip.background = element_blank(), panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0), plot.title.position = "plot")
ggsave(file.path(output_dir, "v22_figure_01_observed_changes.png"),
       figure1, width = 13, height = 8, dpi = 300, bg = "white")
ggsave(file.path(output_dir, "v22_figure_01_observed_changes.pdf"),
       figure1, width = 13, height = 8, bg = "white")

## Figure 1: Clean presentation version - export ----
# Same observations and OLS specification; presentation changes only.
# Keep explanatory text outside this image; panel labels count unique agencies after trimming.
figure1_clean_labels <- setNames(
  c("All agencies", "Municipalities", "Home-rule municipalities",
    "Other districts", "Schools", "Townships"),
  figure1_levels
)
figure1_clean_labels <- setNames(
  paste0(figure1_clean_labels[as.character(figure1_audit$panel)],
         "\nN = ", format(figure1_audit$unique_agencies, big.mark = ",", trim = TRUE),
         " unique agencies"),
  as.character(figure1_audit$panel)
)
figure1_clean <- ggplot(figure1_plot_data, aes(d_eav, d_levy)) +
  geom_abline(intercept = 0, slope = 1, color = "#A4ADB5",
              linewidth = 0.5, linetype = "longdash") +
  geom_hline(yintercept = 0, color = "#D7DEE3", linewidth = 0.3) +
  geom_vline(xintercept = 0, color = "#D7DEE3", linewidth = 0.3) +
  geom_point(color = "#247C87", size = 0.7, alpha = 0.35, stroke = 0) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              color = "#CC6849", linewidth = 0.85, fullrange = TRUE) +
  facet_wrap(~panel, ncol = 3, drop = FALSE,
             labeller = labeller(panel = figure1_clean_labels)) +
  scale_x_continuous(limits = c(-20, 20), breaks = seq(-20, 20, 10),
                     expand = expansion(mult = 0.03)) +
  scale_y_continuous(breaks = seq(-20, 20, 10),
                     expand = expansion(mult = 0.03)) +
  coord_fixed(ratio = 1, ylim = c(-20, 20)) +
  labs(x = "Annual change in EAV (%)", y = "Annual change in levy (%)",
       title = NULL, subtitle = NULL, caption = NULL) +
  theme_minimal(base_size = 12, base_family = "sans") +
  theme(
    text = element_text(color = "#293740"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#EDF0F2", linewidth = 0.3),
    strip.text = element_text(face = "bold", size = 11,
                              margin = margin(b = 12, t = 6)),
    axis.text = element_text(color = "#66747D", size = 10),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 12)),
    panel.spacing = grid::unit(1.3, "lines"),
    plot.margin = margin(16, 20, 16, 16),
    legend.position = "none"
  )
ggsave(file.path(output_dir, "v22_figure_01_observed_changes_clean.png"),
       figure1_clean, width = 12, height = 8.5, dpi = 300, bg = "white")
ggsave(file.path(output_dir, "v22_figure_01_observed_changes_clean.pdf"),
       figure1_clean, width = 12, height = 8.5, bg = "white")

# Model table and sample helpers ----
make_wide_stat_table <- function(df, value_col) {
  value_col <- rlang::ensym(value_col)
  df |>
    select(row, agency, !!value_col) |>
    tidyr::pivot_wider(names_from = agency, values_from = !!value_col)
}

# Core extraction helpers are defined below in the corrected inference section.

sample_index <- function(model, data) {
  idx <- tryCatch(fixest::obs(model), error = function(e) NULL)
  if (!is.null(idx)) return(idx)
  # fallback: mark complete cases for variables used in the model is not perfect,
  # but prevents hard failure if fixest::obs changes.
  seq_len(nrow(data))
}

`%||%` <- function(x, y) if (is.null(x)) y else x


# corrected inference helpers ----
# These helpers are used by the corrected manuscript tables. They keep standard
# errors clustered by taxing agency and add derived statistics to modelsummary.
# Coefficient p-values are retained so manuscript-facing summary tables can
# attach significance stars to every displayed estimate.

sig_stars <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE ~ ""
  )
}

format_estimate <- function(estimate, p_value, digits = 3) {
  dplyr::if_else(
    is.na(estimate),
    "",
    paste0(fmt(estimate, digits), sig_stars(p_value))
  )
}

format_upper_bound <- function(upper_bound, estimate, std_error, p_value,
                               digits = 3) {
  dplyr::if_else(
    is.na(upper_bound),
    "",
    paste0(
      fmt(upper_bound, digits)   # ,
      # " [Est. ", fmt(estimate, digits), sig_stars(p_value),
      # "; SE ", fmt(std_error, digits), "]"
    )
  )
}

model_n <- function(model) {
  # modelsummary can usually find Num.Obs. for fixest models, but the
  # manuscript-summary helpers need their own fallback. Some fixest objects
  # do not cooperate with stats::nobs(), so try the object slots/residuals first.
  candidates <- list(
    tryCatch(model$nobs, error = function(e) NULL),
    tryCatch(length(model$residuals), error = function(e) NULL),
    tryCatch(length(stats::residuals(model)), error = function(e) NULL),
    tryCatch(length(fixest::obs(model)), error = function(e) NULL),
    tryCatch(stats::nobs(model), error = function(e) NULL)
  )

  for (x in candidates) {
    if (is.null(x) || length(x) == 0) next

    # If a method returns more than one number, the first element is the
    # observation count for the fitted sample in the fixest objects used here.
    x1 <- suppressWarnings(as.numeric(x[[1]]))

    if (length(x1) == 1 && is.finite(x1) && !is.na(x1) && x1 > 0) {
      return(as.integer(x1))
    }
  }

  NA_integer_
}

find_model_term <- function(model, terms) {
  b <- tryCatch(stats::coef(model), error = function(e) NULL)

  if (is.null(b)) {
    return(NA_character_)
  }

  terms <- terms[terms %in% names(b)]

  if (length(terms) == 0) {
    return(NA_character_)
  }

  terms[[1]]
}

safe_coef <- function(model, terms) {
  b <- tryCatch(stats::coef(model), error = function(e) NULL)

  if (is.null(b)) {
    return(NA_real_)
  }

  term <- find_model_term(model, terms)

  if (is.na(term)) {
    return(NA_real_)
  }

  unname(b[[term]])
}

safe_se <- function(model, terms) {
  term <- find_model_term(model, terms)

  if (is.na(term)) {
    return(NA_real_)
  }

  # fixest::se() is the most direct route because it uses the model's
  # clustered vcov setting. Fall back to vcov() only if needed.
  s <- tryCatch(fixest::se(model), error = function(e) NULL)

  if (!is.null(s) && term %in% names(s)) {
    return(unname(s[[term]]))
  }

  V <- tryCatch(stats::vcov(model), error = function(e) NULL)

  if (is.null(V)) {
    return(NA_real_)
  }

  if (!(term %in% rownames(V)) || !(term %in% colnames(V))) {
    return(NA_real_)
  }

  unname(sqrt(V[term, term]))
}

safe_p_value <- function(model, terms) {
  term <- find_model_term(model, terms)

  if (is.na(term)) {
    return(NA_real_)
  }

  p <- tryCatch(
    fixest::pvalue(model),
    error = function(e) NULL
  )

  if (!is.null(p) && term %in% names(p)) {
    return(unname(p[[term]]))
  }

  # Fallback: retain the model's clustered SE and use fixest's t-test
  # degrees of freedom rather than a standard-normal reference distribution.
  est <- safe_coef(model, term)
  se <- safe_se(model, term)
  df_t <- tryCatch(
    fixest::degrees_freedom(model, type = "t"),
    error = function(e) NA_real_
  )

  if (is.na(est) || is.na(se) || se == 0 || !is.finite(df_t)) {
    return(NA_real_)
  }

  2 * stats::pt(abs(est / se), df = df_t, lower.tail = FALSE)
}

coef_p <- function(model, term) {
  p <- tryCatch(
    fixest::pvalue(model),
    error = function(e) NULL
  )

  if (is.null(p) || !term %in% names(p)) {
    return(NA_real_)
  }

  unname(p[[term]])
}
# coef_p <- function(model, term) {
#   # For one-coefficient control-function tests, compute the p-value from the
#   # coefficient and the model's clustered vcov instead of relying on coeftable().
#   safe_p_value(model, term)
# }

term_stats <- function(model, term = "d_eav", label = NULL) {

  term_candidates <- unique(c(
    term,
    paste0("fit_", term),
    sub("^fit_", "", term)
  ))

  # Use the same N extraction path as add_rows_from_named().
  N_val <- n_vec(list(model = model))[["model"]]
  model_label <- label %||% deparse(substitute(model))

  td <- tryCatch(
    broom::tidy(model),
    error = function(e) NULL
  )

  empty_result <- function() {
    tibble::tibble(
      model = model_label,
      N = N_val,
      estimate = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_,
      upper_bound = NA_real_,
      estimate_with_stars = "",
      upper_bound_with_details = ""
    )
  }

  if (is.null(td)) {
    return(empty_result())
  }

  td_use <- td |>
    dplyr::filter(.data$term %in% term_candidates) |>
    dplyr::slice(1)

  if (nrow(td_use) == 0) {
    warning(
      "No matching term found. Requested: ",
      paste(term_candidates, collapse = ", "),
      ". Available terms: ",
      paste(td$term, collapse = ", ")
    )

    return(empty_result())
  }

  estimate <- td_use$estimate[[1]]
  std_error <- td_use$std.error[[1]]
  p_value <- if ("p.value" %in% names(td_use)) {
    td_use$p.value[[1]]
  } else {
    safe_p_value(model, term_candidates)
  }
  upper_bound <- estimate + 1.645 * std_error

  tibble::tibble(
    model = model_label,
    N = N_val,
    estimate = estimate,
    std_error = std_error,
    p_value = p_value,
    upper_bound = upper_bound,
    estimate_with_stars = format_estimate(estimate, p_value),
    upper_bound_with_details = format_upper_bound(
      upper_bound, estimate, std_error, p_value
    )
  )
}



check_model_extractors <- function(model, term = "d_eav") {
  term_candidates <- unique(c(
    term,
    paste0("fit_", term),
    sub("^fit_", "", term)
  ))

  tibble::tibble(
    class = paste(class(model), collapse = ", "),
    coef_names = paste(names(tryCatch(stats::coef(model), error = function(e) NULL)), collapse = ", "),
    matched_term = find_model_term(model, term_candidates),
    N = model_n(model),
    estimate = safe_coef(model, term_candidates),
    std_error = safe_se(model, term_candidates),
    p_value = safe_p_value(model, term_candidates)
  )
}

# Linear restriction test using the model's current vcov matrix.
# weights is a named numeric vector, e.g. c(d_2_eav = 1, d_3_eav = 1).
linear_combo_test <- function(model, weights, rhs = 0) {
  b <- stats::coef(model)
  V <- tryCatch(stats::vcov(model), error = function(e) NULL)
  terms <- names(weights)

  if (
    is.null(V) ||
      !all(terms %in% names(b)) ||
      !all(terms %in% rownames(V)) ||
      !all(terms %in% colnames(V))
  ) {
    return(tibble(
      estimate = NA_real_,
      std_error = NA_real_,
      statistic = NA_real_,
      p_value = NA_real_
    ))
  }

  w <- as.numeric(weights)
  names(w) <- terms

  est <- sum(w * b[terms])
  Vsub <- V[terms, terms, drop = FALSE]
  se <- sqrt(as.numeric(t(w) %*% Vsub %*% w))
  stat <- (est - rhs) / se

  # Use the same clustered t-test degrees of freedom as the fixest model.
  df_t <- tryCatch(
    fixest::degrees_freedom(model, type = "t"),
    error = function(e) NA_real_
  )

  p <- if (is.finite(df_t)) {
    2 * stats::pt(abs(stat), df = df_t, lower.tail = FALSE)
  } else {
    NA_real_
  }

  tibble(
    estimate = est,
    std_error = se,
    statistic = stat,
    p_value = p
  )
}

# Also, 1.645 corresponds to a 95% one-sided bound, equivalently one endpoint of a 90% two-sided interval. The notes need to state which interpretation is intended.

p_equal_terms <- function(model, lhs = "neg_d_eav", rhs = "pos_d_eav") {
  out <- linear_combo_test(model, setNames(c(1, -1), c(lhs, rhs)), rhs = 0)
  out$p_value[[1]]
}

lincom_sum <- function(model, terms, label = NULL) {
  out <- linear_combo_test(model, setNames(rep(1, length(terms)), terms), rhs = 0)
  estimate <- out$estimate[[1]]
  std_error <- out$std_error[[1]]
  p_value <- out$p_value[[1]]
  upper_bound <- estimate + 1.645 * std_error

  tibble(
    model = label %||% deparse(substitute(model)),
    N = model_n(model),
    lagged_magnitude = estimate,
    std_error = std_error,
    A_upper_bound = upper_bound,
    lagged_p = p_value,
    lagged_estimate_with_stars = format_estimate(estimate, p_value),
    A_upper_bound_with_details = format_upper_bound(
      upper_bound, estimate, std_error, p_value
    )
  )
}

lag_only_p <- function(model) {
  linear_combo_test(model, c(d_2_eav = 1, d_3_eav = 1), rhs = 0)$p_value[[1]]
}

safe_first_stage_f <- function(model) {
  out <- tryCatch(
    fixest::fitstat(
      model,
      "ivwald1.stat",
      simplify = TRUE,
      verbose = FALSE
    ),
    error = function(e) NULL
  )

  if (is.null(out) || length(out) == 0) {
    return(NA_real_)
  }

  as.numeric(out)[1]
}

safe_first_stage_f_all <- function(model) {
  out <- tryCatch(
    fixest::fitstat(
      model,
      "ivwald1",
      simplify = TRUE,
      verbose = FALSE
    ),
    error = function(e) NULL
  )

  if (is.null(out)) {
    return(numeric(0))
  }

  as.numeric(unlist(out))
}

joint_wald_p <- function(model, terms) {
  # Exact coefficient-name patterns. Dots must be escaped for regex matching.
  keep_terms <- paste0(
    "^",
    gsub(".", "\\.", terms, fixed = TRUE),
    "$"
  )

  out <- tryCatch(
    fixest::wald(
      model,
      keep = keep_terms,
      print = FALSE
    ),
    error = function(e) NULL
  )

  if (is.null(out) || length(out) == 1 && is.na(out)) {
    return(NA_real_)
  }

  as.numeric(out[["p"]])[1]
}

cluster_p_exog_single <- function(iv_model, data, outcome = "d_levy", endog = "d_eav",
                                  instruments = "reassess_year", exog_controls = NULL,
                                  fe = "0", cluster = "~ n_uniqueid") {
  # Restrict the control-function test to the IV estimation sample
  d <- data[fixest::obs(iv_model), , drop = FALSE]

  cluster_fml <- as.formula(cluster)
  fe_part <- if (is.null(fe) || fe == "0") "0" else fe
  inst_string <- paste(instruments, collapse = " + ")

  # first stage
  fs_fml <- if (fe_part == "0") {
    as.formula(paste0(endog, " ~ ", inst_string))
  } else {
    as.formula(paste0(endog, " ~ ", inst_string, " | ", fe_part))
  }
  fs <- feols(fs_fml, data = d, cluster = cluster_fml, fixef.rm = "none")

  # Make sure the residuals and data use the same observations
  if (length(resid(fs)) != nrow(d)) {
    d <- d[fixest::obs(fs), , drop = FALSE]
    fs <- feols(fs_fml, data = d, cluster = cluster_fml, fixef.rm = "none")
  }
  d$.fs_resid <- resid(fs)

  # Control-function regression
  rhs_terms <- c(endog, exog_controls, ".fs_resid")
  cf_fml <- if (fe_part == "0") {
    as.formula(paste0(outcome, " ~ ", paste(rhs_terms, collapse = " + ")))
  } else {
    as.formula(paste0(outcome, " ~ ", paste(rhs_terms, collapse = " + "), " | ", fe_part))
  }
  cf <- feols(cf_fml, data = d, cluster = cluster_fml, fixef.rm = "none")
  coef_p(cf, ".fs_resid")
}

cluster_p_exog_asym <- function(iv_model, data, outcome = "d_levy",
                                endog = c("neg_d_eav", "pos_d_eav"),
                                instruments = c("reassess_year", "pos_d_eav_hat"),
                                exog_controls = NULL, fe = "0", cluster = "~ n_uniqueid") {
  d <- data[fixest::obs(iv_model), , drop = FALSE]
  cluster_fml <- as.formula(cluster)
  fe_part <- if (is.null(fe) || fe == "0") "0" else fe
  inst_string <- paste(instruments, collapse = " + ")
  resid_names <- paste0(".fs_resid_", endog)

  fs_list <- vector("list", length(endog))
  names(fs_list) <- endog
  for (i in seq_along(endog)) {
    fs_fml <- if (fe_part == "0") {
      as.formula(paste0(endog[i], " ~ ", inst_string))
    } else {
      as.formula(paste0(endog[i], " ~ ", inst_string, " | ", fe_part))
    }
    fs_list[[i]] <- feols(fs_fml, data = d, cluster = cluster_fml, fixef.rm = "none")
  }
  if (any(vapply(fs_list, function(m) length(resid(m)) != nrow(d), logical(1)))) {
    keep <- Reduce(intersect, lapply(fs_list, fixest::obs))
    d <- d[keep, , drop = FALSE]
    for (i in seq_along(endog)) {
      fs_fml <- if (fe_part == "0") {
        as.formula(paste0(endog[i], " ~ ", inst_string))
      } else {
        as.formula(paste0(endog[i], " ~ ", inst_string, " | ", fe_part))
      }
      fs_list[[i]] <- feols(fs_fml, data = d, cluster = cluster_fml, fixef.rm = "none")
    }
  }
  for (i in seq_along(endog)) {
    d[[resid_names[i]]] <- resid(fs_list[[i]])
  }

  rhs_terms <- c(endog, exog_controls, resid_names)
  cf_fml <- if (fe_part == "0") {
    as.formula(paste0(outcome, " ~ ", paste(rhs_terms, collapse = " + ")))
  } else {
    as.formula(paste0(outcome, " ~ ", paste(rhs_terms, collapse = " + "), " | ", fe_part))
  }
  cf <- feols(cf_fml, data = d, cluster = cluster_fml, fixef.rm = "none")
  joint_wald_p(cf, resid_names)
}

n_vec <- function(models) {
  vapply(models, model_n, integer(1))
}

add_rows_from_named <- function(models, rows, digits = NULL) {
  # rows is a named list. Each element should be a named numeric vector with
  # one value per model name. This is deliberately explicit so N is pulled from
  # model_n() just like the other custom statistics rather than left to
  # modelsummary's GOF extractor in some tables and custom code in others.
  model_names <- names(models)
  row_names <- names(rows)

  out <- tibble::tibble(term = row_names)

  for (mn in model_names) {
    out[[mn]] <- vapply(seq_along(rows), function(i) {
      val <- rows[[i]][[mn]]
      if (is.null(val) || length(val) == 0) val <- NA_real_

      d <- if (!is.null(digits) && row_names[[i]] %in% names(digits)) {
        digits[[row_names[[i]]]]
      } else if (grepl("^F|regF", row_names[[i]])) {
        1
      } else if (row_names[[i]] == "N") {
        0
      } else {
        3
      }

      fmt(val, d)
    }, character(1))
  }

  out |>
    dplyr::filter(dplyr::if_any(-term, ~ .x != ""))
}

ub_vec <- function(models, term) {
  vapply(models, function(m) term_stats(m, term)$upper_bound[[1]], numeric(1))
}

regf_vec <- function(models) {
  vapply(models, safe_first_stage_f, numeric(1))
}

# asymmetric-model helpers ----
# These helpers are used for Tables 6--9 B/C/D outputs. The point is to report
# coefficient estimates, clustered SEs, and positive/negative upper bounds.
# Coefficient-level p-values are retained for stars on displayed estimates;
# p_equal, p_exog, and first-stage diagnostics remain separate model tests.
# both positive-EAV and negative-EAV upper bounds rather than only the positive
# coefficient, and to avoid mis-reading fixest::fitstat("ivf1") output when there
# are two endogenous variables.

term_stats_component <- function(model, term, component, label = NULL) {
  term_stats(model, term, label) |>
    mutate(component = component) |>
    select(
      model, component, N, estimate, std_error, p_value, upper_bound,
      estimate_with_stars, upper_bound_with_details
    )
}

asym_component_stats <- function(model, label = NULL, iv = FALSE) {
  term_pos <- if (iv) "fit_pos_d_eav" else "pos_d_eav"
  term_neg <- if (iv) "fit_neg_d_eav" else "neg_d_eav"

  bind_rows(
    term_stats_component(model, term_pos, "Positive EAV change", label),
    term_stats_component(model, term_neg, "Negative EAV change", label)
  )
}

joint_wald_F <- function(model, terms) {
  keep_terms <- paste0(
    "^",
    gsub(".", "\\.", terms, fixed = TRUE),
    "$"
  )

  out <- tryCatch(
    fixest::wald(
      model,
      keep = keep_terms,
      print = FALSE
    ),
    error = function(e) NULL
  )

  if (is.null(out) || length(out) == 1 && is.na(out)) {
    return(NA_real_)
  }

  as.numeric(out[["stat"]])[1]
}

first_stage_F_for <- function(iv_model, data, endog,
                              instruments = c("reassess_year", "pos_d_eav_hat"),
                              exog_controls = NULL, fe = "0",
                              cluster = ~n_uniqueid) {
  idx <- sample_index(iv_model, data)
  d <- data[idx, , drop = FALSE]

  rhs_terms <- c(exog_controls, instruments)
  rhs <- paste(rhs_terms, collapse = " + ")
  fe_part <- if (is.null(fe) || fe == "0") "0" else fe

  fs_fml <- if (fe_part == "0") {
    as.formula(paste0(endog, " ~ ", rhs))
  } else {
    as.formula(paste0(endog, " ~ ", rhs, " | ", fe_part))
  }

  fs <- tryCatch(
    feols(fs_fml, data = d, cluster = cluster, fixef.rm = "none"),
    error = function(e) NULL
  )

  if (is.null(fs)) return(NA_real_)

  joint_wald_F(fs, instruments)
}


# Sanderson-Windmeijer conditional F statistics for IV models with two
# endogenous variables. The diagnostic model mirrors the fixest sample,
# exogenous controls, absorbed fixed effects, and agency clustering.
sw_conditional_f <- function(
  iv_model,
  data,
  fe = "0",
  exog_controls = NULL,
  instruments = c("reassess_year", "pos_d_eav_hat"),
  outcome = "d_levy"
) {
  idx <- sample_index(iv_model, data)
  d <- data[idx, , drop = FALSE]

  if (nrow(d) == 0) {
    return(c(SW_F_neg = NA_real_, SW_F_pos = NA_real_))
  }

  d <- d |>
    mutate(
      .sw_year_fe = factor(year),
      .sw_agency_fe = factor(n_uniqueid),
      .sw_year_hr_fe = interaction(
        year,
        home_rule_for_fe,
        drop = TRUE,
        lex.order = TRUE
      ),
      .sw_cluster_agency = factor(n_uniqueid)
    )

  fe_rhs <- switch(
    fe,
    "0" = "0",
    "year" = ".sw_year_fe",
    "year + n_uniqueid" = ".sw_year_fe + .sw_agency_fe",
    "year^home_rule_for_fe" = ".sw_year_hr_fe",
    "year^home_rule_for_fe + n_uniqueid" =
      ".sw_year_hr_fe + .sw_agency_fe",
    stop("Unrecognized fixed-effects specification: ", fe)
  )

  exog_rhs <- if (
    is.null(exog_controls) ||
      length(exog_controls) == 0
  ) {
    "1"
  } else {
    paste(exog_controls, collapse = " + ")
  }

  instrument_rhs <- paste(instruments, collapse = " + ")

  sw_fml <- as.formula(
    paste0(
      outcome,
      " ~ ", exog_rhs,
      " | ", fe_rhs,
      " | (neg_d_eav | pos_d_eav ~ ", instrument_rhs, ")",
      " | .sw_cluster_agency"
    )
  )

  sw_model <- tryCatch(
    lfe::felm(
      formula = sw_fml,
      data = d
    ),
    error = function(e) {
      warning(
        "Sanderson-Windmeijer diagnostic model failed: ",
        conditionMessage(e)
      )
      NULL
    }
  )

  if (is.null(sw_model)) {
    return(c(SW_F_neg = NA_real_, SW_F_pos = NA_real_))
  }

  sw <- tryCatch(
    as.matrix(lfe::condfstat(sw_model, type = "cluster")),
    error = function(e) {
      warning(
        "Sanderson-Windmeijer conditional F calculation failed: ",
        conditionMessage(e)
      )
      NULL
    }
  )

  if (is.null(sw) || nrow(sw) == 0 || ncol(sw) == 0) {
    return(c(SW_F_neg = NA_real_, SW_F_pos = NA_real_))
  }

  # condfstat returns one column per endogenous variable. If row names are
  # available, prefer the cluster row; otherwise use the first returned row.
  row_id <- if (
    !is.null(rownames(sw)) &&
      any(grepl("cluster", rownames(sw), ignore.case = TRUE))
  ) {
    which(grepl("cluster", rownames(sw), ignore.case = TRUE))[1]
  } else {
    1L
  }

  vals <- as.numeric(sw[row_id, , drop = TRUE])
  names(vals) <- colnames(sw)

  extract_sw <- function(term, fallback_position) {
    if (!is.null(names(vals)) && term %in% names(vals)) {
      return(unname(vals[[term]]))
    }

    matched <- if (!is.null(names(vals))) {
      grep(term, names(vals), fixed = TRUE)
    } else {
      integer(0)
    }

    if (length(matched) > 0) {
      return(unname(vals[[matched[[1]]]]))
    }

    if (length(vals) >= fallback_position) {
      return(unname(vals[[fallback_position]]))
    }

    NA_real_
  }

  c(
    SW_F_neg = extract_sw("neg_d_eav", 1L),
    SW_F_pos = extract_sw("pos_d_eav", 2L)
  )
}


lag_joint_test <- function(model) {
  out <- tryCatch(
    fixest::wald(
      model,
      keep = c("^d_2_eav$", "^d_3_eav$"),
      print = FALSE
    ),
    error = function(e) NULL
  )

  if (is.null(out) || length(out) == 1 && is.na(out)) {
    return(c(
      joint_lag_F = NA_real_,
      joint_lag_p = NA_real_
    ))
  }

  c(
    joint_lag_F = as.numeric(out[["stat"]])[1],
    joint_lag_p = as.numeric(out[["p"]])[1]
  )
}

make_wide_component_stat_table <- function(df, value_col) {
  value_col <- rlang::ensym(value_col)

  df |>
    mutate(row = paste(row, component, sep = " - ")) |>
    select(row, agency, !!value_col) |>
    tidyr::pivot_wider(names_from = agency, values_from = !!value_col)
}


# v22 data recodes ----
reg_df <- reg_df |>
  mutate(
    # Keep municipal types separate in by-type models.
    type_2 = case_when(
      type == "Muni" & home_rule_ind == 1 ~ "HR_muni",
      type == "Muni" & home_rule_ind == 0 ~ "NonHR_muni",
      TRUE ~ as.character(type)
    ),
    # Use a non-missing home-rule flag for year-by-home-rule fixed effects.
    home_rule_for_fe = replace_na(as.integer(home_rule_ind), 0L)
  )

agency_label <- function(x) {
  x <- as.character(x)

  dplyr::case_when(
    x == "HR_muni"     ~ "Home-rule municipalities",
    x == "NonHR_muni"  ~ "Non-home-rule municipalities",
    x == "Muni"        ~ "All Munis",
    x == "Other"       ~ "Other",
    x == "Township"    ~ "Townships",
    x == "School"      ~ "Schools",
    TRUE               ~ x
  )
}

row_label <- function(x) {
  x <- as.character(x)

  dplyr::case_when(
    x == "m1" ~ "No controls",
    x == "m2" ~ "Year FE",
    x == "m3" ~ "Year + unit FE",
    x == "m4" ~ "IG revenue + year + unit FE",
    x == "m5" ~ "Same sample as IG row, no IG control",
    x == "m6" ~ "Enrollment + year + unit FE",
    TRUE ~ x
  )
}

gov_types <- c("HR_muni", "NonHR_muni", "Other", "Township", "School")
gov_types_B <- c("HR_muni", "NonHR_muni", "School")
minor_types <- c("ELEMENTARY", "SECONDARY")

# Table 2. All-agency OLS, v22 specification ----

all_ols_1 <- feols(d_levy ~ d_eav,
  data = reg_df, cluster = vcov_uid)

all_ols_2 <- feols(d_levy ~ d_eav | year^home_rule_for_fe,
  data = reg_df, cluster = vcov_uid)

all_ols_3 <- feols(d_levy ~ d_eav | year^home_rule_for_fe + n_uniqueid,
  data = reg_df, cluster = vcov_uid)

all_ols_models_v22 <- list(M1 = all_ols_1, M2 = all_ols_2, M3 = all_ols_3)

all_ols_add_rows <- add_rows_from_named(
  all_ols_models_v22,
  rows = list(
    N = n_vec(all_ols_models_v22),
    upper_bound = ub_vec(all_ols_models_v22, "d_eav")
  ),
  digits = c(N = 0, upper_bound = 3)
)

## Table 2: All-agency OLS - export OLS all agencies ----
save_table(
  all_ols_models_v22,
  file_stub = "v22_table_02_OLS_all_agencies",
  title = "Table 2: All agencies OLS Predict levy using d_eav",
  notes = c(
    "Cook County, Illinois data from 2008 through 2023.",
    "Columns 2 and 3 include year-by-home-rule fixed effects; column 3 includes unit fixed effects.",
    "Standard errors are clustered by taxing agency. upper_bound is estimate + 1.645 * clustered SE."
  ),
  coef_map = c("d_eav" = "Change in EAV"),
  add_rows = all_ols_add_rows,
  gof_omit = ".*"
)

## Table 2: All-agency OLS - export OLS all agencies upper bounds ----
save_df_table(
  bind_rows(
    term_stats(all_ols_1, "d_eav", "M1"),
    term_stats(all_ols_2, "d_eav", "M2"),
    term_stats(all_ols_3, "d_eav", "M3")
  ) |>
    select(
      model, N,
      estimate = estimate_with_stars,
      std_error, p_value,
      upper_bound = upper_bound_with_details
    ),
  "v22_table_02_OLS_all_agencies_upper_bounds",
  "Table 2: Upper bounds for all-agency OLS"
)

# Table 3. All-agency IV, v22 specification ----

all_iv_1 <- feols(d_levy ~ 1 | 0 | d_eav ~ reassess_year,
  data = reg_df, cluster = vcov_uid)
summary(all_iv_1, stage = 1)
summary(all_iv_1, stage = 1:2)

all_iv_2 <- feols(d_levy ~ 1 | year^home_rule_for_fe | d_eav ~ reassess_year,
  data = reg_df, cluster = vcov_uid)

all_iv_3 <- feols(d_levy ~ 1 | year^home_rule_for_fe + n_uniqueid | d_eav ~ reassess_year,
  data = reg_df, cluster = vcov_uid)
summary(all_iv_3, stage = 1:2)

all_iv_models_v22 <- list(M1 = all_iv_1, M2 = all_iv_2, M3 = all_iv_3)

all_iv_p_exog <- c(
  M1 = cluster_p_exog_single(all_iv_1, reg_df, fe = "0"),
  M2 = cluster_p_exog_single(all_iv_2, reg_df, fe = "year^home_rule_for_fe"),
  M3 = cluster_p_exog_single(all_iv_3, reg_df, fe = "year^home_rule_for_fe + n_uniqueid")
)

all_iv_regF <- c(M1 = safe_first_stage_f(all_iv_1), M2 = safe_first_stage_f(all_iv_2), M3 = safe_first_stage_f(all_iv_3))
all_iv_ub <- c(M1 = term_stats(all_iv_1, "fit_d_eav")$upper_bound,
  M2 = term_stats(all_iv_2, "fit_d_eav")$upper_bound,
  M3 = term_stats(all_iv_3, "fit_d_eav")$upper_bound)

all_iv_add_rows <- add_rows_from_named(
  all_iv_models_v22,
  rows = list(
    N = n_vec(all_iv_models_v22),
    upper_bound = all_iv_ub,
    p_exog = all_iv_p_exog,
    regF = all_iv_regF
  ),
  digits = c(N = 0, upper_bound = 3, p_exog = 3, regF = 1)
)

## Table 3: All-agency IV - export IV all agencies ----
save_table(
  all_iv_models_v22,
  file_stub = "v22_table_03_IV_all_agencies",
  title = "Table 3: All agencies IV Predict levy using d_eav",
  notes = c(
    "Cook County, Illinois data from 2008 through 2023.",
    "Columns 2 and 3 include year-by-home-rule fixed effects; column 3 includes unit fixed effects.",
    "d_eav is treated as endogenous and instrumented by reassessment year.",
    "Standard errors are clustered by taxing agency. p_exog is a cluster-robust control-function test of the null that d_eav is exogenous. regF is the first-stage F statistic."
  ),
  coef_map = c("fit_d_eav" = "Change in EAV"),
  add_rows = all_iv_add_rows,
  gof_omit = ".*"
)

## Table 3: All-agency IV - export IV all agencies upper bounds ----
save_df_table(
  bind_rows(
    term_stats(all_iv_1, "fit_d_eav", "M1"),
    term_stats(all_iv_2, "fit_d_eav", "M2"),
    term_stats(all_iv_3, "fit_d_eav", "M3")
  ) |>
    mutate(first_stage_F = all_iv_regF, p_exog = all_iv_p_exog) |>
    select(
      model, N,
      estimate = estimate_with_stars,
      std_error, p_value,
      upper_bound = upper_bound_with_details,
      first_stage_F, p_exog
    ),
  "v22_table_03_IV_all_agencies_upper_bounds",
  "Table 3: Upper bounds for all-agency IV"
)

# Tables 4/A2 and 5/A3. By agency type: OLS + IV summary rows ----

run_type_v22 <- function(df, gov, iv = FALSE) {
  d <- df |> filter(type_2 == gov)
  if (!iv) {
    m1 <- feols(d_levy ~ d_eav, data = d, cluster = vcov_uid)
    m2 <- feols(d_levy ~ d_eav | year, data = d, cluster = vcov_uid)
    m3 <- feols(d_levy ~ d_eav | year + n_uniqueid, data = d, cluster = vcov_uid)
    return(list(m1 = m1, m2 = m2, m3 = m3))
  }
  m1 <- feols(d_levy ~ 1 | 0 | d_eav ~ reassess_year, data = d, cluster = vcov_uid)
  m2 <- feols(d_levy ~ 1 | year | d_eav ~ reassess_year, data = d, cluster = vcov_uid)
  m3 <- feols(d_levy ~ 1 | year + n_uniqueid | d_eav ~ reassess_year, data = d, cluster = vcov_uid)
  list(m1 = m1, m2 = m2, m3 = m3)
}

run_type_B_v22 <- function(df, gov, iv = FALSE) {
  d <- df |> filter(type_2 == gov)
  if (!iv) {
    m4 <- feols(d_levy ~ d_eav + d_total_ig_revenue | year + n_uniqueid, data = d, cluster = vcov_uid)
    idx <- sample_index(m4, d)
    m5 <- feols(d_levy ~ d_eav | year + n_uniqueid, data = d[idx, ], cluster = vcov_uid)
    return(list(m4 = m4, m5 = m5))
  }
  m4 <- feols(d_levy ~ d_total_ig_revenue | year + n_uniqueid | d_eav ~ reassess_year, data = d, cluster = vcov_uid)
  idx <- sample_index(m4, d)
  m5 <- feols(d_levy ~ 1 | year + n_uniqueid | d_eav ~ reassess_year, data = d[idx, ], cluster = vcov_uid)
  list(m4 = m4, m5 = m5)
}

school_extra_v22 <- function(df, iv = FALSE) {
  d <- df |> filter(type_2 == "School")
  if (!iv) {
    return(feols(d_levy ~ d_eav + d_enrollment | year + n_uniqueid, data = d, cluster = vcov_uid))
  }
  feols(d_levy ~ d_enrollment | year + n_uniqueid | d_eav ~ reassess_year, data = d, cluster = vcov_uid)
}

make_by_type_summary <- function(models_A, models_B, school_extra, iv = FALSE) {
  term <- if (iv) "fit_d_eav" else "d_eav"
  rows <- list()
  for (spec in paste0("m", 1:3)) {
    rows[[spec]] <- map_dfr(gov_types, function(gov) {
      term_stats(models_A[[gov]][[spec]], term, gov) |>
        transmute(row = spec, agency = model, N, upper_bound, estimate, std_error, p_value, estimate_with_stars, upper_bound_with_details)
    })
  }
  for (spec in paste0("m", 4:5)) {
    rows[[spec]] <- map_dfr(gov_types_B, function(gov) {
      term_stats(models_B[[gov]][[spec]], term, gov) |>
        transmute(row = spec, agency = model, N, upper_bound, estimate, std_error, p_value, estimate_with_stars, upper_bound_with_details)
    })
  }
  rows[["m6"]] <- term_stats(school_extra, term, "School") |>
    transmute(row = "m6", agency = model, N, upper_bound, estimate, std_error, p_value, estimate_with_stars, upper_bound_with_details)
  bind_rows(rows) |>
    mutate(
      agency = agency_label(agency),
      row = row_label(row)
    )
}

ols_type_A <- setNames(map(gov_types, ~ run_type_v22(reg_df, .x, iv = FALSE)), gov_types)
ols_type_B <- setNames(map(gov_types_B, ~ run_type_B_v22(reg_df, .x, iv = FALSE)), gov_types_B)
ols_school_extra <- school_extra_v22(reg_df, iv = FALSE)

iv_type_A <- setNames(map(gov_types, ~ run_type_v22(reg_df, .x, iv = TRUE)), gov_types)
iv_type_B <- setNames(map(gov_types_B, ~ run_type_B_v22(reg_df, .x, iv = TRUE)), gov_types_B)
iv_school_extra <- school_extra_v22(reg_df, iv = TRUE)

ols_by_type_summary <- make_by_type_summary(ols_type_A, ols_type_B, ols_school_extra, iv = FALSE)
iv_by_type_summary <- make_by_type_summary(iv_type_A, iv_type_B, iv_school_extra, iv = TRUE)


make_symmetric_iv_by_type_diagnostics <- function(
  reg_df,
  iv_type_A,
  iv_type_B,
  iv_school_extra
) {
  rows <- list()

  fe_A <- c(
    m1 = "0",
    m2 = "year",
    m3 = "year + n_uniqueid"
  )

  for (spec in names(fe_A)) {
    rows[[paste0("A_", spec)]] <- map_dfr(
      gov_types,
      function(gov) {
        d <- reg_df |>
          filter(type_2 == gov)

        m <- iv_type_A[[gov]][[spec]]

        tibble(
          row = row_label(spec),
          agency = agency_label(gov),
          N = model_n(m),
          first_stage_F = safe_first_stage_f(m),
          p_exog = cluster_p_exog_single(
            iv_model = m,
            data = d,
            fe = unname(fe_A[[spec]])
          )
        )
      }
    )
  }

  for (spec in c("m4", "m5")) {
    rows[[paste0("B_", spec)]] <- map_dfr(
      gov_types_B,
      function(gov) {
        d <- reg_df |>
          filter(type_2 == gov)

        m <- iv_type_B[[gov]][[spec]]

        if (spec == "m4") {
          d_use <- d
          controls <- "d_total_ig_revenue"
        } else {
          m4 <- iv_type_B[[gov]][["m4"]]
          idx <- sample_index(m4, d)
          d_use <- d[idx, , drop = FALSE]
          controls <- NULL
        }

        tibble(
          row = row_label(spec),
          agency = agency_label(gov),
          N = model_n(m),
          first_stage_F = safe_first_stage_f(m),
          p_exog = cluster_p_exog_single(
            iv_model = m,
            data = d_use,
            fe = "year + n_uniqueid",
            exog_controls = controls
          )
        )
      }
    )
  }

  d_school <- reg_df |>
    filter(type_2 == "School")

  rows[["m6"]] <- tibble(
    row = row_label("m6"),
    agency = agency_label("School"),
    N = model_n(iv_school_extra),
    first_stage_F = safe_first_stage_f(iv_school_extra),
    p_exog = cluster_p_exog_single(
      iv_model = iv_school_extra,
      data = d_school,
      fe = "year + n_uniqueid",
      exog_controls = "d_enrollment"
    )
  )

  bind_rows(rows)
}

iv_by_type_diagnostics <- make_symmetric_iv_by_type_diagnostics(
  reg_df = reg_df,
  iv_type_A = iv_type_A,
  iv_type_B = iv_type_B,
  iv_school_extra = iv_school_extra
)

iv_by_type_summary <- iv_by_type_summary |>
  left_join(
    iv_by_type_diagnostics |>
      select(
        row,
        agency,
        first_stage_F,
        p_exog
      ),
    by = c("row", "agency")
  )

## Table 4: OLS by agency type - export OLS by type upper bounds long ----
save_df_table(
  ols_by_type_summary |>
    select(row, agency, N, estimate = estimate_with_stars, std_error, p_value, upper_bound = upper_bound_with_details),
  "v22_table_04_OLS_by_type_upper_bounds_long",
  "Table 4: By agency type OLS upper-bound estimates"
)
## Table 4: OLS by agency type - export OLS by type upper bounds ----
save_df_table(
  make_wide_stat_table(ols_by_type_summary, upper_bound_with_details),
  "v22_table_04_OLS_by_type_upper_bounds",
  "Table 4: By agency type OLS upper-bound estimates"
)
## Table A2: OLS point estimates by agency type - export OLS by type point estimates ----
save_df_table(
  ols_by_type_summary |>
    select(row, agency, N, estimate = estimate_with_stars, std_error, p_value),
  "v22_table_A2_OLS_by_type_point_estimates",
  "Table A2: By agency type OLS point estimates"
)

## Table A2: OLS point estimates by agency type - export OLS by type point estimates wide ----
save_df_table(
  make_wide_point_table(
    ols_by_type_summary,
    estimate_with_stars
  ),
  "v22_table_A2_OLS_by_type_point_estimates_wide",
  "Table A2: By agency type OLS point estimates"
)
## Table 5: IV by agency type - export IV by type upper bounds long ----
save_df_table(
  iv_by_type_summary |>
    select(
      row,
      agency,
      N,
      estimate = estimate_with_stars,
      std_error,
      p_value,
      upper_bound = upper_bound_with_details,
      first_stage_F,
      p_exog
    ),
  "v22_table_05_IV_by_type_upper_bounds_long",
  "Table 5: By agency type IV upper-bound estimates"
)

## Table 5: IV by agency type - export C: IV by type diagnostics ----
save_df_table(
  iv_by_type_diagnostics,
  "v22_table_05_C_IV_by_type_diagnostics",
  "Table 5C: First-stage strength and endogeneity diagnostics by agency type",
  notes = c(
    "first_stage_F is the cluster-robust first-stage Wald statistic.",
    "p_exog is the cluster-robust control-function test of the null that d_eav is exogenous."
  )
)
## Table 5: IV by agency type - export IV by type upper bounds ----
save_df_table(
  make_wide_stat_table(iv_by_type_summary, upper_bound_with_details),
  "v22_table_05_IV_by_type_upper_bounds",
  "Table 5: By agency type IV upper-bound estimates"
)
## Table A3: IV point estimates by agency type - export IV by type point estimates ----
save_df_table(
  iv_by_type_summary |>
    select(row, agency, N, estimate = estimate_with_stars, std_error, p_value),
  "v22_table_A3_IV_by_type_point_estimates",
  "Table A3: By agency type IV point estimates"
)

## Table A3: IV point estimates by agency type - export IV by type point estimates wide ----
save_df_table(
  make_wide_point_table(
    iv_by_type_summary,
    estimate_with_stars
  ),
  "v22_table_A3_IV_by_type_point_estimates_wide",
  "Table A3: By agency type IV point estimates"
)

# Tables 6/A4/A5 and 7/A6/A7. Asymmetric OLS + IV ----

reg_df <- reg_df |>
  mutate(
    .asym_row_id = row_number(),
    eav_growth = if_else(d_eav > 0, 1, 0),
    pos_d_eav = d_eav * eav_growth, # pos_d_eav = d_eav if positive, otherwise 0
    neg_d_eav = d_eav * (1 - eav_growth) # neg_d_eav = d_eav if negative, otherwise 0
  )

# Reproduce the original generated instrument while preserving row alignment.
# fixest may drop rows from the first stage; use row IDs instead of assigning
# fitted values directly to the full data frame.
first_stage_asym_df <- reg_df |>
  filter(is.finite(d_eav), !is.na(reassess_year), !is.na(year))

first_stage_asym <- feols(d_eav ~ reassess_year | year, data = first_stage_asym_df)

# The first stage should correspond to the actual 2SLS specification, including# the same exogenous variables and fixed effects, and the fitted values need to be generated on exactly the observations entering that IV model.
first_stage_asym_noFE <- feols(d_eav ~ reassess_year, data = first_stage_asym_df)

first_stage_asym_yearFE <- feols(d_eav ~ reassess_year | year, data = first_stage_asym_df)

first_stage_asym_main <- feols(d_eav ~ reassess_year | year + n_uniqueid, data = first_stage_asym_df)

first_stage_asym_used <- first_stage_asym_df[fixest::obs(first_stage_asym), , drop = FALSE] |>
  mutate(d_eav_hat = fitted(first_stage_asym)) |>
  select(.asym_row_id, d_eav_hat)

reg_df <- reg_df |>
  left_join(first_stage_asym_used, by = ".asym_row_id") |>
  mutate(
    # pos_d_eav_hat = pos_d_eav * d_eav_hat,  # DFM had this
    # neg_d_eav_hat = neg_d_eav * d_eav_hat   # DFM would have had this, if he had done a negative change in EAV variable
    # NOTE: pos_d_eav_hat = pos_d_eav * d_eav_hat =  eav_growth * d_eav * d_eav_hat = d_eav * d_eav_hat when the tax base increased from the previous year, and is 0 otherwise
    # where EAV_growth is 1 if positive, 0 otherwise, pos_
    # pos_d_eav_hat = eav_growth * d_eav_hat, # after discussing with DFM, new stata way
    # neg_d_eav_hat = (1 - eav_growth) * d_eav_hat, # stata way   # technically negative or no change

    pos_d_eav_hat = if_else(d_eav_hat > 0, d_eav_hat, 0),
    neg_d_eav_hat = if_else(d_eav_hat <= 0, d_eav_hat, 0)   # technically negative or no change
  )


asym_ols_1 <- feols(d_levy ~ neg_d_eav + pos_d_eav, data = reg_df, cluster = vcov_uid)
asym_ols_2 <- feols(d_levy ~ neg_d_eav + pos_d_eav | year^home_rule_for_fe, data = reg_df, cluster = vcov_uid)
asym_ols_3 <- feols(d_levy ~ neg_d_eav + pos_d_eav | year^home_rule_for_fe + n_uniqueid, data = reg_df, cluster = vcov_uid)

asym_ols_models_v22 <- list(M1 = asym_ols_1, M2 = asym_ols_2, M3 = asym_ols_3)

asym_ols_p_equal <- c(M1 = p_equal_terms(asym_ols_1), M2 = p_equal_terms(asym_ols_2), M3 = p_equal_terms(asym_ols_3))
asym_ols_pos_ub <- c(M1 = term_stats(asym_ols_1, "pos_d_eav")$upper_bound,
  M2 = term_stats(asym_ols_2, "pos_d_eav")$upper_bound,
  M3 = term_stats(asym_ols_3, "pos_d_eav")$upper_bound)
asym_ols_neg_ub <- c(M1 = term_stats(asym_ols_1, "neg_d_eav")$upper_bound,
  M2 = term_stats(asym_ols_2, "neg_d_eav")$upper_bound,
  M3 = term_stats(asym_ols_3, "neg_d_eav")$upper_bound)

asym_ols_add_rows <- add_rows_from_named(
  asym_ols_models_v22,
  rows = list(
    N = n_vec(asym_ols_models_v22),
    p_equal = asym_ols_p_equal,
    pos_upper_bound = asym_ols_pos_ub,
    neg_upper_bound = asym_ols_neg_ub
  ),
  digits = c(N = 0, p_equal = 3, pos_upper_bound = 3, neg_upper_bound = 3)
)

## Table 6: All-agency asymmetric OLS - export OLS all agencies asym ----
save_table(
  asym_ols_models_v22,
  file_stub = "v22_table_06_OLS_all_agencies_asym",
  title = "Table 6: All agencies OLS allowing for asymmetry",
  notes = c("Standard errors are clustered by taxing agency.",
    "p_equal tests equality of the positive- and negative-EAV coefficients.",
    "pos_upper_bound and neg_upper_bound equal the corresponding coefficient plus 1.645 times the clustered SE. neg_d_eav is coded as the actual negative value of d_eav when EAV declines."),
  coef_map = c("pos_d_eav" = "Positive EAV change", "neg_d_eav" = "Negative EAV change"),
  add_rows = asym_ols_add_rows,
  gof_omit = ".*"
)

asym_ols_all_pos_neg_bounds <- bind_rows(
  asym_component_stats(asym_ols_1, "M1", iv = FALSE),
  asym_component_stats(asym_ols_2, "M2", iv = FALSE),
  asym_component_stats(asym_ols_3, "M3", iv = FALSE)
) |>
  mutate(p_equal = asym_ols_p_equal[model]) |>
  select(model, component, N, estimate = estimate_with_stars, std_error, p_value, upper_bound = upper_bound_with_details, p_equal)

## Table 6: All-agency asymmetric OLS - export OLS all agencies asym upper bounds ----
save_df_table(
  asym_ols_all_pos_neg_bounds,
  "v22_table_06_OLS_all_agencies_asym_upper_bounds",
  "Table 6: Upper bounds for positive and negative EAV changes"
)

## Table 6: All-agency asymmetric OLS - export B: OLS all agencies asym pos neg upper bounds ----
save_df_table(
  asym_ols_all_pos_neg_bounds,
  "v22_table_06_B_OLS_all_agencies_asym_pos_neg_upper_bounds",
  "Table 6B: OLS upper bounds for positive and negative EAV changes"
)

# Formula: y ~ exogenous | placeholder | endogenous ~ instrument
# iv_model <- feols(y ~ x_exog | 1 | x_endog ~ z, data = your_data)

asym_iv_1 <- feols(d_levy ~ 1 | 0 | neg_d_eav + pos_d_eav ~ reassess_year + pos_d_eav_hat,
  data = reg_df, cluster = vcov_uid)
asym_iv_2 <- feols(d_levy ~ 1 | year^home_rule_for_fe | neg_d_eav + pos_d_eav ~ reassess_year + pos_d_eav_hat,
  data = reg_df, cluster = vcov_uid)
asym_iv_3 <- feols(d_levy ~ 1 | year^home_rule_for_fe + n_uniqueid | neg_d_eav + pos_d_eav ~ reassess_year + pos_d_eav_hat,
  data = reg_df, cluster = vcov_uid)

# tried to model assymetric IV model another way. to be continued
# all_asym_iv_3 <- feols(d_levy ~ 1 | year^home_rule_for_fe + n_uniqueid | pos_d_eav + neg_d_eav  ~ reassess_year + pos_d_eav,
#                   data = reg_df, cluster = vcov_uid)
# summary(all_asym_iv_3, stage = 1:2)



asym_iv_models_v22 <- list(M1 = asym_iv_1, M2 = asym_iv_2, M3 = asym_iv_3)

asym_iv_p_equal <- c(
  M1 = p_equal_terms(asym_iv_1, "fit_neg_d_eav", "fit_pos_d_eav"),
  M2 = p_equal_terms(asym_iv_2, "fit_neg_d_eav", "fit_pos_d_eav"),
  M3 = p_equal_terms(asym_iv_3, "fit_neg_d_eav", "fit_pos_d_eav")
)
asym_iv_instruments <- c("reassess_year", "pos_d_eav_hat")

asym_iv_p_exog <- c(
  M1 = cluster_p_exog_asym(asym_iv_1, reg_df, fe = "0", instruments = asym_iv_instruments),
  M2 = cluster_p_exog_asym(asym_iv_2, reg_df, fe = "year^home_rule_for_fe", instruments = asym_iv_instruments),
  M3 = cluster_p_exog_asym(asym_iv_3, reg_df, fe = "year^home_rule_for_fe + n_uniqueid", instruments = asym_iv_instruments)
)
asym_iv_pos_ub <- c(M1 = term_stats(asym_iv_1, "fit_pos_d_eav")$upper_bound,
  M2 = term_stats(asym_iv_2, "fit_pos_d_eav")$upper_bound,
  M3 = term_stats(asym_iv_3, "fit_pos_d_eav")$upper_bound)
asym_iv_neg_ub <- c(M1 = term_stats(asym_iv_1, "fit_neg_d_eav")$upper_bound,
  M2 = term_stats(asym_iv_2, "fit_neg_d_eav")$upper_bound,
  M3 = term_stats(asym_iv_3, "fit_neg_d_eav")$upper_bound)

# Do not pull F_pos/F_neg from the second numeric element of fitstat("ivf1").
# That can be a p-value, which is why the old output showed F_pos = 0.0.
# Instead, estimate the two first stages explicitly on the IV model sample.
asym_iv_F_neg <- c(
  M1 = first_stage_F_for(asym_iv_1, reg_df, endog = "neg_d_eav", instruments = asym_iv_instruments, fe = "0"),
  M2 = first_stage_F_for(asym_iv_2, reg_df, endog = "neg_d_eav", instruments = asym_iv_instruments, fe = "year^home_rule_for_fe"),
  M3 = first_stage_F_for(asym_iv_3, reg_df, endog = "neg_d_eav", instruments = asym_iv_instruments, fe = "year^home_rule_for_fe + n_uniqueid")
)
asym_iv_F_pos <- c(
  M1 = first_stage_F_for(asym_iv_1, reg_df, endog = "pos_d_eav", instruments = asym_iv_instruments, fe = "0"),
  M2 = first_stage_F_for(asym_iv_2, reg_df, endog = "pos_d_eav", instruments = asym_iv_instruments, fe = "year^home_rule_for_fe"),
  M3 = first_stage_F_for(asym_iv_3, reg_df, endog = "pos_d_eav", instruments = asym_iv_instruments, fe = "year^home_rule_for_fe + n_uniqueid")
)


asym_iv_SW <- list(
  M1 = sw_conditional_f(
    asym_iv_1,
    reg_df,
    fe = "0",
    instruments = asym_iv_instruments
  ),
  M2 = sw_conditional_f(
    asym_iv_2,
    reg_df,
    fe = "year^home_rule_for_fe",
    instruments = asym_iv_instruments
  ),
  M3 = sw_conditional_f(
    asym_iv_3,
    reg_df,
    fe = "year^home_rule_for_fe + n_uniqueid",
    instruments = asym_iv_instruments
  )
)

asym_iv_SW_neg <- vapply(
  asym_iv_SW,
  function(x) x[["SW_F_neg"]],
  numeric(1)
)

asym_iv_SW_pos <- vapply(
  asym_iv_SW,
  function(x) x[["SW_F_pos"]],
  numeric(1)
)

asym_iv_add_rows <- add_rows_from_named(
  asym_iv_models_v22,
  rows = list(
    N = n_vec(asym_iv_models_v22),
    F_neg = asym_iv_F_neg,
    F_pos = asym_iv_F_pos,
    SW_F_neg = asym_iv_SW_neg,
    SW_F_pos = asym_iv_SW_pos,
    p_exog = asym_iv_p_exog,
    p_equal = asym_iv_p_equal,
    pos_upper_bound = asym_iv_pos_ub,
    neg_upper_bound = asym_iv_neg_ub
  ),
  digits = c(
    N = 0,
    F_neg = 1,
    F_pos = 1,
    SW_F_neg = 1,
    SW_F_pos = 1,
    p_exog = 3,
    p_equal = 3,
    pos_upper_bound = 3,
    neg_upper_bound = 3
  )
)

## Table 7: All-agency asymmetric IV - export IV all agencies asym ----
save_table(
  asym_iv_models_v22,
  file_stub = "v22_table_07_IV_all_agencies_asym",
  title = "Table 7: All agencies IV allowing for asymmetry",
  notes = c("Standard errors are clustered by taxing agency.",
    "F_neg and F_pos are equation-specific, cluster-robust first-stage Wald statistics.",
    "SW_F_neg and SW_F_pos are Sanderson-Windmeijer conditional F statistics for the two endogenous variables.",
    "p_exog is a cluster-robust joint control-function test that the positive and negative EAV variables are exogenous.",
    "p_equal tests equality of the positive- and negative-EAV coefficients.",
    "pos_upper_bound and neg_upper_bound equal the corresponding coefficient plus 1.645 times the clustered SE. neg_d_eav is coded as the actual negative value of d_eav when EAV declines."),
  coef_map = c("fit_pos_d_eav" = "Positive EAV change", "fit_neg_d_eav" = "Negative EAV change"),
  add_rows = asym_iv_add_rows,
  gof_omit = ".*"
)

asym_iv_all_pos_neg_bounds <- bind_rows(
  asym_component_stats(asym_iv_1, "M1", iv = TRUE),
  asym_component_stats(asym_iv_2, "M2", iv = TRUE),
  asym_component_stats(asym_iv_3, "M3", iv = TRUE)
) |>
  mutate(
    p_equal = asym_iv_p_equal[model],
    p_exog = asym_iv_p_exog[model],
    F_neg = asym_iv_F_neg[model],
    F_pos = asym_iv_F_pos[model],
    SW_F_neg = asym_iv_SW_neg[model],
    SW_F_pos = asym_iv_SW_pos[model]
  ) |>
  select(
    model,
    component,
    N,
    estimate = estimate_with_stars,
    std_error,
    p_value,
    upper_bound = upper_bound_with_details,
    F_neg,
    F_pos,
    SW_F_neg,
    SW_F_pos,
    p_exog,
    p_equal
  )

## Table 7: All-agency asymmetric IV - export IV all agencies asym upper bounds ----
save_df_table(
  asym_iv_all_pos_neg_bounds,
  "v22_table_07_IV_all_agencies_asym_upper_bounds",
  "Table 7: Upper bounds for positive and negative EAV changes, IV"
)

## Table 7: All-agency asymmetric IV - export B: IV all agencies asym pos neg upper bounds ----
save_df_table(
  asym_iv_all_pos_neg_bounds,
  "v22_table_07_B_IV_all_agencies_asym_pos_neg_upper_bounds",
  "Table 7B: IV upper bounds for positive and negative EAV changes"
)

## Table 7: All-agency asymmetric IV - export C: IV all agencies asym first stage ----
save_df_table(
  asym_iv_all_pos_neg_bounds |>
    distinct(
      model,
      N,
      F_neg,
      F_pos,
      SW_F_neg,
      SW_F_pos,
      p_exog
    ),
  "v22_table_07_C_IV_all_agencies_asym_first_stage",
  "Table 7C: First-stage and conditional weak-identification diagnostics for asymmetric IV models"
)

# Alternative generated-instrument check that uses neg_d_eav_hat rather than
# pos_d_eav_hat as the second excluded instrument. Treat as robustness output.
asym_iv_alt_instruments <- c("reassess_year", "neg_d_eav_hat")
asym_iv_alt_1 <- feols(d_levy ~ 1 | 0 | neg_d_eav + pos_d_eav ~ reassess_year + neg_d_eav_hat,
  data = reg_df, cluster = vcov_uid)
asym_iv_alt_2 <- feols(d_levy ~ 1 | year^home_rule_for_fe | neg_d_eav + pos_d_eav ~ reassess_year + neg_d_eav_hat,
  data = reg_df, cluster = vcov_uid)
asym_iv_alt_3 <- feols(d_levy ~ 1 | year^home_rule_for_fe + n_uniqueid | neg_d_eav + pos_d_eav ~ reassess_year + neg_d_eav_hat,
  data = reg_df, cluster = vcov_uid)

asym_iv_alt_models_v22 <- list(M1 = asym_iv_alt_1, M2 = asym_iv_alt_2, M3 = asym_iv_alt_3)
asym_iv_alt_p_equal <- c(
  M1 = p_equal_terms(asym_iv_alt_1, "fit_neg_d_eav", "fit_pos_d_eav"),
  M2 = p_equal_terms(asym_iv_alt_2, "fit_neg_d_eav", "fit_pos_d_eav"),
  M3 = p_equal_terms(asym_iv_alt_3, "fit_neg_d_eav", "fit_pos_d_eav")
)
asym_iv_alt_p_exog <- c(
  M1 = cluster_p_exog_asym(asym_iv_alt_1, reg_df, fe = "0", instruments = asym_iv_alt_instruments),
  M2 = cluster_p_exog_asym(asym_iv_alt_2, reg_df, fe = "year^home_rule_for_fe", instruments = asym_iv_alt_instruments),
  M3 = cluster_p_exog_asym(asym_iv_alt_3, reg_df, fe = "year^home_rule_for_fe + n_uniqueid", instruments = asym_iv_alt_instruments)
)
asym_iv_alt_F_neg <- c(
  M1 = first_stage_F_for(asym_iv_alt_1, reg_df, endog = "neg_d_eav", instruments = asym_iv_alt_instruments, fe = "0"),
  M2 = first_stage_F_for(asym_iv_alt_2, reg_df, endog = "neg_d_eav", instruments = asym_iv_alt_instruments, fe = "year^home_rule_for_fe"),
  M3 = first_stage_F_for(asym_iv_alt_3, reg_df, endog = "neg_d_eav", instruments = asym_iv_alt_instruments, fe = "year^home_rule_for_fe + n_uniqueid")
)
asym_iv_alt_F_pos <- c(
  M1 = first_stage_F_for(asym_iv_alt_1, reg_df, endog = "pos_d_eav", instruments = asym_iv_alt_instruments, fe = "0"),
  M2 = first_stage_F_for(asym_iv_alt_2, reg_df, endog = "pos_d_eav", instruments = asym_iv_alt_instruments, fe = "year^home_rule_for_fe"),
  M3 = first_stage_F_for(asym_iv_alt_3, reg_df, endog = "pos_d_eav", instruments = asym_iv_alt_instruments, fe = "year^home_rule_for_fe + n_uniqueid")
)


asym_iv_alt_SW <- list(
  M1 = sw_conditional_f(
    asym_iv_alt_1,
    reg_df,
    fe = "0",
    instruments = asym_iv_alt_instruments
  ),
  M2 = sw_conditional_f(
    asym_iv_alt_2,
    reg_df,
    fe = "year^home_rule_for_fe",
    instruments = asym_iv_alt_instruments
  ),
  M3 = sw_conditional_f(
    asym_iv_alt_3,
    reg_df,
    fe = "year^home_rule_for_fe + n_uniqueid",
    instruments = asym_iv_alt_instruments
  )
)

asym_iv_alt_SW_neg <- vapply(
  asym_iv_alt_SW,
  function(x) x[["SW_F_neg"]],
  numeric(1)
)

asym_iv_alt_SW_pos <- vapply(
  asym_iv_alt_SW,
  function(x) x[["SW_F_pos"]],
  numeric(1)
)

## Table 7: All-agency asymmetric IV - export D: IV all agencies asym alt neg instrument pos neg upper bounds ----
save_df_table(
  bind_rows(
    asym_component_stats(asym_iv_alt_1, "M1", iv = TRUE),
    asym_component_stats(asym_iv_alt_2, "M2", iv = TRUE),
    asym_component_stats(asym_iv_alt_3, "M3", iv = TRUE)
  ) |>
    mutate(
      p_equal = asym_iv_alt_p_equal[model],
      p_exog = asym_iv_alt_p_exog[model],
      F_neg = asym_iv_alt_F_neg[model],
      F_pos = asym_iv_alt_F_pos[model],
      SW_F_neg = asym_iv_alt_SW_neg[model],
      SW_F_pos = asym_iv_alt_SW_pos[model]
    ) |>
    select(
      model,
      component,
      N,
      estimate = estimate_with_stars,
      std_error,
      p_value,
      upper_bound = upper_bound_with_details,
      F_neg,
      F_pos,
      SW_F_neg,
      SW_F_pos,
      p_exog,
      p_equal
    ),
  "v22_table_07_D_IV_all_agencies_asym_alt_neg_instrument_pos_neg_upper_bounds",
  "Table 7D: Alternative asymmetric IV upper bounds using neg_d_eav_hat"
)


# Main and diagnostic models continue to cluster standard errors by taxing
# agency through vcov_uid. reassess_year remains the excluded IV indicator.

run_asym_type_v22 <- function(df, gov, iv = FALSE) {
  d <- df |> filter(type_2 == gov)
  if (!iv) {
    m1 <- feols(d_levy ~ neg_d_eav + pos_d_eav, data = d, cluster = vcov_uid)
    m2 <- feols(d_levy ~ neg_d_eav + pos_d_eav | year, data = d, cluster = vcov_uid)
    m3 <- feols(d_levy ~ neg_d_eav + pos_d_eav | year + n_uniqueid, data = d, cluster = vcov_uid)
    return(list(m1 = m1, m2 = m2, m3 = m3))
  }
  m1 <- feols(d_levy ~ 1 | 0 | neg_d_eav + pos_d_eav ~ reassess_year + pos_d_eav_hat, data = d, cluster = vcov_uid)
  m2 <- feols(d_levy ~ 1 | year | neg_d_eav + pos_d_eav ~ reassess_year + pos_d_eav_hat, data = d, cluster = vcov_uid)
  m3 <- feols(d_levy ~ 1 | year + n_uniqueid | neg_d_eav + pos_d_eav ~ reassess_year + pos_d_eav_hat, data = d, cluster = vcov_uid)
  list(m1 = m1, m2 = m2, m3 = m3)
}

run_asym_type_B_v22 <- function(df, gov, iv = FALSE) {
  d <- df |> filter(type_2 == gov)
  if (!iv) {
    m4 <- feols(d_levy ~ neg_d_eav + pos_d_eav + d_total_ig_revenue | year + n_uniqueid, data = d, cluster = vcov_uid)
    idx <- sample_index(m4, d)
    m5 <- feols(d_levy ~ neg_d_eav + pos_d_eav | year + n_uniqueid, data = d[idx, ], cluster = vcov_uid)
    return(list(m4 = m4, m5 = m5))
  }
  m4 <- feols(d_levy ~ d_total_ig_revenue | year + n_uniqueid | neg_d_eav + pos_d_eav ~ reassess_year + pos_d_eav_hat, data = d, cluster = vcov_uid)
  idx <- sample_index(m4, d)
  m5 <- feols(d_levy ~ 1 | year + n_uniqueid | neg_d_eav + pos_d_eav ~ reassess_year + pos_d_eav_hat, data = d[idx, ], cluster = vcov_uid)
  list(m4 = m4, m5 = m5)
}

asym_school_extra_v22 <- function(df, iv = FALSE) {
  d <- df |> filter(type_2 == "School")
  if (!iv) {
    return(feols(d_levy ~ neg_d_eav + pos_d_eav + d_enrollment | year + n_uniqueid, data = d, cluster = vcov_uid))
  }
  feols(d_levy ~ d_enrollment | year + n_uniqueid | neg_d_eav + pos_d_eav ~ reassess_year + pos_d_eav_hat, data = d, cluster = vcov_uid)
}

make_asym_by_type_summary <- function(models_A, models_B, school_extra, iv = FALSE) {
  term_pos <- if (iv) "fit_pos_d_eav" else "pos_d_eav"
  term_neg <- if (iv) "fit_neg_d_eav" else "neg_d_eav"
  rows <- list()
  for (spec in paste0("m", 1:3)) {
    rows[[spec]] <- map_dfr(gov_types, function(gov) {
      m <- models_A[[gov]][[spec]]
      term_stats(m, term_pos, gov) |>
        transmute(row = spec, agency = model, N, upper_bound, estimate, std_error, p_value, estimate_with_stars, upper_bound_with_details, p_equal = p_equal_terms(m, term_neg, term_pos))
    })
  }
  for (spec in paste0("m", 4:5)) {
    rows[[spec]] <- map_dfr(gov_types_B, function(gov) {
      m <- models_B[[gov]][[spec]]
      term_stats(m, term_pos, gov) |>
        transmute(row = spec, agency = model, N, upper_bound, estimate, std_error, p_value, estimate_with_stars, upper_bound_with_details, p_equal = p_equal_terms(m, term_neg, term_pos))
    })
  }
  rows[["m6"]] <- term_stats(school_extra, term_pos, "School") |>
    transmute(row = "m6", agency = model, N, upper_bound, estimate, std_error, p_value, estimate_with_stars, upper_bound_with_details, p_equal = p_equal_terms(school_extra, term_neg, term_pos))
  bind_rows(rows) |>
    mutate(
      agency = agency_label(agency),
      row = row_label(row)
    )
}

asym_ols_type_A <- setNames(map(gov_types, ~ run_asym_type_v22(reg_df, .x, iv = FALSE)), gov_types)
asym_ols_type_B <- setNames(map(gov_types_B, ~ run_asym_type_B_v22(reg_df, .x, iv = FALSE)), gov_types_B)
asym_ols_school_extra <- asym_school_extra_v22(reg_df, iv = FALSE)

asym_iv_type_A <- setNames(map(gov_types, ~ run_asym_type_v22(reg_df, .x, iv = TRUE)), gov_types)
asym_iv_type_B <- setNames(map(gov_types_B, ~ run_asym_type_B_v22(reg_df, .x, iv = TRUE)), gov_types_B)
asym_iv_school_extra <- asym_school_extra_v22(reg_df, iv = TRUE)

asym_ols_by_type_summary <- make_asym_by_type_summary(asym_ols_type_A, asym_ols_type_B, asym_ols_school_extra, iv = FALSE)
asym_iv_by_type_summary <- make_asym_by_type_summary(asym_iv_type_A, asym_iv_type_B, asym_iv_school_extra, iv = TRUE)

## Table 8: Asymmetric OLS by agency type - export OLS by type asym upper bounds long ----
save_df_table(
  asym_ols_by_type_summary |>
    select(row, agency, N, estimate = estimate_with_stars, std_error, p_value, upper_bound = upper_bound_with_details, p_equal),
  "v22_table_08_OLS_by_type_asym_upper_bounds_long",
  "Table 8: OLS upper-bound estimates allowing for asymmetry"
)
## Table 8: Asymmetric OLS by agency type - export OLS by type asym upper bounds ----
save_df_table(
  make_wide_stat_table(asym_ols_by_type_summary, upper_bound_with_details),
  "v22_table_08_OLS_by_type_asym_upper_bounds",
  "Table 8: OLS upper-bound estimates allowing for asymmetry"
)
## Table A4: OLS symmetry tests - export OLS by type asym p equal ----
save_df_table(asym_ols_by_type_summary |> select(row, agency, p_equal), "v22_table_A4_OLS_by_type_asym_p_equal", "Table A4: P-values for OLS symmetry tests")
## Table A5: Asymmetric OLS point estimates - export OLS by type asym point estimates ----
save_df_table(
  asym_ols_by_type_summary |>
    select(row, agency, N, estimate = estimate_with_stars, std_error, p_value),
  "v22_table_A5_OLS_by_type_asym_point_estimates",
  "Table A5: OLS point estimates for positive EAV changes"
)

## Table A5: Asymmetric OLS point estimates - export OLS by type asym point estimates wide ----
save_df_table(
  make_wide_point_table(
    asym_ols_by_type_summary,
    estimate_with_stars
  ),
  "v22_table_A5_OLS_by_type_asym_point_estimates_wide",
  "Table A5: OLS point estimates for positive EAV changes"
)

## Table 9: Asymmetric IV by agency type - export IV by type asym upper bounds long ----
save_df_table(
  asym_iv_by_type_summary |>
    select(row, agency, N, estimate = estimate_with_stars, std_error, p_value, upper_bound = upper_bound_with_details, p_equal),
  "v22_table_09_IV_by_type_asym_upper_bounds_long",
  "Table 9: IV upper-bound estimates allowing for asymmetry"
)
## Table 9: Asymmetric IV by agency type - export IV by type asym upper bounds ----
save_df_table(
  make_wide_stat_table(asym_iv_by_type_summary, upper_bound_with_details),
  "v22_table_09_IV_by_type_asym_upper_bounds",
  "Table 9: IV upper-bound estimates allowing for asymmetry"
)
## Table A6: IV symmetry tests - export IV by type asym p equal ----
save_df_table(asym_iv_by_type_summary |> select(row, agency, p_equal), "v22_table_A6_IV_by_type_asym_p_equal", "Table A6: P-values for IV symmetry tests")
## Table A7: Asymmetric IV point estimates - export IV by type asym point estimates ----
save_df_table(
  asym_iv_by_type_summary |>
    select(row, agency, N, estimate = estimate_with_stars, std_error, p_value),
  "v22_table_A7_IV_by_type_asym_point_estimates",
  "Table A7: IV point estimates for positive EAV changes"
)

## Table A7: Asymmetric IV point estimates - export IV by type asym point estimates wide ----
save_df_table(
  make_wide_point_table(
    asym_iv_by_type_summary,
    estimate_with_stars
  ),
  "v22_table_A7_IV_by_type_asym_point_estimates_wide",
  "Table A7: IV point estimates for positive EAV changes"
)


# Additional asymmetric outputs: positive and negative EAV upper bounds ----
# These keep the original Table 8/9 files intact but add companion B/C/D files
# that show upper bounds for both sides of the asymmetric specification.

fit_asym_spec <- function(df, spec, iv = FALSE,
                          instruments = c("reassess_year", "pos_d_eav_hat")) {
  data_use <- df
  fe <- dplyr::case_when(
    spec == "m1" ~ "0",
    spec == "m2" ~ "year",
    spec %in% c("m3", "m4", "m5", "m6") ~ "year + n_uniqueid",
    TRUE ~ "0"
  )
  exog_controls <- NULL

  if (spec == "m4") {
    exog_controls <- "d_total_ig_revenue"
  } else if (spec == "m6") {
    exog_controls <- "d_enrollment"
  } else if (spec == "m5") {
    # Same sample as the IG row, but without the IG control.
    if (!iv) {
      m4_tmp <- feols(d_levy ~ neg_d_eav + pos_d_eav + d_total_ig_revenue | year + n_uniqueid,
        data = df, cluster = vcov_uid)
    } else {
      m4_tmp <- feols(as.formula(paste0(
        "d_levy ~ d_total_ig_revenue | year + n_uniqueid | neg_d_eav + pos_d_eav ~ ",
        paste(instruments, collapse = " + ")
      )), data = df, cluster = vcov_uid)
    }
    data_use <- df[sample_index(m4_tmp, df), , drop = FALSE]
  }

  if (!iv) {
    rhs <- paste(c("neg_d_eav", "pos_d_eav", exog_controls), collapse = " + ")
    fml <- if (fe == "0") {
      as.formula(paste0("d_levy ~ ", rhs))
    } else {
      as.formula(paste0("d_levy ~ ", rhs, " | ", fe))
    }
    model <- feols(fml, data = data_use, cluster = vcov_uid)
  } else {
    first_part <- if (is.null(exog_controls)) "1" else paste(exog_controls, collapse = " + ")
    fml <- as.formula(paste0(
      "d_levy ~ ", first_part, " | ", fe,
      " | neg_d_eav + pos_d_eav ~ ", paste(instruments, collapse = " + ")
    ))
    model <- feols(fml, data = data_use, cluster = vcov_uid)
  }

  list(
    model = model,
    data = data_use,
    fe = fe,
    exog_controls = exog_controls,
    instruments = instruments
  )
}

add_asym_iv_diagnostics <- function(out, iv_fit, fit, instruments) {
  # Compute model diagnostics before mutate(). The summary data frame `out`
  # contains a character column named `model`; evaluating `model` inside
  # mutate() would therefore pass that character column instead of the fitted
  # fixest object to fixest::obs().
  F_neg_value <- first_stage_F_for(
    iv_model = iv_fit,
    data = fit$data,
    endog = "neg_d_eav",
    instruments = instruments,
    fe = fit$fe,
    exog_controls = fit$exog_controls
  )

  F_pos_value <- first_stage_F_for(
    iv_model = iv_fit,
    data = fit$data,
    endog = "pos_d_eav",
    instruments = instruments,
    fe = fit$fe,
    exog_controls = fit$exog_controls
  )

  sw <- sw_conditional_f(
    iv_model = iv_fit,
    data = fit$data,
    fe = fit$fe,
    exog_controls = fit$exog_controls,
    instruments = instruments
  )

  p_exog_value <- cluster_p_exog_asym(
    iv_model = iv_fit,
    data = fit$data,
    instruments = instruments,
    exog_controls = fit$exog_controls,
    fe = fit$fe
  )

  out |>
    mutate(
      F_neg = F_neg_value,
      F_pos = F_pos_value,
      SW_F_neg = unname(sw[["SW_F_neg"]]),
      SW_F_pos = unname(sw[["SW_F_pos"]]),
      p_exog = p_exog_value
    )
}


make_asym_by_type_pos_neg_summary <- function(df, iv = FALSE,
                                              instruments = c("reassess_year", "pos_d_eav_hat")) {
  rows <- list()

  for (spec in paste0("m", 1:3)) {
    rows[[spec]] <- map_dfr(gov_types, function(gov) {
      d <- df |> filter(type_2 == gov)
      fit <- fit_asym_spec(d, spec, iv = iv, instruments = instruments)
      m <- fit$model

      out <- asym_component_stats(m, gov, iv = iv) |>
        mutate(
          row = spec,
          p_equal = p_equal_terms(m,
            lhs = if (iv) "fit_neg_d_eav" else "neg_d_eav",
            rhs = if (iv) "fit_pos_d_eav" else "pos_d_eav")
        )

      if (iv) {
        out <- add_asym_iv_diagnostics(
          out = out,
          iv_fit = m,
          fit = fit,
          instruments = instruments
        )
      }

      out
    })
  }

  for (spec in paste0("m", 4:5)) {
    rows[[spec]] <- map_dfr(gov_types_B, function(gov) {
      d <- df |> filter(type_2 == gov)
      fit <- fit_asym_spec(d, spec, iv = iv, instruments = instruments)
      m <- fit$model

      out <- asym_component_stats(m, gov, iv = iv) |>
        mutate(
          row = spec,
          p_equal = p_equal_terms(m,
            lhs = if (iv) "fit_neg_d_eav" else "neg_d_eav",
            rhs = if (iv) "fit_pos_d_eav" else "pos_d_eav")
        )

      if (iv) {
        out <- add_asym_iv_diagnostics(
          out = out,
          iv_fit = m,
          fit = fit,
          instruments = instruments
        )
      }

      out
    })
  }

  rows[["m6"]] <- {
    d <- df |> filter(type_2 == "School")
    fit <- fit_asym_spec(d, "m6", iv = iv, instruments = instruments)
    m <- fit$model

    out <- asym_component_stats(m, "School", iv = iv) |>
      mutate(
        row = "m6",
        p_equal = p_equal_terms(m,
          lhs = if (iv) "fit_neg_d_eav" else "neg_d_eav",
          rhs = if (iv) "fit_pos_d_eav" else "pos_d_eav")
      )

    if (iv) {
      out <- add_asym_iv_diagnostics(
        out = out,
        iv_fit = m,
        fit = fit,
        instruments = instruments
      )
    }

    out
  }

  bind_rows(rows) |>
    mutate(
      agency = agency_label(model),
      row = row_label(row)
    ) |>
    select(
      row, agency, component, N, estimate, std_error, p_value, upper_bound,
      estimate_with_stars, upper_bound_with_details, p_equal,
      everything(), -model
    )
}

asym_ols_by_type_pos_neg_summary <- make_asym_by_type_pos_neg_summary(reg_df, iv = FALSE)
asym_iv_by_type_pos_neg_summary <- make_asym_by_type_pos_neg_summary(reg_df, iv = TRUE, instruments = asym_iv_instruments)
asym_iv_by_type_pos_neg_alt_summary <- make_asym_by_type_pos_neg_summary(reg_df, iv = TRUE, instruments = asym_iv_alt_instruments)

## Table 8: Asymmetric OLS by agency type - export B: OLS by type asym pos neg upper bounds long ----
save_df_table(
  asym_ols_by_type_pos_neg_summary |>
    select(row, agency, component, N, estimate = estimate_with_stars, std_error, p_value, upper_bound = upper_bound_with_details, p_equal),
  "v22_table_08_B_OLS_by_type_asym_pos_neg_upper_bounds_long",
  "Table 8B: OLS upper bounds for positive and negative EAV changes by agency type"
)
## Table 8: Asymmetric OLS by agency type - export B: OLS by type asym pos neg upper bounds ----
save_df_table(
  make_wide_component_stat_table(asym_ols_by_type_pos_neg_summary, upper_bound_with_details),
  "v22_table_08_B_OLS_by_type_asym_pos_neg_upper_bounds",
  "Table 8B: OLS upper bounds for positive and negative EAV changes by agency type"
)
## Table A5: Asymmetric OLS point estimates - export B: OLS by type asym point estimates pos neg ----
save_df_table(
  asym_ols_by_type_pos_neg_summary |>
    select(row, agency, component, N, estimate = estimate_with_stars, std_error, p_value, upper_bound = upper_bound_with_details, p_equal),
  "v22_table_A5_B_OLS_by_type_asym_point_estimates_pos_neg",
  "Table A5B: OLS asymmetric point estimates for positive and negative EAV changes"
)

## Table A5: Asymmetric OLS point estimates - export B: OLS by type asym point estimates pos neg wide ----
save_df_table(
  make_wide_component_point_table(
    asym_ols_by_type_pos_neg_summary,
    estimate_with_stars
  ),
  "v22_table_A5_B_OLS_by_type_asym_point_estimates_pos_neg_wide",
  paste(
    "Table A5B: OLS asymmetric point estimates",
    "for positive and negative EAV changes"
  )
)
## Table 9: Asymmetric IV by agency type - export B: IV by type asym pos neg upper bounds long ----
save_df_table(
  asym_iv_by_type_pos_neg_summary |>
    select(
      row,
      agency,
      component,
      N,
      estimate = estimate_with_stars,
      std_error,
      p_value,
      upper_bound = upper_bound_with_details,
      F_neg,
      F_pos,
      SW_F_neg,
      SW_F_pos,
      p_exog,
      p_equal
    ),
  "v22_table_09_B_IV_by_type_asym_pos_neg_upper_bounds_long",
  "Table 9B: IV upper bounds for positive and negative EAV changes by agency type"
)
## Table 9: Asymmetric IV by agency type - export B: IV by type asym pos neg upper bounds ----
save_df_table(
  make_wide_component_stat_table(asym_iv_by_type_pos_neg_summary, upper_bound_with_details),
  "v22_table_09_B_IV_by_type_asym_pos_neg_upper_bounds",
  "Table 9B: IV upper bounds for positive and negative EAV changes by agency type"
)
## Table 9: Asymmetric IV by agency type - export C: IV by type asym first stage long ----
save_df_table(
  asym_iv_by_type_pos_neg_summary |>
    distinct(
      row,
      agency,
      N,
      F_neg,
      F_pos,
      SW_F_neg,
      SW_F_pos,
      p_exog
    ),
  "v22_table_09_C_IV_by_type_asym_first_stage_long",
  "Table 9C: First-stage and conditional weak-identification diagnostics by agency type"
)
## Table A7: Asymmetric IV point estimates - export B: IV by type asym point estimates pos neg ----
save_df_table(
  asym_iv_by_type_pos_neg_summary |>
    select(
      row,
      agency,
      component,
      N,
      estimate = estimate_with_stars,
      std_error,
      p_value,
      upper_bound = upper_bound_with_details,
      F_neg,
      F_pos,
      SW_F_neg,
      SW_F_pos,
      p_exog,
      p_equal
    ),
  "v22_table_A7_B_IV_by_type_asym_point_estimates_pos_neg",
  "Table A7B: IV asymmetric point estimates for positive and negative EAV changes"
)

## Table A7: Asymmetric IV point estimates - export B: IV by type asym point estimates pos neg wide ----
save_df_table(
  make_wide_component_point_table(
    asym_iv_by_type_pos_neg_summary,
    estimate_with_stars
  ),
  "v22_table_A7_B_IV_by_type_asym_point_estimates_pos_neg_wide",
  paste(
    "Table A7B: IV asymmetric point estimates",
    "for positive and negative EAV changes"
  )
)

## Table 9: Asymmetric IV by agency type - export D: IV by type asym alt neg instrument long ----
save_df_table(
  asym_iv_by_type_pos_neg_alt_summary |>
    select(
      row,
      agency,
      component,
      N,
      estimate = estimate_with_stars,
      std_error,
      p_value,
      upper_bound = upper_bound_with_details,
      F_neg,
      F_pos,
      SW_F_neg,
      SW_F_pos,
      p_exog,
      p_equal
    ),
  "v22_table_09_D_IV_by_type_asym_alt_neg_instrument_long",
  "Table 9D: Alternative asymmetric IV by agency type using neg_d_eav_hat"
)

# Tables 10/11/A8/A9. Lagged OLS models ----

reg_df <- reg_df |>
  group_by(n_agency_group) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    d_2_eav = 100 * log((lag_av * lag_eq_factor_final) / (lag2_av * lag2_eq_factor_final)),
    d_3_eav = 100 * log((lag2_av * lag2_eq_factor_final) / (lag3_av * lag3_eq_factor_final))
  ) |>
  ungroup()

lag_all_ols_1 <- feols(d_levy ~ d_eav + d_2_eav + d_3_eav, data = reg_df, cluster = vcov_uid)
lag_all_ols_2 <- feols(d_levy ~ d_eav + d_2_eav + d_3_eav | year^home_rule_for_fe, data = reg_df, cluster = vcov_uid)
lag_all_ols_3 <- feols(d_levy ~ d_eav + d_2_eav + d_3_eav | year^home_rule_for_fe + n_uniqueid, data = reg_df, cluster = vcov_uid)

lag_all_models_v22 <- list(M1 = lag_all_ols_1, M2 = lag_all_ols_2, M3 = lag_all_ols_3)
lag_all_sum <- list(
  M1 = lincom_sum(lag_all_ols_1, c("d_eav", "d_2_eav", "d_3_eav")),
  M2 = lincom_sum(lag_all_ols_2, c("d_eav", "d_2_eav", "d_3_eav")),
  M3 = lincom_sum(lag_all_ols_3, c("d_eav", "d_2_eav", "d_3_eav"))
)
lag_all_p <- c(M1 = lag_only_p(lag_all_ols_1), M2 = lag_only_p(lag_all_ols_2), M3 = lag_only_p(lag_all_ols_3))
lag_all_ub <- c(M1 = lag_all_sum$M1$A_upper_bound, M2 = lag_all_sum$M2$A_upper_bound, M3 = lag_all_sum$M3$A_upper_bound)


lag_all_joint <- lapply(
  lag_all_models_v22,
  lag_joint_test
)

lag_all_joint_F <- vapply(
  lag_all_joint,
  function(x) x[["joint_lag_F"]],
  numeric(1)
)

lag_all_joint_p <- vapply(
  lag_all_joint,
  function(x) x[["joint_lag_p"]],
  numeric(1)
)

lag_all_add_rows <- add_rows_from_named(
  lag_all_models_v22,
  rows = list(
    N = n_vec(lag_all_models_v22),
    p_lags = lag_all_p,
    joint_lag_F = lag_all_joint_F,
    joint_lag_p = lag_all_joint_p,
    upper_bound = lag_all_ub
  ),
  digits = c(
    N = 0,
    p_lags = 3,
    joint_lag_F = 1,
    joint_lag_p = 3,
    upper_bound = 3
  )
)

## Table 10: All-agency lagged OLS - export OLS all agencies lagged ----
save_table(
  lag_all_models_v22,
  file_stub = "v22_table_10_OLS_all_agencies_lagged",
  title = "Table 10: All agencies OLS allowing for two lags of d_eav",
  notes = c("Standard errors are clustered by taxing agency.",
    "p_lags tests whether the sum of the two lag coefficients equals zero.",
    "joint_lag_F and joint_lag_p test whether both lag coefficients jointly equal zero.",
    "upper_bound is computed for the cumulative sum of current and lagged EAV coefficients."),
  coef_map = c("d_eav" = "Current EAV change", "d_2_eav" = "Lag 1 EAV change", "d_3_eav" = "Lag 2 EAV change"),
  add_rows = lag_all_add_rows,
  gof_omit = ".*"
)

## Table 10: All-agency lagged OLS - export OLS all agencies lagged sums ----
save_df_table(
  bind_rows(
    lincom_sum(lag_all_ols_1, c("d_eav", "d_2_eav", "d_3_eav"), "M1"),
    lincom_sum(lag_all_ols_2, c("d_eav", "d_2_eav", "d_3_eav"), "M2"),
    lincom_sum(lag_all_ols_3, c("d_eav", "d_2_eav", "d_3_eav"), "M3")
  ) |>
    mutate(
      joint_lag_F = lag_all_joint_F[model],
      joint_lag_p = lag_all_joint_p[model]
    ) |>
    select(
      model,
      N,
      lagged_magnitude = lagged_estimate_with_stars,
      std_error,
      lagged_p,
      joint_lag_F,
      joint_lag_p,
      A_upper_bound = A_upper_bound_with_details
    ),
  "v22_table_10_OLS_all_agencies_lagged_sums",
  "Table 10: Sum of current and lagged EAV effects"
)

run_lag_type_v22 <- function(df, gov) {
  d <- df |> filter(type_2 == gov)
  m1 <- feols(d_levy ~ d_eav + d_2_eav + d_3_eav, data = d, cluster = vcov_uid)
  m2 <- feols(d_levy ~ d_eav + d_2_eav + d_3_eav | year, data = d, cluster = vcov_uid)
  m3 <- feols(d_levy ~ d_eav + d_2_eav + d_3_eav | year + n_uniqueid, data = d, cluster = vcov_uid)
  list(m1 = m1, m2 = m2, m3 = m3)
}

run_lag_type_B_v22 <- function(df, gov) {
  d <- df |> filter(type_2 == gov)
  m4 <- feols(d_levy ~ d_eav + d_2_eav + d_3_eav + d_total_ig_revenue | year + n_uniqueid, data = d, cluster = vcov_uid)
  idx <- sample_index(m4, d)
  m5 <- feols(d_levy ~ d_eav + d_2_eav + d_3_eav | year + n_uniqueid, data = d[idx, ], cluster = vcov_uid)
  list(m4 = m4, m5 = m5)
}

lag_school_extra_v22 <- function(df) {
  d <- df |> filter(type_2 == "School")
  feols(d_levy ~ d_eav + d_2_eav + d_3_eav + d_enrollment | year + n_uniqueid, data = d, cluster = vcov_uid)
}

make_one_lag_summary <- function(model, spec, agency) {
  cumulative <- lincom_sum(
    model,
    c("d_eav", "d_2_eav", "d_3_eav"),
    agency
  )

  joint <- lag_joint_test(model)

  cumulative |>
    transmute(
      row = spec,
      agency = .data$model,
      N,
      A_upper_bound,
      lagged_magnitude,
      std_error,
      lagged_p,
      lagged_estimate_with_stars,
      A_upper_bound_with_details,
      joint_lag_F = unname(joint[["joint_lag_F"]]),
      joint_lag_p = unname(joint[["joint_lag_p"]])
    )
}


make_lag_by_type_summary <- function(models_A, models_B, school_extra) {
  rows <- list()

  for (spec in paste0("m", 1:3)) {
    rows[[spec]] <- map_dfr(
      gov_types,
      function(gov) {
        make_one_lag_summary(
          models_A[[gov]][[spec]],
          spec,
          gov
        )
      }
    )
  }

  for (spec in paste0("m", 4:5)) {
    rows[[spec]] <- map_dfr(
      gov_types_B,
      function(gov) {
        make_one_lag_summary(
          models_B[[gov]][[spec]],
          spec,
          gov
        )
      }
    )
  }

  rows[["m6"]] <- make_one_lag_summary(
    school_extra,
    "m6",
    "School"
  )

  bind_rows(rows) |>
    mutate(
      agency = agency_label(agency),
      row = row_label(row)
    )
}

lag_type_A <- setNames(map(gov_types, ~ run_lag_type_v22(reg_df, .x)), gov_types)
lag_type_B <- setNames(map(gov_types_B, ~ run_lag_type_B_v22(reg_df, .x)), gov_types_B)
lag_school_extra <- lag_school_extra_v22(reg_df)

lag_by_type_summary <- make_lag_by_type_summary(lag_type_A, lag_type_B, lag_school_extra)

## Table 11: Lagged OLS by agency type - export OLS by type lagged upper bounds long ----
save_df_table(
  lag_by_type_summary |>
    select(
      row, agency, N,
      lagged_magnitude = lagged_estimate_with_stars,
      std_error,
      lagged_p,
      joint_lag_F,
      joint_lag_p,
      A_upper_bound = A_upper_bound_with_details
    ),
  "v22_table_11_OLS_by_type_lagged_upper_bounds_long",
  "Table 11: OLS upper-bound estimates allowing for two lags of d_eav"
)
## Table 11: Lagged OLS by agency type - export OLS by type lagged upper bounds ----
save_df_table(
  make_wide_stat_table(lag_by_type_summary, A_upper_bound_with_details),
  "v22_table_11_OLS_by_type_lagged_upper_bounds",
  "Table 11: OLS upper-bound estimates allowing for two lags of d_eav"
)
## Table A8: Lagged OLS tests - export OLS by type lagged p values ----
save_df_table(
  lag_by_type_summary |>
    select(
      row,
      agency,
      cumulative_effect_p = lagged_p,
      joint_lag_F,
      joint_lag_p
    ),
  "v22_table_A8_OLS_by_type_lagged_p_values",
  "Table A8: Cumulative-effect and joint lag-exclusion tests"
)
## Table A9: Cumulative lagged OLS estimates - export OLS by type lagged point estimates ----
save_df_table(
  lag_by_type_summary |>
    select(row, agency, lagged_magnitude = lagged_estimate_with_stars, std_error, lagged_p),
  "v22_table_A9_OLS_by_type_lagged_point_estimates",
  "Table A9: Sum of current and lagged EAV coefficients"
)

## Table A9: Cumulative lagged OLS estimates - export OLS by type lagged point estimates wide ----
save_df_table(
  make_wide_point_table(
    lag_by_type_summary,
    lagged_estimate_with_stars
  ),
  "v22_table_A9_OLS_by_type_lagged_point_estimates_wide",
  "Table A9: Sum of current and lagged EAV coefficients"
)

message("Finished corrected clustered manuscript tables. Output folder: ", output_dir)

# Table 4: Agency-type slope equality test ----
# For Table 4, estimate all agency-type slopes in one pooled regression, then test whether two slopes are equal.
m_ols_type_compare <- feols(
  d_levy ~
    d_eav +
      i(type_2, d_eav, ref = "HR_muni") |
      n_uniqueid + year^type_2,
  data = reg_df,
  cluster = ~n_uniqueid
)
etable(
  m_ols_type_compare,
  keep = "d_eav|type_2::"
)
# $p [1] 0.04858383

all_type_equal_test <- fixest::wald(
  m_ols_type_compare,
  keep = "type_2::",
  print = FALSE
)

all_type_equal_test
