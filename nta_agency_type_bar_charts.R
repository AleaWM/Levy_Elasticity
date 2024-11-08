## Bar Charts for NTA ##

library(tidyverse)

## All agencies

df |>
  filter(year == 2022) |>
  mutate(type = ifelse(type == "Muni", "Municipality", type)) |>
  ggplot(aes(x = type)) +
  geom_bar(fill = "blue4") +
  geom_text(stat = 'count', aes(label = ..count.., y = ..count..), vjust = -0.5, color = "black") +
  labs(
    title = "Taxing Agency Count by Agency Type",
    subtitle = "Values Are Consistent Across All Years",
    x = "Agency Type",
    y = "N"
  ) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.y = element_text(angle = 0))

# Schools

df |>
  filter(year == 2022) |>
  filter(type == "School") |>
  filter(minor_type %in% c("ELEMENTARY", "SECONDARY")) |>
  ggplot(aes(x = minor_type, fill = minor_type)) +
  geom_bar(fill = "blue4") +
  geom_text(stat = 'count', aes(label = ..count.., y = ..count..), vjust = -0.5, color = "black") +
  labs(
    title = "Schools: Primary and Secondary",
    subtitle = "Values Are Consistent Across All Years",
    x = "School Agency Type",
    y = "N"
  ) +
  theme_classic() +
  #scale_fill_manual(values = c(brewer.pal(n = 6, "Blues")[3], brewer.pal(n = 6, "Blues")[5])) +  # Specify colors individually
  theme(legend.position = "none",
        axis.title.y = element_text(angle = 0))

# Munis

df |>
  filter(year == 2022) |>
  filter(type == "Muni") |>
  mutate(home_rule = ifelse(home_rule_ind == 1, "Yes", "No")) |>  # Create a new column for home rule status
  ggplot(aes(x = home_rule, fill = home_rule)) +  # Use the new column for x-axis
  geom_bar(fill = "blue4") +
  geom_text(stat = 'count', aes(label = ..count.., y = ..count..), vjust = -0.5, color = "black") +
  labs(
    title = "Municipalities by Home Rule Status",
    subtitle = "Values Are Consistent Across All Years",
    x = "Home Rule Status",
    y = "N"
  ) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.y = element_text(angle = 0))

## Histograms for NTA?

# All agencies

df |>
  filter(year %in% c("2012", "2022")) |>
  ggplot(aes(x = (cty_total_eav/1e+06))) +
  geom_histogram(fill = "blue4") +
  facet_grid(cols = vars(year)) +
  labs(
    title = "Taxing Agency EAV",
    subtitle = "2012 and 2022",
    x = "$ (millions)",
    y = "N"
  ) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.y = element_text(angle = 0))

df |>
  filter(year %in% c("2012", "2022")) |>
  ggplot(aes(x = (total_final_levy/1e+06))) +
  geom_histogram(fill = "blue4") +
  facet_grid(cols = vars(year)) +
  labs(
    title = "Taxing Agency EAV",
    subtitle = "2012 and 2022",
    x = "$ (millions)",
    y = "N"
  ) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.y = element_text(angle = 0))

# Munis

df |>
  filter(year %in% c("2012", "2022")) |>
  ggplot(aes(x = (cty_total_eav/1e+06))) +
  geom_histogram(fill = "blue4") +
  facet_grid(cols = vars(year), rows = vars(home_rule_ind)) +
  labs(
    title = "Taxing Agency EAV",
    subtitle = "2012 and 2022",
    x = "$ (millions)",
    y = "N"
  ) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.y = element_text(angle = 0))
