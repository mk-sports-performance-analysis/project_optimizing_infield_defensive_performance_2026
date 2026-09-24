# ============================================================
# Figure 2: Pitch location and batted-ball trajectory
# ============================================================
library(tidyverse)
library(ggsignif)
library(viridis)
library(ggpattern)
library(shadowtext)
library(here)

# Load processed data
load(here("data", "battedball.RData"))
source(here("R", "load_functions.R"))

# Select right- and left-handed batters for the 2025 season
R <- battedball |> filter(stand == "R", year == 2025) |> select(-stand)
L <- battedball |> filter(stand == "L", year == 2025) |> select(-stand)


# Statistical tests
table_r <- table(R$New_zone_height, R$launch_angle_cat)
chisq_r <- chisq.test(table_r)
print(chisq_r)

table_l <- table(L$New_zone_height, L$launch_angle_cat)
chisq_l <- chisq.test(table_l)
print(chisq_l)

cramers_v(table_r)
cramers_v(table_l)


##############################
#figure 3

data2025 <- battedball |>
  filter(year == 2025) |>
  mutate(
    New_zone2 = forcats::fct_relevel(
      forcats::fct_expand(New_zone, "0"),
      "0"))

# Add the overall panel
data_facet <- bind_rows(
  data2025 |> mutate(New_zone2 = "0"),
  data2025
)

# Calculate proportions
summarybylaunch <- data_facet |>
  count(launch_angle_cat, stand, New_zone2, Areas, name = "n") |>
  group_by(launch_angle_cat, New_zone2, stand) |>
  mutate(prop1 = n / sum(n) * 100) |>
  ungroup()


df1 <- summarybylaunch |>
  mutate(New_zone_height = case_when(
    New_zone2 %in% 1:3 ~ "High",
    New_zone2 %in% 4:6 ~ "Middle",
    New_zone2 %in% 7:9 ~ "Low",
    TRUE ~ NA_character_
  ),
  New_zone_height = factor(New_zone_height, levels = c("Low", "Middle", "High"))) |>
  filter(is.na(New_zone_height) == F) |>
  group_by(launch_angle_cat, New_zone_height, stand) |>
  summarise(totaln = sum(n)) |>
  group_by( New_zone_height, stand) |>
  mutate(alln = sum(totaln)) |>
  ungroup() |>
  mutate(ratio = totaln/alln)

