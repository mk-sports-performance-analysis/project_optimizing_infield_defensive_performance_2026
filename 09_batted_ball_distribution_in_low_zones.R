library(tidyverse)
library(xgboost)
library(caret)
library(patchwork)
library(here)

load(here("data", "base_data.RData"))
source(here("R", "load_functions.R"))
load(here("data", "l_2025_league.RData"))
load(here("data", "optimal set.RData"))

count(base_data |> filter(year == 2025), y_result, stand, year) |>
  group_by(year, stand) |>
  mutate(ratio = n / sum(n))

data <- base_data |> 
  filter(year == 2025, New_zone %in% 7:9) |> 
  mutate(stand = factor(stand, levels = c("R", "L")))

l <- l_2025_league |>
  filter(position %in% c("3B", "SS", "2B", "1B")) |>
  group_by(position, stand) |>
  summarise(x = mean(lx), y = mean(ly), .groups = "drop")

angle_breaks <- c(-Inf, seq(-45, 45, 15), Inf)
dist_breaks <- seq(0, 60, by = 10)

heat <- data |>
  mutate(
    angle_bin = cut(sprayangle_raw, breaks = angle_breaks, include.lowest = TRUE, right = FALSE),
    dist_bin = cut(Distance, breaks = dist_breaks, include.lowest = TRUE, right = FALSE)
  ) |>
  count(stand, New_zone, angle_bin, dist_bin) |>
  group_by(stand, New_zone) |>
  mutate(
    angle_id = as.numeric(angle_bin), dist_id = as.numeric(dist_bin),
    start = angle_breaks[angle_id], end = angle_breaks[angle_id + 1],
    r0 = dist_breaks[dist_id], r = dist_breaks[dist_id + 1],
    ratio = n / sum(n) * 100
  ) |>
  ungroup()

heat_total <- heat |>
  group_by(stand, New_zone, angle_bin) |>
  summarise(n = sum(n, na.rm = TRUE), ratio = sum(ratio, na.rm = TRUE), .groups = "drop") |>
  mutate(dist_bin = "Total")

heat <- bind_rows(heat, heat_total)

windowsFonts(ASE_FONT = windowsFont("Arial"))
FIG_SIZE_MM <- 174
TEXT_MM <- 3
LINE_MM <- 0.3
POINT_MM <- 0.7
MARGIN_MM <- 0.5


zone_labeller <- as_labeller(function(x) sapply(x, function(val) {
  paste0(tolower(as.character(utils::as.roman(as.numeric(val) - 6))), ". zone ", val)
}))

R_position <- R_position |>
  rename(New_zone = zone) |>
  filter(Formation == "Optimal", New_zone %in% 7:9) |>
  mutate(
    Distance = sqrt(x^2 + y^2),
    sprayangle_raw = -atan2(x, y) * 180 / pi,
    angle_bin = cut(sprayangle_raw, breaks = angle_breaks, include.lowest = TRUE, right = FALSE),
    dist_bin = cut(Distance, breaks = dist_breaks, include.lowest = TRUE, right = FALSE)
  )

L_position <- L_position |>
  rename(New_zone = zone) |>
  filter(Formation == "Optimal", New_zone %in% 7:9) |>
  mutate(
    Distance = sqrt(x^2 + y^2),
    sprayangle_raw = -atan2(x, y) * 180 / pi,
    angle_bin = cut(sprayangle_raw, breaks = angle_breaks, include.lowest = TRUE, right = FALSE),
    dist_bin = cut(Distance, breaks = dist_breaks, include.lowest = TRUE, right = FALSE)
  )

raw_dist_levels <- levels(cut(0, breaks = dist_breaks, include.lowest = TRUE, right = FALSE))
angle_levels_rev <- rev(levels(cut(0, breaks = angle_breaks, include.lowest = TRUE, right = FALSE)))
dist_levels <- c("Total", raw_dist_levels)

heat <- heat |> mutate(dist_bin = factor(dist_bin, levels = dist_levels), angle_bin = factor(angle_bin, levels = angle_levels_rev))
R_position <- R_position |> mutate(dist_bin = factor(dist_bin, levels = dist_levels), angle_bin = factor(angle_bin, levels = angle_levels_rev))
L_position <- L_position |> mutate(dist_bin = factor(dist_bin, levels = dist_levels), angle_bin = factor(angle_bin, levels = angle_levels_rev))
