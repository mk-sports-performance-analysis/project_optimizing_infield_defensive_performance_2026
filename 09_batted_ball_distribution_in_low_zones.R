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

shapes_map <- c("1B" = 21, "SS" = 22, "2B" = 24, "3B" = 25)


windowsFonts(ASE_FONT = windowsFont("Arial"))

FIG_SIZE_MM <- 174
TEXT_MM <- 3
LINE_MM <- 0.3
POINT_MM <- 0.7
MARGIN_MM <- 0.5

TEXT_SIZE <- mm_text_theme(TEXT_MM)
LINE_SIZE <- mm_line(LINE_MM)

theme_mm <- theme_classic() +
  theme(
    text = element_text(size = TEXT_SIZE, family = "ASE_FONT", colour = "black"),
    plot.title = element_text(hjust = 0, face = "bold", size = TEXT_SIZE, family = "ASE_FONT", colour = "black"),
    axis.title = element_text(size = TEXT_SIZE, family = "ASE_FONT", colour = "black"),
    axis.text = element_text(size = TEXT_SIZE, family = "ASE_FONT", colour = "black"),
    strip.background = element_blank(),
    strip.text.x = element_text(angle = 0, hjust = 0, face = "bold", size = TEXT_SIZE, family = "ASE_FONT", colour = "black"),
    legend.title = element_text(size = TEXT_SIZE, family = "ASE_FONT", colour = "black"),
    legend.text = element_text(size = TEXT_SIZE, family = "ASE_FONT", colour = "black")
  )

g1 <- ggplot() +
  geom_point(
    data = R_position,
    aes(x = angle_bin, y = dist_bin, shape = position),
    fill = "gray", colour = "gray20", size = 6, stroke = 0.5) +
  geom_tile(
    data = heat |> filter(stand == "R"),
    aes(x = angle_bin, y = dist_bin, fill = ratio), alpha = 0.6) +
  geom_text(
    data = heat |> filter(stand == "R"),
    aes(x = angle_bin, y = dist_bin, label = sprintf("%.1f", ratio)),
    size = TEXT_MM, family = "ASE_FONT") +
  scale_fill_gradient("Ratio [%]", low = "white", high = "red", limits = c(0, 38)) +
  scale_x_discrete(limits = levels(heat$angle_bin)) +
  scale_y_discrete(limits = levels(heat$dist_bin)) +
  facet_wrap(~New_zone, ncol = 1, labeller = zone_labeller) +
  theme_mm +
  xlab("Spray-angle ranges [°]") +
  ylab("Flight distance ranges [m]") +
  ggtitle("a. RHB") +
  geom_vline(xintercept = c(1.5, 4.5, 7.5), linetype = c(rep(c("solid", "dotted", "solid"), 3)), col = "gray") +
  geom_hline(yintercept = 1.5, linetype = "solid", col = "gray") +
  theme(
    legend.background = element_rect(colour = "black", fill = "white", linewidth = LINE_SIZE * 0.3),
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)
  ) +
  scale_shape_manual(values = shapes_map, name = "Position: ")

g2 <- ggplot() +
  geom_point(
    data = L_position,
    aes(x = angle_bin, y = dist_bin, shape = position),
    fill = "gray", colour = "gray20", size = 6, stroke = 0.5) +
  geom_tile(
    data = heat |> filter(stand == "L"),
    aes(x = angle_bin, y = dist_bin, fill = ratio), alpha = 0.6) +
  geom_text(
    data = heat |> filter(stand == "L"),
    aes(x = angle_bin, y = dist_bin, label = sprintf("%.1f", ratio)),
    size = TEXT_MM, family = "ASE_FONT") +
  scale_fill_gradient("Ratio [%]", low = "white", high = "red", limits = c(0, 38)) +
  facet_wrap(~New_zone, ncol = 1, labeller = zone_labeller) +
  scale_x_discrete(limits = levels(heat$angle_bin)) +
  scale_y_discrete(limits = levels(heat$dist_bin)) +
  theme_mm +
  xlab("Spray-angle ranges [°]") +
  ylab("Flight distance ranges [m]") +
  ggtitle("b. LHB") +
  geom_vline(xintercept = c(1.5, 4.5, 7.5), linetype = c(rep(c("solid", "dotted", "solid"), 3)), col = "gray") +
  geom_hline(yintercept = 1.5, linetype = "solid", col = "gray") +
  theme(
    legend.background = element_rect(colour = "black", fill = "white", linewidth = LINE_SIZE * 0.3),
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)
  ) +
  scale_shape_manual(values = shapes_map, name = "Position: ")

g3 <- (g1 | g2) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
g3

ggsave(
  "supple3.tiff", g3,
  width = 170, height = 150, units = "mm", dpi = 300,
  bg = "white", limitsize = FALSE, compression = "lzw", device = "tiff"
)
