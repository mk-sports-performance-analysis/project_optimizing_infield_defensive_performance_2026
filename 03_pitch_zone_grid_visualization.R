
library(stringr)
library(tidyverse)
library(ggtext)
library(here)

# Load processed data
load(here("data", "battedball.RData"))
source(here("R", "load_functions.R"))


# Count observations by pitch zone and batter handedness
zone_n <- battedball |>
  group_by(stand) |>
  filter(!is.na(New_zone)) |>
  count(New_zone) |>
  rename(zone = New_zone) |>
  mutate(zone = factor(zone))

# Define pitch-zone boundaries
inch <- 0.0254
half_width <- (17 + 2.9) * inch / 3 / 2
z_top <- 0.859
z_mid <- 0.673

zone_df <- tibble(
  zone = factor(1:9),
  xmin = c(-Inf, -half_width, half_width, -Inf, -half_width, half_width, -Inf, -half_width, half_width),
  xmax = c(-half_width, half_width, Inf, -half_width, half_width, Inf, -half_width, half_width, Inf),
  ymin = c(z_top, z_top, z_top, z_mid, z_mid, z_mid, -Inf, -Inf, -Inf),
  ymax = c(Inf, Inf, Inf, z_top, z_top, z_top, z_mid, z_mid, z_mid)
)

# Create zone labels and observation counts
label_df <- zone_df |>
  mutate(
    x = ifelse(is.infinite(xmin), xmax - 0.05, ifelse(is.infinite(xmax), xmin + 0.05, (xmin + xmax) / 2)),
    y = ifelse(is.infinite(ymin), ymax - 0.05, ifelse(is.infinite(ymax), ymin + 0.05, (ymin + ymax) / 2))
  ) |>
  tidyr::expand_grid(stand = c("R", "L"))

label_df2 <- label_df |>
  left_join(zone_n, by = c("zone", "stand")) |>
  mutate(
    n = if_else(stand == "L" & zone %in% c(8, 9), paste0(stand, ":   ", scales::comma(n)), paste0(stand, ": ", scales::comma(n))),
    stand = factor(stand, levels = c("R", "L"))
  ) |>
  pivot_wider(values_from = n, names_from = stand) |>
  mutate(
    zone_label = paste0("zone ", zone),
    count_label = paste0(R, "\n", L)
  )

# Figure settings
windowsFonts(ASE_FONT = windowsFont("Arial"))
family <- "ASE_FONT"
FIG_SIZE_MM <- 100
TEXT_MM <- 2.8
LINE_MM <- 0.7
POINT_MM <- 2
MARGIN_MM <- 0.5

theme_mm <- theme_classic() +
  theme(
    text = element_text(size = mm_text_theme(TEXT_MM), family = family, colour = "black"),
    axis.title = element_text(size = mm_text_theme(TEXT_MM), family = family, colour = "black"),
    axis.text = element_text(size = mm_text_theme(TEXT_MM), family = family, colour = "black"),
    plot.margin = ggplot2::margin(t = mm_line(MARGIN_MM), r = 0, b = mm_line(MARGIN_MM), l = 0, unit = "pt"),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text.x = element_text(angle = 0, hjust = 0, face = "bold", size = mm_text_theme(TEXT_MM), family = family, colour = "black")
  )

# Create supplementary Figure 1
g1 <- ggplot() +
  geom_point(data = battedball, aes(plate_x, plate_z, fill = New_zone, col = New_zone), shape = 21, size = POINT_MM, alpha = 0.3) +
  geom_text(
    data = label_df2,
    aes(x = x + scale(x) * 0.11, y = y + scale(y) * 0.12 + 0.06, label = zone_label),
    size = TEXT_MM, family = family, fontface = "bold", vjust = 1
  ) +
  geom_text(
    data = label_df2,
    aes(x = x + scale(x) * 0.11, y = y + scale(y) * 0.12, label = count_label),
    size = TEXT_MM, family = family, lineheight = 1, vjust = 1
  ) +
  coord_equal() +
  labs(x = "Horizontal pitch location [m]", y = "Vertical pitch location [m]") +
  scale_y_continuous(limits = c(0, 1.27)) +
  theme_mm +
  annotate("segment", x = -0.2, xend = -0.45, y = 0.1, yend = 0.1, arrow = arrow(length = unit(2, "mm")), size = LINE_MM, colour = "black") +
  annotate("text", x = -0.25, y = 0.05, label = "Inside", size = TEXT_MM, family = family) +
  annotate("segment", x = 0.2, xend = 0.45, y = 0.1, yend = 0.1, arrow = arrow(length = unit(2, "mm")), size = LINE_MM, colour = "black") +
  annotate("text", x = 0.25, y = 0.05, label = "Outside", size = TEXT_MM, family = family)


ggsave(
  file = "supple fig2.tiff",
  plot = g1,
  dpi = 300,
  width = 100,
  height = 84,
  units = "mm",
  bg = "white",
  limitsize = FALSE,
  compression = "lzw",
  device = "tiff"
)