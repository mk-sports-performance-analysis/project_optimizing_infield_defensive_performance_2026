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
# Figure2

prep_plot_data <- function(data) {
  data |>
    group_by(New_zone_height, launch_angle_cat) |>
    summarize(count = n(), .groups = "drop") |>
    group_by(New_zone_height) |>
    mutate(Percentage = count / sum(count) * 100) |>
    ungroup() |>
    mutate(
      Zone = factor(New_zone_height, levels = c("Low", "Middle", "High")),
      Trajectory = factor(launch_angle_cat, levels = rev(c("GB", "LD", "FB", "Pop-up")))
    )
}


# settings
TEXT_MM <- 2.6
MARGIN_MM <- 0.5
LINE_MM <- 0.1
windowsFonts(ASE_FONT = windowsFont("Arial"))
family <- "ASE_FONT"

theme_mm <- theme_classic() +
  theme(
    text = element_text(size = mm_text(TEXT_MM), family = family, colour = "black"),
    axis.title = element_text(size = mm_text(TEXT_MM)),
    axis.text = element_text(size = mm_text(TEXT_MM)),
    plot.margin = margin(t = mm_line(MARGIN_MM), r = 0, b = mm_line(MARGIN_MM), l = 0, unit = "pt"),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text.x = element_text(angle = 0, hjust = 0, face = "bold", size = mm_text(TEXT_MM))
  )

df_r <- prep_plot_data(R) |> mutate(stand = "a. RHB")
df_l <- prep_plot_data(L) |> mutate(stand = "b. LHB")
df_all <- bind_rows(df_r, df_l) |> mutate(stand = factor(stand, levels = c("a. RHB", "b. LHB")))


#Figure 2

final_fig <- ggplot(df_all, aes(x = Zone, y = Percentage, fill = Trajectory, pattern = Trajectory)) +
  geom_bar_pattern(
    stat = "identity",
    position = "stack",
    width = 0.8,
    color = "black",
    linewidth = mm_line(0.05),
    pattern_fill = "black",
    pattern_color = "black",
    pattern_density = 0.08,
    pattern_spacing = 0.04,pattern_size = 0.05, 
    pattern_key_scale_factor = 0.6
  ) +
  geom_shadowtext(
    aes(label = ifelse(Trajectory != "FB" & Percentage >= 5, sprintf("%.1f", Percentage), "")),
    position = position_stack(vjust = 0.5), family = family, size = TEXT_MM, fontface = "bold",
    color = "black", bg.colour = "white", bg.r = 0.15
  ) +
  scale_fill_viridis("Flight trajectory classification", discrete = TRUE, direction = -1) +
  scale_pattern_manual(
    name = "Flight trajectory classification",
    values = c("Pop-up" = "circle", "FB" = "crosshatch", "LD" = "stripe", "GB" = "none")
  ) +
  geom_signif(
    y_position = c(105, 114, 105), xmin = c(1.1, 1.0, 2.1), xmax = c(1.9, 3.0, 2.9),
    annotation = c("*", "*", "*"), tip_length = 0.03, textsize = TEXT_MM*1.3,
    vjust = 0.5,  family = family
  ) +
  scale_y_continuous(limits = c(0, 125), expand = c(0, 0), breaks = seq(0, 100, 50)) +
  facet_wrap(~stand) +
  labs(x = "Pitch zones in the vertical direction", y = "Ratio (%)") +
  guides(
    fill = guide_legend(
      title = "Flight trajectory classification", nrow = 1, title.position = "top", title.hjust = 0.5,
      keywidth = unit(2.5, "mm"), keyheight = unit(2.5, "mm")
    ),
    pattern = guide_legend(
      title = "Flight trajectory classification", nrow = 1, title.position = "top", title.hjust = 0.5,
      keywidth = unit(2.5, "mm"), keyheight = unit(2.5, "mm")
    )
  ) +
  theme_mm +
  theme(
    strip.text = element_text(face = "bold", size = mm_text(TEXT_MM), hjust = 0),
    panel.spacing = unit(3, "mm"),
    legend.position = "bottom",
    legend.background = element_rect(color = "black", linewidth = mm_line(0.1)),
    legend.margin = margin(2, 2, 2, 2, "pt"),
    legend.spacing.x = unit(0.5, "mm"),
    legend.title = element_text(size = mm_text(TEXT_MM), face = "bold"),
    legend.text = element_text(size = mm_text(TEXT_MM)),
    plot.margin = margin(2, 4, 2, 2, "pt")
  )

print(final_fig)

ggsave(
  filename = "Fig2.tiff", plot = final_fig, dpi = 300,
  width = 84, height = 75, units = "mm", bg = "white",
  limitsize = FALSE, compression = "lzw", device = "tiff"
)

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
  


# Facet labels
labeli <- as_labeller(c(
  "0" = "i. All", "1" = "ii. zone 1", "2" = "iii. zone 2",
  "3" = "iv. zone 3", "4" = "v. zone 4", "5" = "vi. zone 5",
  "6" = "vii. zone 6", "7" = "viii. zone 7", "8" = "ix. zone 8",
  "9" = "x. zone 9"
))

# Plot function
make_plot <- function(data, stand_label) {
  n_label <- data |> group_by(launch_angle_cat, New_zone2) |>
    summarise(n_total = sum(n), .groups = "drop")
  
  count_header <- n_label |> filter(launch_angle_cat == "Pop-up") |>
    mutate(header_label = "Count")
  
  ggplot(data, aes(x = launch_angle_cat, y = prop1, fill = Areas)) +
    geom_col(width = 0.9, color = "grey30", linewidth = LINE_MM, position = position_stack(reverse = TRUE)) +
    geom_text(
      aes(label = ifelse(prop1 > 12, sprintf("%.1f", prop1), ""), color = ifelse(Areas == "Pull side", "white", "black")),
      position = position_stack(vjust = 0.5), size = TEXT_MM * 0.65, family = family
    ) +
    coord_flip() +
    scale_y_continuous(breaks = seq(0, 100, 25), limits = c(0, 132), expand = c(0, 0.3)) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.25))) +
    scale_fill_viridis_d(name = "Spray-angle classification", direction = 1) +
    scale_color_identity() +
    labs(x = "Flight trajectory classification", y = "Ratio (%)") +
    ggtitle(stand_label) +
    facet_wrap(~New_zone2, labeller = labeli, ncol = 5) +
    theme_classic() +
    theme(
      text = element_text(size = mm_text(TEXT_MM), family = family, colour = "black"),
      plot.title = element_text(hjust = 0, face = "bold", size = mm_text(TEXT_MM * 1.2), family = family),
      strip.text = element_text(hjust = 0, face = "bold", size = mm_text(TEXT_MM), family = family),
      strip.background = element_blank(),
      legend.position = "bottom",
      legend.background = element_rect(fill = "white", colour = "black", linewidth = mm_line(LINE_MM)),
      legend.key.size = unit(2, "mm"),
      legend.spacing.x = unit(3, "mm"),
      legend.spacing.y = unit(1, "mm"),
      legend.title = element_text(size = mm_text(TEXT_MM), face = "bold"),
      legend.text = element_text(size = mm_text(TEXT_MM)),
      panel.spacing = unit(2, "mm")
    ) +
    guides(fill = guide_legend(override.aes = list(size = 2)), color = "none") +
    geom_text(
      data = n_label, aes(x = launch_angle_cat, y = 105, label = paste0("(", n_total, ")")),
      inherit.aes = FALSE, hjust = 0, size = TEXT_MM * 0.68, family = family, colour = "black"
    ) +
    geom_text(
      data = count_header, aes(x = launch_angle_cat, y = 105, label = header_label),
      inherit.aes = FALSE, hjust = 0, vjust = -1, size = TEXT_MM * 0.7, family = family, colour = "black"
    )
}

# Create Figure 3
plot_r <- make_plot(filter(summarybylaunch, stand == "R"), "a. RHB")
plot_l <- make_plot(filter(summarybylaunch, stand == "L"), "b. LHB")

final_plot <- plot_r / plot_l +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(final_plot)

# Save Figure 3
ggsave(
  filename = "Fig3.tiff", plot = final_plot, dpi = 300,
  width = 174, height = 130, units = "mm", bg = "white",
  limitsize = FALSE, compression = "lzw", device = "tiff"
)
