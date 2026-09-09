library(tidyverse)
library(viridis)
library(quantreg)
library(patchwork)
library(here)

load(here("data", "battedball.RData"))
source(here("R", "load_functions.R"))
load(here("data", "QR_main_model.RData"))

MIN_BIP <- 100
REPS    <- 100         
TAUS    <- c(0.10, 0.25, 0.50, 0.75, 0.90)

set.seed(1)

battedball <- battedball |>
  filter(year != 2025) |>
  select(
    batter, pitcher, platoon, p_throws, stand, main_type, 
    plate_x, plate_z, pfx_x, pfx_z, release_speed,
    sprayangle, launch_angle, situation_cat, year
  ) |>
  mutate(
    across(c(batter, pitcher, platoon, p_throws, stand, main_type, situation_cat), as.factor)
  ) |>
  group_by(main_type) |>
  mutate(
    release_speed_c = release_speed - mean(release_speed, na.rm = TRUE),
    pfx_x_c         = pfx_x - mean(pfx_x, na.rm = TRUE),
    pfx_z_c         = pfx_z - mean(pfx_z, na.rm = TRUE)
  ) |>
  ungroup()

form_qr <- sprayangle ~
  plate_x + plate_z +
  platoon +
  situation_cat +
  main_type +
  release_speed_c * main_type +
  pfx_x_c * main_type +
  pfx_z_c * main_type +
  pfx_x_c * platoon

batter_n <- battedball |>
  filter(stand %in% c("R", "L")) |>
  count(stand, batter, name = "n_bip")

keep_R <- batter_n |> filter(stand == "R", n_bip >= MIN_BIP) |> pull(batter)
keep_L <- batter_n |> filter(stand == "L", n_bip >= MIN_BIP) |> pull(batter)

base_R <- battedball |> filter(stand == "R", batter %in% keep_R) |> select(-stand)
base_L <- battedball |> filter(stand == "L", batter %in% keep_L) |> select(-stand)

balanced_sample <- function(data, n_per_batter = MIN_BIP) {
  data |>
    group_by(batter) |>
    slice_sample(n = n_per_batter) |>
    ungroup() |>
    mutate(batter = droplevels(batter))
}

fit_balanced_once <- function(base_data, formula, rep_id) {  
  bal_data <- balanced_sample(base_data)  
  map_dfr(TAUS, function(q) {
    m <- quantreg::rq(
      formula,
      data = bal_data,
      tau = q
    )  
    ct <- summary(m)$coefficients    
    tibble(
      rep = rep_id,
      tau = q,
      plate_x = ct["plate_x", 1],
      plate_z = ct["plate_z", 1]
    )
  })
}

resample_R <- map_dfr(1:REPS, ~ fit_balanced_once(base_R, form_qr, .x))
resample_L <- map_dfr(1:REPS, ~ fit_balanced_once(base_L, form_qr, .x))

format_combined_results <- function(m_list) {
  map_dfr(m_list, function(m) {
    sum_m <- summary(m)
    ct <- as.data.frame(sum_m$coefficients)
    colnames(ct) <- c("Estimate", "Std_Error", "t_value", "p_value")
    ct |>
      tibble::rownames_to_column("term") |>
      mutate(
        est   = sprintf("%.1f", Estimate),
        lower = sprintf("%.1f", Estimate - 1.96 * Std_Error),
        upper = sprintf("%.1f", Estimate + 1.96 * Std_Error),
        sig   = case_when(p_value < 0.001 ~ "*", TRUE ~ ""),
        display_text = paste0(est, sig, "@[", lower, ", ", upper, "]"),
        display_text = gsub("-", "\u2212", display_text)
      ) |>
      select(term, display_text)
  }, .id = "quantile") |>
    pivot_wider(names_from = quantile, values_from = display_text)
}

orig_est <- function(m_list, side_label) {
  format_combined_results(m_list) |>
    pivot_longer(-term, names_to = "quantile", values_to = "text") |>
    filter(term %in% c("plate_x", "plate_z")) |>
    mutate(
      text = str_replace_all(text, "\u2212", "-"),
      est  = as.numeric(str_extract(text, "^[-0-9.]+")),
      tau  = as.numeric(str_remove(quantile, "%")) / 100,
      side = side_label
    ) |>
    select(term, tau, side, orig_est = est)
}

orig_R <- orig_est(models_R, "RHB")
orig_L <- orig_est(models_L, "LHB")

summarise_resample <- function(resample_df, side_label) {
  resample_df |>
    pivot_longer(cols = c(plate_x, plate_z), names_to = "term", values_to = "estimate") |>
    group_by(term, tau) |>
    summarise(
      resample_mean = mean(estimate),
      resample_sd   = sd(estimate),
      resample_lo   = min(estimate),
      resample_hi   = max(estimate),
      .groups = "drop"
    ) |>
    mutate(side = side_label)
}

resample_summary <- bind_rows(
  summarise_resample(resample_R, "RHB"),
  summarise_resample(resample_L, "LHB")
)

comparison <- resample_summary |>
  left_join(bind_rows(orig_R, orig_L), by = c("term", "tau", "side")) |>
  mutate(
    diff        = resample_mean - orig_est,
    pct_diff    = 100 * diff / orig_est,
    orig_inside_res_CI = orig_est >= resample_lo & orig_est <= resample_hi
  ) |>
  arrange(side, term, tau)

plot_data <- bind_rows(
  resample_R |> mutate(side = "RHB"),
  resample_L |> mutate(side = "LHB")
) |>
  pivot_longer(cols = c(plate_x, plate_z), names_to = "term", values_to = "estimate") |>
  mutate(
    term = case_when(
      term == "plate_x" ~ "a. Horizontal pitch location",
      term == "plate_z" ~ "b. Vertical pitch location"
    ),
    term = factor(term, levels = c("a. Horizontal pitch location", "b. Vertical pitch location")),
    side = factor(side, levels = c("RHB", "LHB"))
  )

orig_points <- bind_rows(orig_R, orig_L) |>
  mutate(
    term = case_when(
      term == "plate_x" ~ "a. Horizontal pitch location",
      term == "plate_z" ~ "b. Vertical pitch location"
    ),
    term = factor(term, levels = c("a. Horizontal pitch location", "b. Vertical pitch location")),
    side = factor(side, levels = c("RHB", "LHB"))
  )

if (.Platform$OS.type == "windows") {
  windowsFonts(MEI = windowsFont("Arial"))
  font_family <- "MEI"
} else {
  font_family <- "sans"
}

FIG_SIZE_MM <- 174
TEXT_MM     <- 2.7
LINE_MM     <- 0.3
POINT_MM    <- 2

g3 <- ggplot(plot_data, aes(x = tau, y = estimate, group = interaction(tau, side))) +
  geom_boxplot(
    aes(fill = paste0(side, "_box"), alpha = "Distribution of estimates from resamples"), 
    color = "black", position = position_dodge(width = 0.1), 
    outlier.size = 0.8, width = 0.1, linewidth = LINE_MM
  ) +
  geom_point(
    data = orig_points, 
    aes(x = tau, y = orig_est, shape = side, fill = side, alpha = "Original"),
    color = "black", position = position_dodge(width = 0.1), 
    size = POINT_MM, stroke = LINE_MM
  ) +
  geom_hline(yintercept = 0, color = "black", linewidth = LINE_MM, alpha = 0.6) +
  facet_wrap(~term, ncol = 2) +
  scale_x_continuous(
    breaks = c(0.10, 0.25, 0.50, 0.75, 0.90), 
    labels = c("0.10", "0.25", "0.50", "0.75", "0.90"), 
    limits = c(0.05, 0.95)
  ) +
  scale_y_continuous(breaks = scales::breaks_width(10)) +
  scale_fill_manual(
    name   = "Batter Handedness", 
    values = c(RHB = "blue", LHB = "firebrick", RHB_box = "#D0E4FF", LHB_box = "#FFD8D8"), 
    breaks = c("RHB", "LHB")
  ) +
  scale_shape_manual(name = "Batter Handedness", values = c(RHB = 23, LHB = 24)) +
  scale_alpha_manual(
    name   = "Data", 
    values = c("Distribution of estimates from resamples" = 1, "Original" = 1)
  ) +
  guides(
    fill  = guide_legend(order = 1), 
    shape = guide_legend(order = 1),
    alpha = guide_legend(
      order = 2, 
      override.aes = list(shape = c(22, 24), fill = c("white", "white"), color = "black", size = 2.5)
    )
  ) +
  labs(x = "Quantiles", y = "Estimated coefficient") +
  theme_classic() +
  theme(
    text                  = element_text(size = mm_text_theme(TEXT_MM), family = font_family, colour = "black"),
    plot.title            = element_text(hjust = 0, face = "bold", size = mm_text_theme(TEXT_MM), family = font_family, colour = "black"),
    strip.background      = element_blank(),
    strip.text.x          = element_text(angle = 0, hjust = 0, face = "bold", size = mm_text_theme(TEXT_MM), family = font_family, colour = "black"),
    axis.line             = element_line(linewidth = mm_line(LINE_MM), colour = "black"),
    axis.ticks            = element_line(linewidth = mm_line(LINE_MM), colour = "black"),
    panel.grid.major      = element_line(color = "gray88", linewidth = mm_line(LINE_MM * 0.8)),
    panel.grid.minor      = element_blank(),
    legend.position       = "bottom",
    legend.text           = element_text(size = mm_text_theme(TEXT_MM), family = font_family, colour = "black"),
    legend.background     = element_blank(), 
    legend.box.background = element_rect(colour = "black", fill = "white", linewidth = mm_line(LINE_MM * 0.5)),
    legend.margin         = margin(t = 3, r = 5, b = 3, l = 5, unit = "pt")
  )

g3

ggsave(
  "supple6.tiff", plot = g3, dpi = 300,
  width = 174, height = 100, units = "mm",
  bg = "white", limitsize = FALSE, compression = "lzw", device = "tiff"
)
