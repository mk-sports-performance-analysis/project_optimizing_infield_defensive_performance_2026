# ============================================================
#  quantile regression
# ============================================================
library(tidyverse)
library(ggsignif)
library(viridis)
library(qgam)
library(DescTools)
library(here)

# Load processed data
load(here("data", "battedball.RData"))
source(here("R", "load_functions.R"))

battedball <- battedball |>
  filter(discate == "Infield (<60m)", !is.na(pfx_x), !is.na(launch_speed), !is.na(launch_angle),!is.na(sprayangle),!is.na(Distance), is.na(main_type) == F) 

battedball <- battedball |>
  select(batter, pitcher, platoon, p_throws, stand, main_type, plate_x,  plate_z, pfx_x, pfx_z, release_speed,
         sprayangle,  launch_angle, situation_cat,  year) |>
  mutate(
    batter = factor(batter), 
    pitcher = factor(pitcher),
    platoon = factor(platoon),
    p_throws = factor(p_throws),
    stand = factor(stand),
    main_type = factor(main_type), 
    situation_cat = factor(situation_cat)
  ) |>
  group_by(main_type) |>
  mutate(
    release_speed_c = release_speed - mean(release_speed, na.rm = TRUE),
    pfx_x_c = pfx_x - mean(pfx_x, na.rm = TRUE),
    pfx_z_c = pfx_z - mean(pfx_z, na.rm = TRUE)
  ) |>
  ungroup()

##R ########
R <- battedball |>
  filter(stand == "R") |>
  select(-stand)

train1_R <- R |>
  filter(year != "2025")
test1_R <- R |>
  filter(year == "2025")

form_qgam_R <- sprayangle ~
  plate_x + plate_z +
  platoon +
  situation_cat +
  main_type +
  release_speed_c*main_type +
  pfx_x_c*main_type +
  pfx_z_c*main_type +
  pfx_x_c*platoon


qgam_R_10 <- qgam(form_qgam_R, data = train1_R, qu = 0.10)
qgam_R_25 <- qgam(form_qgam_R, data = train1_R, qu = 0.25)
qgam_R_50 <- qgam(form_qgam_R, data = train1_R, qu = 0.5)
qgam_R_75 <- qgam(form_qgam_R, data = train1_R, qu = 0.75)
qgam_R_90 <- qgam(form_qgam_R, data = train1_R, qu = 0.90)


#################

L <- battedball |>
  filter(stand == "L") |>
  select(-stand)

train1_L <- L |>
  filter(year != "2025")
test1_L <- L |>
  filter(year == "2025")


form_qgam_L <- sprayangle ~
  plate_x + plate_z +
  platoon +
  situation_cat +
  main_type +
  release_speed_c*main_type +
  pfx_x_c*main_type +
  pfx_z_c*main_type +
  pfx_x_c*platoon

qgam_L_10 <- qgam(form_qgam_L, data = train1_L, qu = 0.10)
qgam_L_25 <- qgam(form_qgam_L, data = train1_L, qu = 0.25)
qgam_L_50 <- qgam(form_qgam_L, data = train1_L, qu = 0.5)
qgam_L_75 <- qgam(form_qgam_L, data = train1_L, qu = 0.75)
qgam_L_90 <- qgam(form_qgam_L, data = train1_L, qu = 0.90)

#validation

pred_10_R  <- predict(qgam_R_10, newdata  = test1_R)
pred_25_R  <- predict(qgam_R_25, newdata  = test1_R)
pred_50_R  <- predict(qgam_R_50, newdata  = test1_R)
pred_75_R  <- predict(qgam_R_75, newdata  = test1_R)
pred_90_R  <- predict(qgam_R_90, newdata  = test1_R)

pred_10_L  <- predict(qgam_L_10, newdata  = test1_L)
pred_25_L  <- predict(qgam_L_25, newdata  = test1_L)
pred_50_L  <- predict(qgam_L_50, newdata  = test1_L)
pred_75_L  <- predict(qgam_L_75, newdata  = test1_L)
pred_90_L  <- predict(qgam_L_90, newdata  = test1_L)

pinball <- function(y, yhat, tau){
  mean(ifelse(y >= yhat,
              tau * (y - yhat),
              (tau - 1) * (y - yhat)))
}

null_pred <- function(y, tau) {
  quantile(y, probs = tau)
}


eval_R <- tibble(
  tau = c(0.10, 0.25, 0.50, 0.75, 0.90),
  pinball = c(
    pinball(test1_R$sprayangle, pred_10_R, 0.10),
    pinball(test1_R$sprayangle, pred_25_R, 0.25),
    pinball(test1_R$sprayangle, pred_50_R, 0.50),
    pinball(test1_R$sprayangle, pred_75_R, 0.75),
    pinball(test1_R$sprayangle, pred_90_R, 0.90)
  ),
  null_pinball = c(
    pinball(test1_R$sprayangle, null_pred(train1_R$sprayangle, 0.10), 0.10),
    pinball(test1_R$sprayangle, null_pred(train1_R$sprayangle, 0.25), 0.25),
    pinball(test1_R$sprayangle, null_pred(train1_R$sprayangle, 0.50), 0.50),
    pinball(test1_R$sprayangle, null_pred(train1_R$sprayangle, 0.75), 0.75),
    pinball(test1_R$sprayangle, null_pred(train1_R$sprayangle, 0.90), 0.90)
  ),
  calibration = c(
    mean(test1_R$sprayangle <= pred_10_R),
    mean(test1_R$sprayangle <= pred_25_R),
    mean(test1_R$sprayangle <= pred_50_R),
    mean(test1_R$sprayangle <= pred_75_R),
    mean(test1_R$sprayangle <= pred_90_R))) |>
  mutate(calibration_error = (calibration - tau),
    r2 = 1-(pinball/null_pinball)) 
eval_R

eval_L <- tibble(
  tau = c(0.10, 0.25, 0.50, 0.75, 0.90),
  pinball = c(
    pinball(test1_L$sprayangle, pred_10_L, 0.10),
    pinball(test1_L$sprayangle, pred_25_L, 0.25),
    pinball(test1_L$sprayangle, pred_50_L, 0.50),
    pinball(test1_L$sprayangle, pred_75_L, 0.75),
    pinball(test1_L$sprayangle, pred_90_L, 0.90)
  ),
  null_pinball = c(
    pinball(test1_L$sprayangle, null_pred(train1_L$sprayangle, 0.10), 0.10),
    pinball(test1_L$sprayangle, null_pred(train1_L$sprayangle, 0.25), 0.25),
    pinball(test1_L$sprayangle, null_pred(train1_L$sprayangle, 0.50), 0.50),
    pinball(test1_L$sprayangle, null_pred(train1_L$sprayangle, 0.75), 0.75),
    pinball(test1_L$sprayangle, null_pred(train1_L$sprayangle, 0.90), 0.90)
  ),
  calibration = c(
    mean(test1_L$sprayangle <= pred_10_L),
    mean(test1_L$sprayangle <= pred_25_L),
    mean(test1_L$sprayangle <= pred_50_L),
    mean(test1_L$sprayangle <= pred_75_L),
    mean(test1_L$sprayangle <= pred_90_L))) |>
  mutate(calibration_error = (calibration - tau),
    r2 = 1-(pinball/null_pinball)) 
eval_L

# figure 4 ##################

models_R <- list("10%" = qgam_R_10, "25%" = qgam_R_25, "50%" = qgam_R_50, "75%" = qgam_R_75, "90%" = qgam_R_90)
models_L <- list("10%" = qgam_L_10, "25%" = qgam_L_25, "50%" = qgam_L_50, "75%" = qgam_L_75, "90%" = qgam_L_90)

save(models_R, models_L, file = here("data", "QR_main_model.RData"))

format_combined_results <- function(m_list) {
  map_df(m_list, function(m) {
    sum_m <- summary(m)
    as.data.frame(sum_m$p.table) |>
      tibble::rownames_to_column("term") |>
      mutate(
        est = sprintf("%.1f", Estimate),
        lower = sprintf("%.1f", Estimate - 1.96 * `Std. Error`),
        upper = sprintf("%.1f", Estimate + 1.96 * `Std. Error`),
        sig = case_when(`Pr(>|z|)` < 0.001 ~ "*", TRUE ~ ""),
        display_text = paste0(est, sig, "@[", lower, ", ", upper, "]"),
        display_text = gsub("-", "−", display_text)
      ) |>
      select(term, display_text)
  }, .id = "quantile") |>
    pivot_wider(names_from = quantile, values_from = display_text)
}

windowsFonts(ASE_FONT = windowsFont("Arial"))
family <- "ASE_FONT"
FIG_SIZE_MM <- 174
TEXT_MM <- 2.6
LINE_MM <- 0.2
POINT_MM <- 0.8
MARGIN_MM <- 0

plot_data_R <- format_combined_results(models_R) |>
  pivot_longer(cols = -term, names_to = "quantile", values_to = "text") |>
  mutate(side = "RHB")
plot_data_L <- format_combined_results(models_L) |>
  pivot_longer(cols = -term, names_to = "quantile", values_to = "text") |>
  mutate(side = "LHB")

plot_data_clean <- bind_rows(plot_data_R, plot_data_L) |>
  filter(term %in% c("plate_x", "plate_z")) |>
  mutate(
    text = str_replace_all(text, "−", "-"),
    coef = as.numeric(str_extract(text, "^[-0-9.]+")),
    lower = as.numeric(str_extract(text, "(?<=\\[)[-0-9.]+")),
    upper = as.numeric(str_extract(text, "(?<=,\\s)[-0-9.]+|(?<=,)[-0-9.]+")),
    stars = str_extract(text, "(\\*\\*\\*|\\*\\*|\\*)"),
    stars = ifelse(is.na(stars), "", stars),
    quantile_num = as.numeric(str_remove(quantile, "%")) / 100,
    term = case_when(term == "plate_x" ~ "a. Horizontal pitch location", term == "plate_z" ~ "b. Vertical pitch location", TRUE ~ term),
    term = factor(term, levels = c("a. Horizontal pitch location", "b. Vertical pitch location"))
  ) |>
  group_by(term) |>
  mutate(offset_val = (max(upper, na.rm = TRUE) - min(lower, na.rm = TRUE)) * 0.025) |>
  group_by(term, quantile_num) |>
  mutate(is_higher = coef == max(coef)) |>
  ungroup() |>
  mutate(y_star = ifelse(is_higher, upper + offset_val, lower - offset_val), v_just = ifelse(is_higher, 0, 1))

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

g <- ggplot(plot_data_clean, aes(x = quantile_num, y = coef, color = side, fill = side)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(linewidth = mm_line(LINE_MM)) +
  geom_point(aes(shape = side), size = mm_line(POINT_MM)) +
  geom_text(aes(y = y_star, label = stars, vjust = v_just), color = "black", size = TEXT_MM, fontface = "bold", family = "ASE_FONT", show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "black", linewidth = mm_line(LINE_MM * 0.8), alpha = 0.6) +
  facet_wrap(~term, ncol = 2) +
  scale_x_continuous(breaks = c(0.10, 0.25, 0.50, 0.75, 0.90), labels = c(0.10, 0.25, 0.50, 0.75, 0.90)) +
  scale_color_manual(values = c("RHB" = "#1f77b4", "LHB" = "#d62728"), name = "Batter handedness") +
  scale_fill_manual(values = c("RHB" = "#1f77b4", "LHB" = "#d62728"), name = "Batter handedness") +
  scale_shape_manual(values = c("RHB" = 16, "LHB" = 15), name = "Batter handedness") +
  labs(x = "Quantiles", y = "Estimated coefficient") +scale_y_continuous(breaks = scales::breaks_width(10)) +
  theme_classic() +
  theme(
    text = element_text(size = mm_text_theme(TEXT_MM), family = "ASE_FONT", colour = "black"),
    plot.title = element_text(hjust = 0, face = "bold", size = mm_text_theme(TEXT_MM), family = "ASE_FONT", colour = "black"),
    strip.background = element_blank(),
    strip.text.x = element_text(angle = 0, hjust = 0, face = "bold", size = mm_text_theme(TEXT_MM), family = "ASE_FONT", colour = "black"),
    axis.line = element_line(linewidth = mm_line(LINE_MM), colour = "black"),
    axis.ticks = element_line(linewidth = mm_line(LINE_MM), colour = "black"),
    panel.grid.major = element_line(color = "gray88", linewidth = mm_line(LINE_MM * 0.8)),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = mm_text_theme(TEXT_MM), family = "ASE_FONT", colour = "black"),
    legend.background = element_rect(colour = "black", fill = "white", linewidth = mm_line(LINE_MM * 0.2))
  )

print(g)

ggsave("Fig4.tiff", plot = g, dpi = 300, width = 164, height = 80, units = "mm", 
       bg = "white", limitsize = FALSE, compression = "lzw", device = "tiff")

# figure 5 ##################

grid_data <- expand.grid(
  platoon = "same",
  plate_x = c(-0.2, 0, 0.2),
  plate_z = c(0.55,0.75, 0.95),
  pfx_x_c = 0,
  pfx_z_c = 0,
  release_speed_c = 0,
  p_throws = levels(train1_R$p_throws)[2],
  main_type = levels(train1_R$main_type)[1],
  situation_cat = levels(train1_R$situation_cat)[2]
)

model_list_R <- list(
  "tau_0.10" = qgam_R_10, 
  "tau_0.25" = qgam_R_25, 
  "tau_0.50" = qgam_R_50, 
  "tau_0.75" = qgam_R_75, 
  "tau_0.90" = qgam_R_90
)

model_list_L <- list(
  "tau_0.10" = qgam_L_10, 
  "tau_0.25" = qgam_L_25, 
  "tau_0.50" = qgam_L_50, 
  "tau_0.75" = qgam_L_75, 
  "tau_0.90" = qgam_L_90
)

model_list_R <- list("tau_0.10" = qgam_R_10, "tau_0.25" = qgam_R_25, "tau_0.50" = qgam_R_50, "tau_0.75" = qgam_R_75, "tau_0.90" = qgam_R_90)
model_list_L <- list("tau_0.10" = qgam_L_10, "tau_0.25" = qgam_L_25, "tau_0.50" = qgam_L_50, "tau_0.75" = qgam_L_75, "tau_0.90" = qgam_L_90)


get_predictions <- function(model_list, grid_df) {
  imap_dfr(model_list, function(m, label) {
    pred <- predict(m, newdata = grid_df, se.fit = TRUE)
    grid_df |>
      mutate(
        pred_value = pred$fit,
        se_value   = pred$se.fit,
        lower      = pred_value - 1.96 * se_value,
        upper      = pred_value + 1.96 * se_value,
        tau        = label
      )
  })
}

plot_smooth_R <- get_predictions(model_list_R, grid_data)
plot_smooth_L <- get_predictions(model_list_L, grid_data)

zone_x <- (17 + 2.9) * 0.0254 / 3 / 2
plot_smooth_R <- plot_smooth_R |>
  mutate(
    New_zone = case_when(
      plate_x < -zone_x & plate_z >= 0.859 ~ 1L,
      plate_x >= -zone_x & plate_x <= zone_x & plate_z >= 0.859 ~ 2L,
      plate_x > zone_x & plate_z >= 0.859 ~ 3L,
      plate_x < -zone_x & plate_z >= 0.673  & plate_z < 0.859 ~ 4L,
      plate_x >= -zone_x & plate_x <= zone_x & plate_z >= 0.673  & plate_z < 0.859 ~ 5L,
      plate_x > zone_x & plate_z >= 0.673  & plate_z < 0.859 ~ 6L,
      plate_x < -zone_x & plate_z < 0.673  ~ 7L,
      plate_x >= -zone_x & plate_x <= zone_x & plate_z < 0.673  ~ 8L,
      plate_x > zone_x & plate_z < 0.673  ~ 9L,
      TRUE ~ NA_integer_
    )
  ) |>
  mutate(
    New_zone = factor(New_zone, levels = 9:1)
  )

plot_smooth_R$tau <- factor(
  plot_smooth_R$tau,
  levels = c("tau_0.10","tau_0.25","tau_0.50","tau_0.75","tau_0.90"),
  labels = c("0.10","0.25","0.50","0.75","0.90")
)

plot_smooth_L <- plot_smooth_L |>
  mutate(
    New_zone = case_when(
      plate_x < -zone_x & plate_z >= 0.859 ~ 1L,
      plate_x >= -zone_x & plate_x <= zone_x & plate_z >= 0.859 ~ 2L,
      plate_x > zone_x & plate_z >= 0.859 ~ 3L,
      plate_x < -zone_x & plate_z >= 0.673  & plate_z < 0.859 ~ 4L,
      plate_x >= -zone_x & plate_x <= zone_x & plate_z >= 0.673  & plate_z < 0.859 ~ 5L,
      plate_x > zone_x & plate_z >= 0.673  & plate_z < 0.859 ~ 6L,
      plate_x < -zone_x & plate_z < 0.673  ~ 7L,
      plate_x >= -zone_x & plate_x <= zone_x & plate_z < 0.673  ~ 8L,
      plate_x > zone_x & plate_z < 0.673  ~ 9L,
      TRUE ~ NA_integer_
    )
  ) |>
  mutate(
    New_zone = factor(New_zone, levels = 9:1)
  )

plot_smooth_L$tau <- factor(
  plot_smooth_L$tau,
  levels = c("tau_0.10","tau_0.25","tau_0.50","tau_0.75","tau_0.90"),
  labels = c("0.10","0.25","0.50","0.75","0.90")
)

theme_mm <- theme_classic() +
  theme(
    text = element_text(
      size   = mm_text_theme(TEXT_MM),
      family = "ASE_FONT",
      colour = "black"
    ),
    plot.title = element_text(hjust = 0, face="bold", size   = mm_text_theme(TEXT_MM), family = "ASE_FONT", colour = "black"), 
    strip.background = element_blank(),
    strip.text.x = element_text(angle = 0, hjust = 0, face="bold",  size   = mm_text_theme(TEXT_MM), family = "ASE_FONT", colour = "black"),
  )

g1 <- ggplot(
  plot_smooth_R,
  aes(x = pred_value, y = New_zone, fill = tau, shape = tau, group = New_zone)) +
  geom_line(size = LINE_MM, alpha = 0.8) +
  geom_errorbar(aes(xmin = lower, xmax = upper, color = tau),height = 0, linewidth = 2.2, alpha = 0.8) +    
  geom_point(size = POINT_MM * 1.5, color = "white", stroke = 0.3) +
  geom_point(size = POINT_MM * 1.3, col = "black", stroke = 0.2) +
  scale_color_viridis_d(name = expression(tau), direction = -1) +
  scale_fill_viridis_d(name = expression(tau), direction = -1) +
  scale_shape_manual(name = expression(tau), values = c(21, 22, 23, 24, 25)) +
  labs(shape = expression(tau)) +
  theme_mm + xlab("Predicted spray angle [°]") + ylab("Pitch zones") +
  theme(
    legend.position = "right",
    panel.grid.major.x = element_line(color = "gray90", linewidth = mm_line(LINE_MM * 0.5)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(colour = "black", fill = "white", linewidth = mm_line(LINE_MM * 0.1))
  ) +
  scale_x_reverse(limits = c(55, -55), breaks = seq(-45, 45, by = 15)) +
  ggtitle("a. RHB")

g2 <- ggplot(plot_smooth_L,
             aes(x = pred_value,y = New_zone,fill = tau,shape = tau,group = New_zone))+ 
  geom_line(size = LINE_MM, alpha = 0.8) +
  geom_errorbar(aes(xmin = lower, xmax = upper, color = tau),height = 0, linewidth = 2.2, alpha = 0.8) +    
  geom_point(size = POINT_MM * 1.5, color = "white", stroke = 0.3) +
  geom_point(size = POINT_MM * 1.3, col = "black", stroke = 0.2) +
  scale_color_viridis_d(name = expression(tau), direction = -1) +
  scale_fill_viridis_d(name = expression(tau), direction = -1) +
  scale_shape_manual(name = expression(tau), values = c(21, 22, 23, 24, 25)) +
  labs(shape = expression(tau)) +
  theme_mm + xlab("Predicted spray angle [°]") + ylab("Pitch zones") +
  theme(
    legend.position = "right",
    panel.grid.major.x = element_line(
      color = "gray90",
      linewidth = mm_line(LINE_MM * 0.5)
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(
      colour = "black",
      fill = "white",
      linewidth = mm_line(LINE_MM*0.1)
    )
  ) +scale_x_reverse(limits = c(55, -55),breaks = seq(-45, 45, by = 15)) +  ggtitle("b. LHB")


g3 <- (g1 | g2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

g3

ggsave(file = "Fig5.tiff", plot = g3, dpi = 300,
       width = 174, height = 80, units = "mm",
       bg = "white",limitsize = FALSE, compression = "lzw", device = "tiff")
