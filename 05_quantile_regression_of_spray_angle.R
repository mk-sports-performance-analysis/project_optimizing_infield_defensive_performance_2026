# ============================================================
#  quantile regression
# ============================================================
clear1<-try(clearAll(), silent = T) 
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggsignif)
library(viridis)
library(quantreg)
library(DescTools)
library(patchwork)


# Load processed data
load(here("data", "battedball.RData"))
source(here("R", "load_functions.R"))

battedball <- battedball |>
  filter(discate == "Infield (≤60m)", !is.na(pfx_x), !is.na(launch_speed), !is.na(launch_angle),!is.na(sprayangle),!is.na(Distance), is.na(main_type) == F) 

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

form_qr_R <- sprayangle ~
  plate_x + plate_z +
  platoon +
  situation_cat +
  main_type +
  release_speed_c*main_type +
  pfx_x_c*main_type +
  pfx_z_c*main_type +
  pfx_x_c*platoon


qr_R_10 <- rq(form_qr_R, data = train1_R, tau = 0.10)
qr_R_25 <- rq(form_qr_R, data = train1_R, tau = 0.25)
qr_R_50 <- rq(form_qr_R, data = train1_R, tau = 0.5)
qr_R_75 <- rq(form_qr_R, data = train1_R, tau = 0.75)
qr_R_90 <- rq(form_qr_R, data = train1_R, tau = 0.90)


#################

L <- battedball |>
  filter(stand == "L") |>
  select(-stand)

train1_L <- L |>
  filter(year != "2025")
test1_L <- L |>
  filter(year == "2025")


form_qr_L <- sprayangle ~
  plate_x + plate_z +
  platoon +
  situation_cat +
  main_type +
  release_speed_c*main_type +
  pfx_x_c*main_type +
  pfx_z_c*main_type +
  pfx_x_c*platoon

qr_L_10 <- rq(form_qr_L, data = train1_L, tau = 0.10)
qr_L_25 <- rq(form_qr_L, data = train1_L, tau = 0.25)
qr_L_50 <- rq(form_qr_L, data = train1_L, tau = 0.5)
qr_L_75 <- rq(form_qr_L, data = train1_L, tau = 0.75)
qr_L_90 <- rq(form_qr_L, data = train1_L, tau = 0.90)

#validation

pred_10_R  <- predict(qr_R_10, newdata  = test1_R)
pred_25_R  <- predict(qr_R_25, newdata  = test1_R)
pred_50_R  <- predict(qr_R_50, newdata  = test1_R)
pred_75_R  <- predict(qr_R_75, newdata  = test1_R)
pred_90_R  <- predict(qr_R_90, newdata  = test1_R)

pred_10_L  <- predict(qr_L_10, newdata  = test1_L)
pred_25_L  <- predict(qr_L_25, newdata  = test1_L)
pred_50_L  <- predict(qr_L_50, newdata  = test1_L)
pred_75_L  <- predict(qr_L_75, newdata  = test1_L)
pred_90_L  <- predict(qr_L_90, newdata  = test1_L)

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
  "tau_0.10" = qr_R_10, 
  "tau_0.25" = qr_R_25, 
  "tau_0.50" = qr_R_50, 
  "tau_0.75" = qr_R_75, 
  "tau_0.90" = qr_R_90
)

model_list_L <- list(
  "tau_0.10" = qr_L_10, 
  "tau_0.25" = qr_L_25, 
  "tau_0.50" = qr_L_50, 
  "tau_0.75" = qr_L_75, 
  "tau_0.90" = qr_L_90
)

model_list_R <- list("tau_0.10" = qr_R_10, "tau_0.25" = qr_R_25, "tau_0.50" = qr_R_50, "tau_0.75" = qr_R_75, "tau_0.90" = qr_R_90)
model_list_L <- list("tau_0.10" = qr_L_10, "tau_0.25" = qr_L_25, "tau_0.50" = qr_L_50, "tau_0.75" = qr_L_75, "tau_0.90" = qr_L_90)


get_predictions <- function(model_list, grid_df) {
  imap_dfr(model_list, function(m, label) {
    pred <- predict(m, newdata = grid_df, interval = "confidence", se = "boot")
    grid_df |>
      mutate(
        pred_value = pred[, "fit"],
        lower      = pred[, "lower"],
        upper      = pred[, "higher"],
        tau        = label
      )
  })
}

plot_smooth_R <- get_predictions(model_list_R, grid_data)
plot_smooth_L <- get_predictions(model_list_L, grid_data)
