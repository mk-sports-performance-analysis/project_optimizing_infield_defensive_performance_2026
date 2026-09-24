library(tidyverse)
library(ggsignif)
library(viridis)
library(xgboost)
library(caret)
library(ggnewscale)
library(patchwork)
library(here)

source(here("R", "load_functions.R"))
load(here("data", "Outcome prediction model.RData"))
load(here("data", "responsibility model.RData"))
load(here("data", "l_2025_league.RData"))

d <- test_R |> group_by(New_zone) |> summarise(po = mean(p0))
d2 <- test_R |> group_by(New_zone) |> summarise(rawp = sum(y_result) / n()) |> left_join(d, by = "New_zone")

read_zone_R <- function(zonenum) {
  file <- here("data", paste0("res_R_zone", zonenum, ".rds"))
  message("Reading: ", file)
  bind_rows(readRDS(file)) |> mutate(zone = zonenum)
}

res_all_zone <- map_dfr(1:9, read_zone_R)
load(here("data", "grid_all_ori_R.RData"))

grid_all <- grid_all_ori |> select(grid_id, dx_3B, dy_3B, dx_SS, dy_SS, dx_2B, dy_2B, dx_1B, dy_1B)
res_all_zone <- res_all_zone |> left_join(grid_all, by = "grid_id")

opt_by_zone <- res_all_zone |> group_by(zone) |> slice_min(pmean, n = 1, with_ties = FALSE) |> ungroup()

opt_R_long <- opt_by_zone |>
  pivot_longer(matches("^l(SS|1B|2B|3B)_[xy]$"), names_to = c("position", "coord"),
               names_pattern = "l(SS|1B|2B|3B)_([xy])", values_to = "value") |>
  pivot_wider(names_from = coord, values_from = value)

opt_R_long_sum <- opt_R_long |> group_by(zone) |> summarise(pmean = mean(pmean))

dR3 <- opt_R_long_sum |>
  rename(New_zone = zone) |>
  mutate(New_zone = factor(New_zone)) |>
  left_join(d2, by = "New_zone") |>
  mutate(diffp1 = pmean - rawp, diffp2 = pmean - po, error1 = po - rawp)

l <- l_2025_league |>
  filter(position %in% c("3B", "SS", "2B", "1B")) |>
  group_by(position, stand) |>
  summarise(x = mean(lx), y = mean(ly), .groups = "drop")

lall <- l |> uncount(9, .id = "zone")

IF_base_R <- l |> filter(stand == "R")

IF_base_wide <- l |>
  group_by(position, stand) |>
  summarise(lx0 = mean(x), ly0 = mean(y), .groups = "drop") |>
  pivot_wider(id_cols = stand, names_from = position, values_from = c(lx0, ly0),
              names_glue = "{.value}_{position}")

test_R2 <- test_R |>
  mutate(vforward = launch_speed * cos(deg2rad(launch_angle)), battedball_id = row_number()) |>
  select(New_zone, stand, vz:lly_raw, launch_speed, launch_angle, battedball_id, p0, is_foul_tag,
         hp_to_1b, vforward, y_position:home_teamWSH, arm_fielder_3:arm_fielder_6) |>
  left_join(IF_base_wide, by = "stand") |>
  select(-stand)

opt_comb <- count(opt_R_long, grid_id, zone)
allop <- res_all_zone |> merge(opt_comb, by = c("grid_id", "zone"))

infielders <- c("1B", "2B", "3B", "SS")
outfielders <- c("LF", "CF", "RF")
pos_cols <- c(infielders, outfielders)
num_class <- 7

res_R <- data.frame()

for (zonenum in 1:9) {
  zonedata <- test_R2 |> filter(New_zone == zonenum)
  g <- allop |> filter(zone == zonenum) |> select(-c(lSS_x:l3B_y), -n)
  
  dat_exp <- crossing(zonedata, g) |>
    mutate(
      theta0_SS = atan2(-ly0_SS, -lx0_SS) - pi / 2,
      lx_SS = lx0_SS + dx_SS * cos(theta0_SS) - dy_SS * sin(theta0_SS),
      ly_SS = ly0_SS + dx_SS * sin(theta0_SS) + dy_SS * cos(theta0_SS),
      theta0_1B = atan2(-ly0_1B, -lx0_1B) - pi / 2,
      lx_1B = lx0_1B + dx_1B * cos(theta0_1B) - dy_1B * sin(theta0_1B),
      ly_1B = ly0_1B + dx_1B * sin(theta0_1B) + dy_1B * cos(theta0_1B),
      theta0_2B = atan2(-ly0_2B, -lx0_2B) - pi / 2,
      lx_2B = lx0_2B + dx_2B * cos(theta0_2B) - dy_2B * sin(theta0_2B),
      ly_2B = ly0_2B + dx_2B * sin(theta0_2B) + dy_2B * cos(theta0_2B),
      theta0_3B = atan2(-ly0_3B, -lx0_3B) - pi / 2,
      lx_3B = lx0_3B + dx_3B * cos(theta0_3B) - dy_3B * sin(theta0_3B),
      ly_3B = ly0_3B + dx_3B * sin(theta0_3B) + dy_3B * cos(theta0_3B)
    ) |>
    mutate(
      pSS_dist = sqrt((llx_raw - lx_SS)^2 + (lly_raw - ly_SS)^2),
      pSS_angle = atan2((0 - lx_SS) * (lly_raw - ly_SS) - (0 - ly_SS) * (llx_raw - lx_SS),
                        (0 - lx_SS) * (llx_raw - lx_SS) + (0 - ly_SS) * (lly_raw - ly_SS)) * 180 / pi,
      pSS_1B_thtime = sqrt((lx_SS - 27.431 * cos(deg2rad(45)))^2 +
                             (ly_SS - 27.431 * sin(deg2rad(45)))^2) / arm_fielder_6,
      p1B_dist = sqrt((llx_raw - lx_1B)^2 + (lly_raw - ly_1B)^2),
      p1B_angle = atan2((0 - lx_1B) * (lly_raw - ly_1B) - (0 - ly_1B) * (llx_raw - lx_1B),
                        (0 - lx_1B) * (llx_raw - lx_1B) + (0 - ly_1B) * (lly_raw - ly_1B)) * 180 / pi,
      p1B_1B_thtime = sqrt((lx_1B - 27.431 * cos(deg2rad(45)))^2 +
                             (ly_1B - 27.431 * sin(deg2rad(45)))^2) / arm_fielder_3,
      p2B_dist = sqrt((llx_raw - lx_2B)^2 + (lly_raw - ly_2B)^2),
      p2B_angle = atan2((0 - lx_2B) * (lly_raw - ly_2B) - (0 - ly_2B) * (llx_raw - lx_2B),
                        (0 - lx_2B) * (llx_raw - lx_2B) + (0 - ly_2B) * (lly_raw - ly_2B)) * 180 / pi,
      p2B_1B_thtime = sqrt((lx_2B - 27.431 * cos(deg2rad(45)))^2 +
                             (ly_2B - 27.431 * sin(deg2rad(45)))^2) / arm_fielder_4,
      p3B_dist = sqrt((llx_raw - lx_3B)^2 + (lly_raw - ly_3B)^2),
      p3B_angle = atan2((0 - lx_3B) * (lly_raw - ly_3B) - (0 - ly_3B) * (llx_raw - lx_3B),
                        (0 - lx_3B) * (llx_raw - lx_3B) + (0 - ly_3B) * (lly_raw - ly_3B)) * 180 / pi,
      p3B_1B_thtime = sqrt((lx_3B - 27.431 * cos(deg2rad(45)))^2 +
                             (ly_3B - 27.431 * sin(deg2rad(45)))^2) / arm_fielder_5,
      fielder3_fhtime = sqrt(lx_1B^2 + ly_1B^2) / vforward,
      fielder4_fhtime = sqrt(lx_2B^2 + ly_2B^2) / vforward,
      fielder5_fhtime = sqrt(lx_3B^2 + ly_3B^2) / vforward,
      fielder6_fhtime = sqrt(lx_SS^2 + ly_SS^2) / vforward,
      min_if_dist = pmin(pSS_dist, p2B_dist, p3B_dist, p1B_dist),
      front_nearest = case_when(
        pSS_dist == min_if_dist ~ ((0 - lx_SS) * (llx_raw - lx_SS) + (0 - ly_SS) * (lly_raw - ly_SS)) > 0,
        p2B_dist == min_if_dist ~ ((0 - lx_2B) * (llx_raw - lx_2B) + (0 - ly_2B) * (lly_raw - ly_2B)) > 0,
        p3B_dist == min_if_dist ~ ((0 - lx_3B) * (llx_raw - lx_3B) + (0 - ly_3B) * (lly_raw - ly_3B)) > 0,
        p1B_dist == min_if_dist ~ ((0 - lx_1B) * (llx_raw - lx_1B) + (0 - ly_1B) * (lly_raw - ly_1B)) > 0,
        TRUE ~ FALSE
      ),
      nearest_angle = case_when(
        pSS_dist == min_if_dist ~ pSS_angle,
        p2B_dist == min_if_dist ~ p2B_angle,
        p3B_dist == min_if_dist ~ p3B_angle,
        p1B_dist == min_if_dist ~ p1B_angle,
        TRUE ~ NA_real_
      ),
      is_bounced_bip = launch_angle < 10 & min_if_dist > 5 & abs(nearest_angle) < 100
    ) |>
    select(-min_if_dist, -front_nearest, -nearest_angle)
  
  x_dat_exp <- model.matrix(
    ~ p_throwsLHP + p_throwsRHP + launch_speed + launch_angle + is_foul_tag + is_bounced_bip +
      p1B_dist + p1B_angle + p2B_dist + p2B_angle + p3B_dist + p3B_angle + pSS_dist + pSS_angle +
      dteamATL + dteamAZ + dteamBAL + dteamBOS + dteamCHC + dteamCIN + dteamCLE + dteamCOL + dteamCWS +
      dteamDET + dteamHOU + dteamKC + dteamLAA + dteamLAD + dteamMIA + dteamMIL + dteamMIN + dteamNYM + dteamNYY +
      dteamOAK + dteamPHI + dteamPIT + dteamSD + dteamSEA + dteamSF + dteamSTL + dteamTB + dteamTEX + dteamTOR + dteamWSH +
      home_teamATL + home_teamAZ + home_teamBAL + home_teamBOS + home_teamCHC + home_teamCIN + home_teamCLE + home_teamCOL +
      home_teamCWS + home_teamDET + home_teamHOU + home_teamKC + home_teamLAA + home_teamLAD + home_teamMIA + home_teamMIL +
      home_teamMIN + home_teamNYM + home_teamNYY + home_teamOAK + home_teamPHI + home_teamPIT + home_teamSD + home_teamSEA +
      home_teamSF + home_teamSTL + home_teamTB + home_teamTEX + home_teamTOR + home_teamWSH - 1,
    data = dat_exp, na.action = na.pass
  ) |> xgb.DMatrix()
  
  pred_respon_R <- predict(model_R_respon, newdata = x_dat_exp)
  colnames(pred_respon_R) <- levels(test_R$fielderposition)
  
  probs_if <- as.matrix(pred_respon_R[, infielders])
  hit_if <- probs_if >= 0.2
  max_idx <- max.col(probs_if, ties.method = "first")
  
  dat_exp$gate_infield <- map(seq_len(nrow(dat_exp)), \(i) {
    hi <- infielders[hit_if[i, ]]
    if (length(hi) > 0) hi else infielders[max_idx[i]]
  })
  
  dat_exp <- dat_exp |>
    mutate(
      pSS_dist_eff = ifelse(map_lgl(gate_infield, ~ "SS" %in% .x), pSS_dist, NA_real_),
      pSS_angle_eff = ifelse(map_lgl(gate_infield, ~ "SS" %in% .x), pSS_angle, NA_real_),
      pSS_1B_thtime_eff = ifelse(map_lgl(gate_infield, ~ "SS" %in% .x), pSS_1B_thtime, NA_real_),
      p2B_dist_eff = ifelse(map_lgl(gate_infield, ~ "2B" %in% .x), p2B_dist, NA_real_),
      p2B_angle_eff = ifelse(map_lgl(gate_infield, ~ "2B" %in% .x), p2B_angle, NA_real_),
      p2B_1B_thtime_eff = ifelse(map_lgl(gate_infield, ~ "2B" %in% .x), p2B_1B_thtime, NA_real_),
      p3B_dist_eff = ifelse(map_lgl(gate_infield, ~ "3B" %in% .x), p3B_dist, NA_real_),
      p3B_angle_eff = ifelse(map_lgl(gate_infield, ~ "3B" %in% .x), p3B_angle, NA_real_),
      p3B_1B_thtime_eff = ifelse(map_lgl(gate_infield, ~ "3B" %in% .x), p3B_1B_thtime, NA_real_),
      p1B_dist_eff = ifelse(map_lgl(gate_infield, ~ "1B" %in% .x), p1B_dist, NA_real_),
      p1B_angle_eff = ifelse(map_lgl(gate_infield, ~ "1B" %in% .x), p1B_angle, NA_real_),
      p1B_1B_thtime_eff = ifelse(map_lgl(gate_infield, ~ "1B" %in% .x), p1B_1B_thtime, NA_real_),
      fielder3_fhtime_eff = ifelse(map_lgl(gate_infield, ~ "1B" %in% .x) & is_bounced_bip, fielder3_fhtime, NA_real_),
      fielder4_fhtime_eff = ifelse(map_lgl(gate_infield, ~ "2B" %in% .x) & is_bounced_bip, fielder4_fhtime, NA_real_),
      fielder5_fhtime_eff = ifelse(map_lgl(gate_infield, ~ "3B" %in% .x) & is_bounced_bip, fielder5_fhtime, NA_real_),
      fielder6_fhtime_eff = ifelse(map_lgl(gate_infield, ~ "SS" %in% .x) & is_bounced_bip, fielder6_fhtime, NA_real_),
      pSS_1B_thtime_eff = ifelse(is_bounced_bip, pSS_1B_thtime_eff, NA_real_),
      p2B_1B_thtime_eff = ifelse(is_bounced_bip, p2B_1B_thtime_eff, NA_real_),
      p3B_1B_thtime_eff = ifelse(is_bounced_bip, p3B_1B_thtime_eff, NA_real_),
      p1B_1B_thtime_eff = ifelse(is_bounced_bip, p1B_1B_thtime_eff, NA_real_)
    )
  
  x2_dat_exp <- dat_exp |>
    select(
      p_throwsLHP, p_throwsRHP, launch_angle, launch_speed, hp_to_1b, is_foul_tag,
      p1B_dist_eff, p1B_angle_eff, p2B_dist_eff, p2B_angle_eff, p3B_dist_eff, p3B_angle_eff,
      pSS_dist_eff, pSS_angle_eff, p1B_1B_thtime_eff, p2B_1B_thtime_eff, p3B_1B_thtime_eff,
      pSS_1B_thtime_eff, fielder3_fhtime_eff, fielder4_fhtime_eff, fielder5_fhtime_eff, fielder6_fhtime_eff,
      dteamATL:dteamWSH, home_teamATL:home_teamWSH
    ) |> as.matrix()
  
  d_R <- xgb.DMatrix(data = x2_dat_exp, missing = NA)
  dat_exp$p1 <- predict(model_final_R, newdata = d_R)
  dat_exp$change <- dat_exp$p1 - dat_exp$p0
  res_R <- rbind(res_R, dat_exp |> mutate(zone = zonenum))
}

zone_labeller <- as_labeller(\(x) sapply(x, \(val) {
  roman_num <- tolower(as.character(utils::as.roman(as.numeric(val))))
  paste0(" ", roman_num, ". zone ", val)
}))

LABEL_SIZE <- 1.8
shapes_map <- c("1B" = 21, "SS" = 22, "2B" = 24, "3B" = 25)
col1 <- viridis(2)

R_position <- lall |> filter(stand == "R") |> select(-stand) |> mutate(Formation = "Season-average")
opt_R_long_s <- opt_R_long |> select(x, y, position, zone) |> mutate(Formation = "Optimal")
R_position <- rbind(R_position, opt_R_long_s)

dL <- test_L |> group_by(New_zone) |> summarise(po = mean(p0))
dL2 <- test_L |> group_by(New_zone) |> summarise(rawp = sum(y_result) / n()) |> left_join(dL, by = "New_zone")

read_zone_L <- function(zonenum) {
  file <- here("data", paste0("res_L_zone", zonenum, ".rds"))
  message("Reading: ", file)
  bind_rows(readRDS(file)) |> mutate(zone = zonenum)
}

res_all_L_zone <- map_dfr(1:9, read_zone_L)
load(here("data", "grid_all_ori_L.RData"))

grid_Lall <- grid_all_ori |> select(grid_id, dx_3B, dy_3B, dx_SS, dy_SS, dx_2B, dy_2B, dx_1B, dy_1B)
res_all_L_zone <- res_all_L_zone |> left_join(grid_Lall, by = "grid_id")

opt_by_Lzone <- res_all_L_zone |> group_by(zone) |> slice_min(pmean, n = 1, with_ties = FALSE) |> ungroup()

opt_L_long <- opt_by_Lzone |>
  pivot_longer(matches("^l(SS|1B|2B|3B)_[xy]$"), names_to = c("position", "coord"),
               names_pattern = "l(SS|1B|2B|3B)_([xy])", values_to = "value") |>
  pivot_wider(names_from = coord, values_from = value)

opt_L_long_sum <- opt_L_long |> group_by(zone) |> summarise(pmean = mean(pmean))

dL3 <- opt_L_long_sum |>
  rename(New_zone = zone) |>
  mutate(New_zone = factor(New_zone)) |>
  left_join(dL2, by = "New_zone") |>
  mutate(diffp1 = pmean - rawp, diffp2 = pmean - po, error1 = po - rawp)

test_L2 <- test_L |>
  mutate(vforward = launch_speed * cos(deg2rad(launch_angle)), battedball_id = row_number()) |>
  select(New_zone, stand, vz:lly_raw, launch_speed, launch_angle, battedball_id, p0, is_foul_tag,
         hp_to_1b, vforward, y_position:home_teamWSH, arm_fielder_3:arm_fielder_6) |>
  left_join(IF_base_wide, by = "stand") |>
  select(-stand)

opt_Lcomb <- count(opt_L_long, grid_id, zone)
allLop <- res_all_L_zone |> merge(opt_Lcomb, by = c("grid_id", "zone"))

res_L <- data.frame()

for (zonenum in 1:9) {
  zonedata <- test_L2 |> filter(New_zone == zonenum)
  g <- allLop |> filter(zone == zonenum) |> select(-c(lSS_x:l3B_y), -n)
  
  dat_exp <- crossing(zonedata, g) |>
    mutate(
      theta0_SS = atan2(-ly0_SS, -lx0_SS) - pi / 2,
      lx_SS = lx0_SS + dx_SS * cos(theta0_SS) - dy_SS * sin(theta0_SS),
      ly_SS = ly0_SS + dx_SS * sin(theta0_SS) + dy_SS * cos(theta0_SS),
      theta0_1B = atan2(-ly0_1B, -lx0_1B) - pi / 2,
      lx_1B = lx0_1B + dx_1B * cos(theta0_1B) - dy_1B * sin(theta0_1B),
      ly_1B = ly0_1B + dx_1B * sin(theta0_1B) + dy_1B * cos(theta0_1B),
      theta0_2B = atan2(-ly0_2B, -lx0_2B) - pi / 2,
      lx_2B = lx0_2B + dx_2B * cos(theta0_2B) - dy_2B * sin(theta0_2B),
      ly_2B = ly0_2B + dx_2B * sin(theta0_2B) + dy_2B * cos(theta0_2B),
      theta0_3B = atan2(-ly0_3B, -lx0_3B) - pi / 2,
      lx_3B = lx0_3B + dx_3B * cos(theta0_3B) - dy_3B * sin(theta0_3B),
      ly_3B = ly0_3B + dx_3B * sin(theta0_3B) + dy_3B * cos(theta0_3B)
    ) |>
    mutate(
      pSS_dist = sqrt((llx_raw - lx_SS)^2 + (lly_raw - ly_SS)^2),
      pSS_angle = atan2((0 - lx_SS) * (lly_raw - ly_SS) - (0 - ly_SS) * (llx_raw - lx_SS),
                        (0 - lx_SS) * (llx_raw - lx_SS) + (0 - ly_SS) * (lly_raw - ly_SS)) * 180 / pi,
      pSS_1B_thtime = sqrt((lx_SS - 27.431 * cos(deg2rad(45)))^2 + (ly_SS - 27.431 * sin(deg2rad(45)))^2) / arm_fielder_6,
      p1B_dist = sqrt((llx_raw - lx_1B)^2 + (lly_raw - ly_1B)^2),
      p1B_angle = atan2((0 - lx_1B) * (lly_raw - ly_1B) - (0 - ly_1B) * (llx_raw - lx_1B),
                        (0 - lx_1B) * (llx_raw - lx_1B) + (0 - ly_1B) * (lly_raw - ly_1B)) * 180 / pi,
      p1B_1B_thtime = sqrt((lx_1B - 27.431 * cos(deg2rad(45)))^2 + (ly_1B - 27.431 * sin(deg2rad(45)))^2) / arm_fielder_3,
      p2B_dist = sqrt((llx_raw - lx_2B)^2 + (lly_raw - ly_2B)^2),
      p2B_angle = atan2((0 - lx_2B) * (lly_raw - ly_2B) - (0 - ly_2B) * (llx_raw - lx_2B),
                        (0 - lx_2B) * (llx_raw - lx_2B) + (0 - ly_2B) * (lly_raw - ly_2B)) * 180 / pi,
      p2B_1B_thtime = sqrt((lx_2B - 27.431 * cos(deg2rad(45)))^2 + (ly_2B - 27.431 * sin(deg2rad(45)))^2) / arm_fielder_4,
      p3B_dist = sqrt((llx_raw - lx_3B)^2 + (lly_raw - ly_3B)^2),
      p3B_angle = atan2((0 - lx_3B) * (lly_raw - ly_3B) - (0 - ly_3B) * (llx_raw - lx_3B),
                        (0 - lx_3B) * (llx_raw - lx_3B) + (0 - ly_3B) * (lly_raw - ly_3B)) * 180 / pi,
      p3B_1B_thtime = sqrt((lx_3B - 27.431 * cos(deg2rad(45)))^2 + (ly_3B - 27.431 * sin(deg2rad(45)))^2) / arm_fielder_5,
      fielder3_fhtime = sqrt(lx_1B^2 + ly_1B^2) / vforward,
      fielder4_fhtime = sqrt(lx_2B^2 + ly_2B^2) / vforward,
      fielder5_fhtime = sqrt(lx_3B^2 + ly_3B^2) / vforward,
      fielder6_fhtime = sqrt(lx_SS^2 + ly_SS^2) / vforward,
      min_if_dist = pmin(pSS_dist, p2B_dist, p3B_dist, p1B_dist),
      front_nearest = case_when(
        pSS_dist == min_if_dist ~ ((0 - lx_SS) * (llx_raw - lx_SS) + (0 - ly_SS) * (lly_raw - ly_SS)) > 0,
        p2B_dist == min_if_dist ~ ((0 - lx_2B) * (llx_raw - lx_2B) + (0 - ly_2B) * (lly_raw - ly_2B)) > 0,
        p3B_dist == min_if_dist ~ ((0 - lx_3B) * (llx_raw - lx_3B) + (0 - ly_3B) * (lly_raw - ly_3B)) > 0,
        p1B_dist == min_if_dist ~ ((0 - lx_1B) * (llx_raw - lx_1B) + (0 - ly_1B) * (lly_raw - ly_1B)) > 0,
        TRUE ~ FALSE
      ),
      nearest_angle = case_when(
        pSS_dist == min_if_dist ~ pSS_angle,
        p2B_dist == min_if_dist ~ p2B_angle,
        p3B_dist == min_if_dist ~ p3B_angle,
        p1B_dist == min_if_dist ~ p1B_angle,
        TRUE ~ NA_real_
      ),
      is_bounced_bip = launch_angle < 10 & min_if_dist > 5 & abs(nearest_angle) < 100
    ) |>
    select(-min_if_dist, -front_nearest, -nearest_angle)
  
  x_dat_exp <- model.matrix(
    ~ p_throwsLHP + p_throwsRHP + launch_speed + launch_angle + is_foul_tag + is_bounced_bip +
      p1B_dist + p1B_angle + p2B_dist + p2B_angle + p3B_dist + p3B_angle + pSS_dist + pSS_angle +
      dteamATL + dteamAZ + dteamBAL + dteamBOS + dteamCHC + dteamCIN + dteamCLE + dteamCOL + dteamCWS +
      dteamDET + dteamHOU + dteamKC + dteamLAA + dteamLAD + dteamMIA + dteamMIL + dteamMIN + dteamNYM + dteamNYY +
      dteamOAK + dteamPHI + dteamPIT + dteamSD + dteamSEA + dteamSF + dteamSTL + dteamTB + dteamTEX + dteamTOR + dteamWSH +
      home_teamATL + home_teamAZ + home_teamBAL + home_teamBOS + home_teamCHC + home_teamCIN + home_teamCLE + home_teamCOL +
      home_teamCWS + home_teamDET + home_teamHOU + home_teamKC + home_teamLAA + home_teamLAD + home_teamMIA + home_teamMIL +
      home_teamMIN + home_teamNYM + home_teamNYY + home_teamOAK + home_teamPHI + home_teamPIT + home_teamSD + home_teamSEA +
      home_teamSF + home_teamSTL + home_teamTB + home_teamTEX + home_teamTOR + home_teamWSH - 1,
    data = dat_exp, na.action = na.pass
  ) |> xgb.DMatrix()
  
  pred_respon_L <- predict(model_L_respon, newdata = x_dat_exp)
  colnames(pred_respon_L) <- levels(test_L$fielderposition)
  
  probs_if <- as.matrix(pred_respon_L[, infielders])
  hit_if <- probs_if >= 0.2
  max_idx <- max.col(probs_if, ties.method = "first")
  
  dat_exp$gate_infield <- map(seq_len(nrow(dat_exp)), \(i) {
    hi <- infielders[hit_if[i, ]]
    if (length(hi) > 0) hi else infielders[max_idx[i]]
  })
  
  dat_exp <- dat_exp |>
    mutate(
      pSS_dist_eff = ifelse(map_lgl(gate_infield, ~ "SS" %in% .x), pSS_dist, NA_real_),
      pSS_angle_eff = ifelse(map_lgl(gate_infield, ~ "SS" %in% .x), pSS_angle, NA_real_),
      pSS_1B_thtime_eff = ifelse(map_lgl(gate_infield, ~ "SS" %in% .x), pSS_1B_thtime, NA_real_),
      p2B_dist_eff = ifelse(map_lgl(gate_infield, ~ "2B" %in% .x), p2B_dist, NA_real_),
      p2B_angle_eff = ifelse(map_lgl(gate_infield, ~ "2B" %in% .x), p2B_angle, NA_real_),
      p2B_1B_thtime_eff = ifelse(map_lgl(gate_infield, ~ "2B" %in% .x), p2B_1B_thtime, NA_real_),
      p3B_dist_eff = ifelse(map_lgl(gate_infield, ~ "3B" %in% .x), p3B_dist, NA_real_),
      p3B_angle_eff = ifelse(map_lgl(gate_infield, ~ "3B" %in% .x), p3B_angle, NA_real_),
      p3B_1B_thtime_eff = ifelse(map_lgl(gate_infield, ~ "3B" %in% .x), p3B_1B_thtime, NA_real_),
      p1B_dist_eff = ifelse(map_lgl(gate_infield, ~ "1B" %in% .x), p1B_dist, NA_real_),
      p1B_angle_eff = ifelse(map_lgl(gate_infield, ~ "1B" %in% .x), p1B_angle, NA_real_),
      p1B_1B_thtime_eff = ifelse(map_lgl(gate_infield, ~ "1B" %in% .x), p1B_1B_thtime, NA_real_),
      fielder3_fhtime_eff = ifelse(map_lgl(gate_infield, ~ "1B" %in% .x) & is_bounced_bip, fielder3_fhtime, NA_real_),
      fielder4_fhtime_eff = ifelse(map_lgl(gate_infield, ~ "2B" %in% .x) & is_bounced_bip, fielder4_fhtime, NA_real_),
      fielder5_fhtime_eff = ifelse(map_lgl(gate_infield, ~ "3B" %in% .x) & is_bounced_bip, fielder5_fhtime, NA_real_),
      fielder6_fhtime_eff = ifelse(map_lgl(gate_infield, ~ "SS" %in% .x) & is_bounced_bip, fielder6_fhtime, NA_real_),
      pSS_1B_thtime_eff = ifelse(is_bounced_bip, pSS_1B_thtime_eff, NA_real_),
      p2B_1B_thtime_eff = ifelse(is_bounced_bip, p2B_1B_thtime_eff, NA_real_),
      p3B_1B_thtime_eff = ifelse(is_bounced_bip, p3B_1B_thtime_eff, NA_real_),
      p1B_1B_thtime_eff = ifelse(is_bounced_bip, p1B_1B_thtime_eff, NA_real_)
    )
  
  x2_dat_exp <- dat_exp |>
    select(
      p_throwsLHP, p_throwsRHP, launch_angle, launch_speed, hp_to_1b, is_foul_tag,
      p1B_dist_eff, p1B_angle_eff, p2B_dist_eff, p2B_angle_eff, p3B_dist_eff, p3B_angle_eff,
      pSS_dist_eff, pSS_angle_eff, p1B_1B_thtime_eff, p2B_1B_thtime_eff, p3B_1B_thtime_eff,
      pSS_1B_thtime_eff, fielder3_fhtime_eff, fielder4_fhtime_eff, fielder5_fhtime_eff, fielder6_fhtime_eff,
      dteamATL:dteamWSH, home_teamATL:home_teamWSH
    ) |> as.matrix()
  
  d_L <- xgb.DMatrix(data = x2_dat_exp, missing = NA)
  dat_exp$p1 <- predict(model_final_L, newdata = d_L)
  dat_exp$change <- dat_exp$p1 - dat_exp$p0
  res_L <- rbind(res_L, dat_exp |> mutate(zone = zonenum))
}

L_position <- lall |> filter(stand == "L") |> select(-stand) |> mutate(Formation = "Season-average")
opt_L_long_s <- opt_L_long |> select(x, y, position, zone) |> mutate(Formation = "Optimal")
L_position <- rbind(L_position, opt_L_long_s)

max_change_val <- max(c(abs(res_R$change), abs(res_L$change)), na.rm = TRUE)
common_limits <- c(-max_change_val, max_change_val)

save(res_R, res_L, dR3, dL3, file = here("data", "dataset for sup 3.RData"))
save(R_position, L_position, file = here("data", "optimal set.RData"))
