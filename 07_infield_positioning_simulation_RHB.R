library(tidyverse)
library(xgboost)
library(here)

source(here("R", "load_functions.R"))
load(here("data", "Outcome prediction model.RData"))
load(here("data", "responsibility model.RData"))
load(here("data", "l_2025_league.RData"))

IF_base_wide <- l_2025_league |>
  filter(position %in% c("3B", "SS", "2B", "1B")) |>
  group_by(position, stand) |>
  summarise(lx0 = mean(lx), ly0 = mean(ly), .groups = "drop") |>
  pivot_wider(id_cols = c(stand), names_from = position, values_from = c(lx0, ly0), names_glue = "{.value}_{position}")

test_R2 <- test_R |>
  mutate(vforward = launch_speed * cos(deg2rad(launch_angle))) |>
  mutate(battedball_id = row_number()) |>
  select(New_zone, stand, vz:lly_raw, launch_speed, launch_angle, battedball_id, p0, is_foul_tag, hp_to_1b,
         vforward, y_position:home_teamWSH, arm_fielder_3:arm_fielder_6) |>
  left_join(IF_base_wide, by = c("stand")) |>
  select(-stand)

# grid search range: movement limits per position
grid_combinations <- crossing(
  dx_3B = seq(-6, 3, by = 3), dy_3B = seq(-9, 9, by = 3),
  dx_SS = seq(-9, 9, by = 3), dy_SS = seq(-9, 9, by = 3),
  dx_2B = seq(-6, 3, by = 3), dy_2B = seq(-9, 9, by = 3),
  dx_1B = seq(-3, 6, by = 3), dy_1B = seq(-6, 6, by = 3)
)

all_patterns <- IF_base_wide |>
  filter(stand == "R") |>
  cross_join(grid_combinations) |>
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
    ly_3B = ly0_3B + dx_3B * sin(theta0_3B) + dy_3B * cos(theta0_3B),
    ang_3B = atan2(ly_3B, lx_3B) * 180 / pi,
    ang_SS = atan2(ly_SS, lx_SS) * 180 / pi,
    ang_2B = atan2(ly_2B, lx_2B) * 180 / pi,
    ang_1B = atan2(ly_1B, lx_1B) * 180 / pi,
    is_valid = (
      sqrt(lx_3B^2 + ly_3B^2) <= 48 &
        sqrt(lx_SS^2 + ly_SS^2) <= 48 &
        sqrt(lx_2B^2 + ly_2B^2) <= 48 &
        sqrt(lx_1B^2 + ly_1B^2) <= 48 &
        sqrt((lx_3B - lx_SS)^2 + (ly_3B - ly_SS)^2) >= 5 &
        sqrt((lx_SS - lx_2B)^2 + (ly_SS - ly_2B)^2) >= 5 &
        sqrt((lx_2B - lx_1B)^2 + (ly_2B - ly_1B)^2) >= 5) &
      sqrt((lx_1B - 27.431 * cos(deg2rad(45)))^2 + (ly_1B - 27.431 * sin(deg2rad(45)))^2) < 15 &
      abs(ang_3B - ang_SS) <= 45 &
      abs(ang_SS - ang_2B) <= 45 &
      abs(ang_2B - ang_1B) <= 45 &
      abs(ang_3B - ang_SS) >= 6 &
      abs(ang_SS - ang_2B) >= 6 &
      abs(ang_2B - ang_1B) >= 6)

grid_all_ori <- all_patterns |>
  filter(is_valid == TRUE) |>
  mutate(grid_id = as.character(row_number()))

grid_all <- grid_all_ori |>
  select(grid_id, lx_1B, lx_2B, lx_3B, lx_SS, ly_1B, ly_2B, ly_3B, ly_SS)

chunk_size <- 100 # adjust based on available memory

grid_chunks <- split(grid_all, ceiling(seq_len(nrow(grid_all)) / chunk_size))

infielders <- c("1B", "2B", "3B", "SS")
num_class <- 7

for (zonenum in c(1:9)) {
  
  final_file <- here("data", paste0("res_R_zone", zonenum, ".rds"))
  
  res_R <- list()
  zonedata <- test_R2 |> filter(New_zone == zonenum)
  
  for (i in seq_along(grid_chunks)) {
    
    g <- grid_chunks[[i]]
    dat_exp <- tidyr::crossing(zonedata, g)
    
    dat_exp <- dat_exp |>
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
        nearest_angle = case_when(
          pSS_dist == min_if_dist ~ pSS_angle,
          p2B_dist == min_if_dist ~ p2B_angle,
          p3B_dist == min_if_dist ~ p3B_angle,
          p1B_dist == min_if_dist ~ p1B_angle,
          TRUE ~ NA_real_
        ),
        is_bounced_bip = launch_angle < 10 & min_if_dist > 5 & abs(nearest_angle) < 100
      ) |>
      select(-min_if_dist, -nearest_angle)
    
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
      data = dat_exp
    )
    x_dat_exp <- xgb.DMatrix(data = x_dat_exp)
    
    pred_respon_R <- matrix(predict(model_R_respon, newdata = x_dat_exp), ncol = num_class, byrow = TRUE)
    colnames(pred_respon_R) <- levels(test_R$fielderposition)
    
    probs_if <- as.matrix(pred_respon_R[, infielders])
    hit_if <- probs_if >= 0.2
    max_idx <- max.col(probs_if, ties.method = "first")
    
    dat_exp$gate_infield <- purrr::map(
      seq_len(nrow(dat_exp)),
      \(i) {
        hi <- infielders[hit_if[i, ]]
        if (length(hi) > 0) hi else infielders[max_idx[i]]
      }
    )
    
    dat_exp <- dat_exp |>
      mutate(
        pSS_dist_eff = ifelse(map_lgl(gate_infield, ~"SS" %in% .x), pSS_dist, NA_real_),
        pSS_angle_eff = ifelse(map_lgl(gate_infield, ~"SS" %in% .x), pSS_angle, NA_real_),
        pSS_1B_thtime_eff = ifelse(map_lgl(gate_infield, ~"SS" %in% .x), pSS_1B_thtime, NA_real_),
        p2B_dist_eff = ifelse(map_lgl(gate_infield, ~"2B" %in% .x), p2B_dist, NA_real_),
        p2B_angle_eff = ifelse(map_lgl(gate_infield, ~"2B" %in% .x), p2B_angle, NA_real_),
        p2B_1B_thtime_eff = ifelse(map_lgl(gate_infield, ~"2B" %in% .x), p2B_1B_thtime, NA_real_),
        p3B_dist_eff = ifelse(map_lgl(gate_infield, ~"3B" %in% .x), p3B_dist, NA_real_),
        p3B_angle_eff = ifelse(map_lgl(gate_infield, ~"3B" %in% .x), p3B_angle, NA_real_),
        p3B_1B_thtime_eff = ifelse(map_lgl(gate_infield, ~"3B" %in% .x), p3B_1B_thtime, NA_real_),
        p1B_dist_eff = ifelse(map_lgl(gate_infield, ~"1B" %in% .x), p1B_dist, NA_real_),
        p1B_angle_eff = ifelse(map_lgl(gate_infield, ~"1B" %in% .x), p1B_angle, NA_real_),
        p1B_1B_thtime_eff = ifelse(map_lgl(gate_infield, ~"1B" %in% .x), p1B_1B_thtime, NA_real_),
        fielder3_fhtime_eff = ifelse((map_lgl(gate_infield, ~"1B" %in% .x) & is_bounced_bip), fielder3_fhtime, NA_real_),
        fielder4_fhtime_eff = ifelse((map_lgl(gate_infield, ~"2B" %in% .x) & is_bounced_bip), fielder4_fhtime, NA_real_),
        fielder5_fhtime_eff = ifelse((map_lgl(gate_infield, ~"3B" %in% .x) & is_bounced_bip), fielder5_fhtime, NA_real_),
        fielder6_fhtime_eff = ifelse((map_lgl(gate_infield, ~"SS" %in% .x) & is_bounced_bip), fielder6_fhtime, NA_real_),
        pSS_1B_thtime_eff = ifelse(is_bounced_bip, pSS_1B_thtime_eff, NA_real_),
        p2B_1B_thtime_eff = ifelse(is_bounced_bip, p2B_1B_thtime_eff, NA_real_),
        p3B_1B_thtime_eff = ifelse(is_bounced_bip, p3B_1B_thtime_eff, NA_real_),
        p1B_1B_thtime_eff = ifelse(is_bounced_bip, p1B_1B_thtime_eff, NA_real_)
      )
    
    x2_dat_exp <- dat_exp |>
      select(p_throwsLHP, p_throwsRHP, launch_angle, launch_speed, hp_to_1b, is_foul_tag,
             p1B_dist_eff, p1B_angle_eff, p2B_dist_eff, p2B_angle_eff,
             p3B_dist_eff, p3B_angle_eff, pSS_dist_eff, pSS_angle_eff,
             p1B_1B_thtime_eff, p2B_1B_thtime_eff, p3B_1B_thtime_eff, pSS_1B_thtime_eff,
             fielder3_fhtime_eff, fielder4_fhtime_eff, fielder5_fhtime_eff, fielder6_fhtime_eff,
             dteamATL, dteamAZ, dteamBAL, dteamBOS, dteamCHC, dteamCIN, dteamCLE, dteamCOL, dteamCWS,
             dteamDET, dteamHOU, dteamKC, dteamLAA, dteamLAD, dteamMIA, dteamMIL, dteamMIN, dteamNYM, dteamNYY,
             dteamOAK, dteamPHI, dteamPIT, dteamSD, dteamSEA, dteamSF, dteamSTL, dteamTB, dteamTEX, dteamTOR, dteamWSH,
             home_teamATL, home_teamAZ, home_teamBAL, home_teamBOS, home_teamCHC, home_teamCIN, home_teamCLE, home_teamCOL,
             home_teamCWS, home_teamDET, home_teamHOU, home_teamKC, home_teamLAA, home_teamLAD, home_teamMIA, home_teamMIL,
             home_teamMIN, home_teamNYM, home_teamNYY, home_teamOAK, home_teamPHI, home_teamPIT, home_teamSD, home_teamSEA,
             home_teamSF, home_teamSTL, home_teamTB, home_teamTEX, home_teamTOR, home_teamWSH
      ) |>
      as.matrix()
    
    d_R <- xgb.DMatrix(data = x2_dat_exp, missing = NA)
    
    dat_exp$p1 <- predict(model_final_R, newdata = d_R)
    
    res_R[[i]] <- dat_exp |>
      group_by(grid_id) |>
      summarise(
        lSS_x = mean(lx_SS), lSS_y = mean(ly_SS), l1B_x = mean(lx_1B), l1B_y = mean(ly_1B),
        l2B_x = mean(lx_2B), l2B_y = mean(ly_2B), l3B_x = mean(lx_3B), l3B_y = mean(ly_3B),
        pmean = mean(p1, na.rm = TRUE), .groups = "drop"
      )
    
    rm(dat_exp, x_dat_exp, d_R, x2_dat_exp, hit_if, probs_if, max_idx, pred_respon_R)
    
    if (i %% 5 == 0) {
      tmpfile <- here("data", paste0("res_R_zone", zonenum, "_temp.rds"))
      if (file.exists(tmpfile)) file.remove(tmpfile)
      saveRDS(res_R, file = tmpfile)
      print(paste("Zone:", zonenum, "| Chunk:", i, "saved."))
    }
  }
  saveRDS(res_R, file = final_file)
  
  rm(res_R, zonedata)
  gc()
}

save(grid_all_ori, file = here("data", "grid_all_ori_R.RData"))