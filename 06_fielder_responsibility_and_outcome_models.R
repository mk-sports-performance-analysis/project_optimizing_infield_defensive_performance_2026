library(tidyverse)
library(ggsignif)
library(viridis)
library(ggpattern)
library(shadowtext)
library(xgboost)
library(caret)
library(here)

# Load processed data
load(here("data", "battedball.RData"))
source(here("R", "load_functions.R"))
battedball <- battedball |>
  mutate(dteam = ifelse(inning_topbot == "Top", home_team, away_team)) |>
  mutate(
    dteam = ifelse(dteam %in% c("WAS", "ATH"), ifelse(dteam == "WAS", "WSH", "OAK"), dteam),
    home_team = ifelse(home_team %in% c("WAS", "ATH"), ifelse(home_team == "WAS", "WSH", "OAK"), home_team)
  ) |>
  filter(fielderposition %in% c("1B", "2B", "3B","SS" ,"CF", "LF", "RF"))

hp_to_1b <- read_csv(here("data", "sprint_speed_2023_2025.csv")) |>
  select(player_id, team, hp_to_1b) |>
  rename(batter = player_id)

battedball <- battedball |>
  left_join(hp_to_1b, by = "batter")

league_mean <- battedball |>
  summarise(league_mean = mean(hp_to_1b, na.rm = TRUE)) |>
  pull(league_mean)

battedball <- battedball |>
  mutate(hp_to_1b = if_else(is.na(hp_to_1b), league_mean, hp_to_1b))

files <- c("arm_strength_2023.csv", "arm_strength_2024.csv", "arm_strength_2025.csv")

arm_strength <- files |>
  lapply(\(f) read_csv(here("data", f))) |>
  bind_rows() |>
  select(player_id, primary_position, primary_position_name, arm_overall) |>
  rename(fielder = player_id, position = primary_position) |>
  group_by(fielder, position) |>
  summarise(arm_overall = mean(arm_overall, na.rm = T)) |>
  mutate(source = "recent") |>
  select(fielder, position, arm_overall, source) |>
  mutate(arm_overall = arm_overall * 0.44704)

league_position_mean <- files |>
  lapply(\(f) read_csv(here("data", f))) |>
  bind_rows() |>
  select(player_id, primary_position, primary_position_name, arm_overall) |>
  rename(fielder = player_id, position = primary_position) |>
  group_by(position) |>
  summarise(arm_overall = mean(arm_overall, na.rm = T)) |>
  filter(position %in% 3:9) |>
  mutate(fielder = NA_real_, source = "league") |>
  select(fielder, position, arm_overall, source) |>
  mutate(arm_overall = arm_overall * 0.44704)

arm_strength_all_year <- read_csv(here("data", "arm_strength_all.csv")) |>
  select(player_id, primary_position, primary_position_name, arm_overall) |>
  rename(fielder = player_id, position = primary_position) |>
  group_by(fielder, position) |>
  summarise(arm_overall = mean(arm_overall, na.rm = T)) |>
  mutate(source = "past") |>
  select(fielder, position, arm_overall, source) |>
  mutate(arm_overall = arm_overall * 0.44704)

arm_stack <- bind_rows(arm_strength, arm_strength_all_year, league_position_mean) |>
  mutate(source_rank = case_when(source == "recent" ~ 1L, source == "past" ~ 2L, source == "league" ~ 3L)) |>
  arrange(fielder, source_rank) |>
  group_by(fielder) |>
  dplyr::slice(1) |>
  ungroup() |>
  select(fielder, arm_overall)

fs <- battedball |>
  mutate(row_id = row_number()) |>
  pivot_longer(
    cols = c(fielder_3, fielder_4, fielder_5, fielder_6, fielder_7, fielder_8, fielder_9),
    names_to = "fielder_pos",
    values_to = "fielder"
  ) |>
  left_join(arm_stack, by = "fielder") |>
  pivot_wider(
    id_cols = row_id,
    names_from = fielder_pos,
    values_from = arm_overall,
    names_prefix = "arm_"
  )

battedball <- battedball |>
  mutate(row_id = row_number()) |>
  left_join(fs, by = "row_id")

league_mean_vec <- league_position_mean |>
  filter(position %in% 3:9) |>
  select(position, arm_overall) |>
  tibble::deframe()

battedball <- battedball |>
  mutate(
    arm_fielder_3 = coalesce(arm_fielder_3, league_mean_vec["3"]),
    arm_fielder_4 = coalesce(arm_fielder_4, league_mean_vec["4"]),
    arm_fielder_5 = coalesce(arm_fielder_5, league_mean_vec["5"]),
    arm_fielder_6 = coalesce(arm_fielder_6, league_mean_vec["6"]),
    arm_fielder_7 = coalesce(arm_fielder_7, league_mean_vec["7"]),
    arm_fielder_8 = coalesce(arm_fielder_8, league_mean_vec["8"]),
    arm_fielder_9 = coalesce(arm_fielder_9, league_mean_vec["9"])
  )

battedball <- battedball |>
  filter(if_fielding_alignment %in% c("Infield shade", "Standard"))

base_data <- battedball |>
  select(
    year, New_zone, bases, home_team, simp_desc, des, results, sprayangle_raw,
    if_fielding_alignment, inning_topbot, vz, launch_angle, launch_speed, Distance,
    llx_raw, lly_raw, sz_top, sz_bot, p_throws, stand, plate_x, plate_z,
    fielderposition, fielder_3:fielder_9, dteam, hp_to_1b, base_state_name,
    arm_fielder_3:arm_fielder_9
  ) |>
  filter(lly_raw > -10) |>
  mutate(is_foul_tag = ifelse(abs(sprayangle_raw) > 50, T, F))

team_map <- c(
  "Braves" = "ATL", "D-backs" = "AZ", "Orioles" = "BAL", "Red Sox" = "BOS",
  "Cubs" = "CHC", "Reds" = "CIN", "Guardians" = "CLE", "Rockies" = "COL",
  "White Sox" = "CWS", "Tigers" = "DET", "Astros" = "HOU", "Royals" = "KC",
  "Angels" = "LAA", "Dodgers" = "LAD", "Marlins" = "MIA", "Brewers" = "MIL",
  "Twins" = "MIN", "Mets" = "NYM", "Yankees" = "NYY", "Athletics" = "OAK",
  "Phillies" = "PHI", "Pirates" = "PIT", "Padres" = "SD", "Mariners" = "SEA",
  "Giants" = "SF", "Cardinals" = "STL", "Rays" = "TB", "Rangers" = "TEX",
  "Blue Jays" = "TOR", "Nationals" = "WSH"
)

load(here("data", "positioning_data2023_2025.RData"))

l <- positioning_data |>
  mutate(
    ave_distance = avg_norm_start_distance * 0.3048,
    avg_angle = avg_norm_start_angle * (-1)
  ) |>
  filter(is.na(season) == F) |>
  mutate(
    lx = polar2orth_center45(ave_distance, avg_angle + 90)$x,
    ly = polar2orth_center45(ave_distance, avg_angle + 90)$y
  ) |>
  mutate(if_fielding_alignment = ifelse(shift == 1, "Infield shade", "Standard")) |>
  group_by(if_fielding_alignment) |>
  mutate(
    dteam = ifelse(
      fld_name_display_club %in% names(team_map),
      team_map[fld_name_display_club],
      NA
    )
  ) |>
  rename(year = season, stand = batSide) |>
  select(
    fielder_id, dteam, year, position, if_fielding_alignment, stand,
    ave_distance, avg_angle, lx, ly
  )


l_2025_league <- l |>
  filter(year == 2025) |>
   group_by(dteam, position, stand, fielder_id) |>
  summarise(
    n_plays = n(),
    across(c(ave_distance, avg_angle, lx, ly), \(x) mean(x, na.rm = TRUE)),
    .groups = "drop_last"
  ) |>
  summarise(
    across(
      c(ave_distance, avg_angle, lx, ly),
      \(x) weighted.mean(x, w = n_plays, na.rm = TRUE)
    ),
    total_plays = sum(n_plays),
    n_fielders  = n_distinct(fielder_id),
    .groups = "drop"
  ) |>
  group_by(position, stand) |>
  summarise(
    across(
      c(ave_distance, avg_angle, lx, ly),
      \(x) mean(x, na.rm = TRUE)
    ),
    .groups = "drop"
  )
save(l_2025_league, file = here("data", "l_2025_league.RData"))


l_personal <- l |>
  select(
    fielder_id, dteam, year, stand, position, if_fielding_alignment,
    ave_distance, avg_angle, lx, ly
  ) |>
  ungroup()

l_year_team_mean <- l |>
  group_by(dteam, year, stand, position, if_fielding_alignment) |>
  summarise(
    ave_distance_m = mean(ave_distance, na.rm = TRUE),
    avg_angle_m = mean(avg_angle, na.rm = TRUE),
    lx_m = mean(lx, na.rm = TRUE),
    ly_m = mean(ly, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ungroup()

l_team_mean <- l |>
  group_by(dteam, stand, position, if_fielding_alignment) |>
  summarise(
    ave_distance_m = mean(ave_distance, na.rm = TRUE),
    avg_angle_m = mean(avg_angle, na.rm = TRUE),
    lx_m2 = mean(lx, na.rm = TRUE),
    ly_m2 = mean(ly, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ungroup()

l_team_all_mean <- l |>
  group_by(dteam, stand, position) |>
  summarise(
    ave_distance_m = mean(ave_distance, na.rm = TRUE),
    avg_angle_m = mean(avg_angle, na.rm = TRUE),
    lx_m2 = mean(lx, na.rm = TRUE),
    ly_m2 = mean(ly, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ungroup()

pos_map <- c(
  "3" = "1B", "4" = "2B", "5" = "3B", "6" = "SS",
  "7" = "LF", "8" = "CF", "9" = "RF"
)

for (i in 3:9) {
  pos <- pos_map[as.character(i)]
  arm_col <- paste0("arm_fielder_", i)
  
  base_data <- base_data |>
    left_join(
      l_personal |>
        filter(position == pos) |>
        rename(
          lx_p = lx, ly_p = ly,
          ave_distance_p = ave_distance, avg_angle_p = avg_angle
        ),
      by = setNames(
        c("fielder_id", "dteam", "year", "stand", "if_fielding_alignment"),
        c(paste0("fielder_", i), "dteam", "year", "stand", "if_fielding_alignment")
      )
    ) |>
    left_join(
      l_year_team_mean |>
        filter(position == pos) |>
        rename(
          lx_yt = lx_m, ly_yt = ly_m,
          ave_distance_yt = ave_distance_m, avg_angle_yt = avg_angle_m
        ),
      by = c("dteam", "year", "stand", "if_fielding_alignment")
    ) |>
    left_join(
      l_team_mean |>
        filter(position == pos) |>
        rename(
          lx_t = lx_m2, ly_t = ly_m2,
          ave_distance_t = ave_distance_m, avg_angle_t = avg_angle_m
        ),
      by = c("dteam", "stand", "if_fielding_alignment")
    ) |>
    left_join(
      l_team_all_mean |>
        filter(position == pos) |>
        rename(
          lx_ta = lx_m2, ly_ta = ly_m2,
          ave_distance_ta = ave_distance_m, avg_angle_ta = avg_angle_m
        ),
      by = c("dteam", "stand")
    ) |>
    mutate(
      source_stage = case_when(
        !is.na(lx_p) ~ "personal",
        is.na(lx_p) & !is.na(lx_yt) ~ "team_year",
        is.na(lx_p) & is.na(lx_yt) & !is.na(lx_t) ~ "team_align",
        is.na(lx_p) & is.na(lx_yt) & is.na(lx_t) & !is.na(lx_ta) ~ "team_all",
        TRUE ~ "still_na"
      ),
      lx = coalesce(lx_p, lx_yt, lx_t, lx_ta),
      ly = coalesce(ly_p, ly_yt, ly_t, ly_ta),
      ave_distance = coalesce(ave_distance_p, ave_distance_yt, ave_distance_t, ave_distance_ta),
      avg_angle = coalesce(avg_angle_p, avg_angle_yt, avg_angle_t, avg_angle_ta)
    ) |>
    mutate(
      vforward = launch_speed * cos(deg2rad(launch_angle)),
      est_hangtime = sqrt(lx^2 + ly^2) / vforward
    ) |>
    rename(
      !!paste0("fielder", i, "_dist") := ave_distance,
      !!paste0("fielder", i, "_angle") := avg_angle,
      !!paste0("fielder", i, "_fhtime") := est_hangtime,
      !!paste0("fielder", i, "_source") := source_stage
    ) |>
    mutate(
      !!paste0("p", pos, "_dist") := sqrt((llx_raw - lx)^2 + (lly_raw - ly)^2),
      !!paste0("p", pos, "_angle") := atan2(
        (0 - lx) * (lly_raw - ly) - (0 - ly) * (llx_raw - lx),
        (0 - lx) * (llx_raw - lx) + (0 - ly) * (lly_raw - ly)
      ) * 180 / pi,
      !!paste0("p", pos, "_1B_thtime") := sqrt(
        (lx - 27.431 * cos(deg2rad(45)))^2 +
          (ly - 27.431 * sin(deg2rad(45)))^2
      ) / .data[[arm_col]]
    ) |>
    select(
      -starts_with("lx_"), -starts_with("ly_"),
      -starts_with("ave_distance_"), -starts_with("avg_angle_"),
      -starts_with("est_hangtime_"), -lx, -ly
    )
}

base_data |>
  select(ends_with("_source")) |>
  tidyr::pivot_longer(everything()) |>
  count(value)

base_data <- base_data |>
  select(
    year, des, New_zone, results, fielderposition, p_throws, home_team, dteam,
    stand, sprayangle_raw, launch_angle, launch_speed, vz, Distance, llx_raw, lly_raw,
    p1B_dist, p1B_angle, p2B_dist, p2B_angle, p3B_dist, p3B_angle, pSS_dist, pSS_angle,
    pLF_dist, pLF_angle, pCF_dist, pCF_angle, pRF_dist, pRF_angle,
    p1B_1B_thtime, p2B_1B_thtime, p3B_1B_thtime, pSS_1B_thtime,
    pLF_1B_thtime, pCF_1B_thtime, pRF_1B_thtime,
    fielder3_fhtime, fielder4_fhtime, fielder5_fhtime, fielder6_fhtime,
    fielder7_fhtime, fielder8_fhtime, fielder9_fhtime,
    plate_x, plate_z, arm_fielder_3:arm_fielder_9, is_foul_tag, hp_to_1b, New_zone
  ) |>
  filter(results %in% c("Base hit", "Out")) |>
  filter(if_all(everything(), ~!is.na(.))) |>
  mutate(
    is_bounced_bip = case_when(
      launch_angle > 10 ~ FALSE,
      launch_angle <= 10 &
        (pmin(p1B_dist, p2B_dist, pSS_dist, p3B_dist) > 5) &
        !(abs(p1B_angle) > 100 & p1B_dist > 5) &
        !(abs(p2B_angle) > 100 & p2B_dist > 5) &
        !(abs(pSS_angle) > 100 & pSS_dist > 5) &
        !(abs(p3B_angle) > 100 & p3B_dist > 5) ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  filter(results %in% c("Base hit", "Out"), !fielderposition %in% c("P", "C")) |>
  mutate(
    fielderposition = factor(fielderposition),
    y_position = as.integer(fielderposition) - 1,
    results = factor(results, levels = c("Out", "Base hit")),
    y_result = as.integer(results) - 1
  ) |>
  select(-plate_x, -plate_z)

X_team <- onehot(base_data$dteam, prefix = "dteam")
X_home <- onehot(base_data$home_team, prefix = "home_team")
X_throw <- onehot(base_data$p_throws, prefix = "p_throws")

base_data <- base_data |>
  cbind(X_throw, X_team, X_home)

save(base_data, file = here("data", "base_data.RData"))

R <- base_data |> filter(stand == "R")
L <- base_data |> filter(stand == "L")

train_R <- R |> filter(year != 2025)
test_R <- R |> filter(year == 2025)
train_L <- L |> filter(year != 2025)
test_L <- L |> filter(year == 2025)

x_train_R <- model.matrix(
  ~p_throwsLHP + p_throwsRHP + launch_speed + launch_angle + is_foul_tag + is_bounced_bip +
    p1B_dist + p1B_angle + p2B_dist + p2B_angle + p3B_dist + p3B_angle + pSS_dist + pSS_angle +
    dteamATL + dteamAZ + dteamBAL + dteamBOS + dteamCHC + dteamCIN + dteamCLE + dteamCOL + dteamCWS +
    dteamDET + dteamHOU + dteamKC + dteamLAA + dteamLAD + dteamMIA + dteamMIL + dteamMIN + dteamNYM + dteamNYY +
    dteamOAK + dteamPHI + dteamPIT + dteamSD + dteamSEA + dteamSF + dteamSTL + dteamTB + dteamTEX + dteamTOR + dteamWSH +
    home_teamATL + home_teamAZ + home_teamBAL + home_teamBOS + home_teamCHC + home_teamCIN + home_teamCLE + home_teamCOL +
    home_teamCWS + home_teamDET + home_teamHOU + home_teamKC + home_teamLAA + home_teamLAD + home_teamMIA + home_teamMIL +
    home_teamMIN + home_teamNYM + home_teamNYY + home_teamOAK + home_teamPHI + home_teamPIT + home_teamSD + home_teamSEA +
    home_teamSF + home_teamSTL + home_teamTB + home_teamTEX + home_teamTOR + home_teamWSH - 1,
  data = train_R
)

dtrain_R0 <- xgb.DMatrix(data = x_train_R, label = train_R$y_position)

x_test_R <- model.matrix(
  ~p_throwsLHP + p_throwsRHP + launch_speed + launch_angle + is_foul_tag + is_bounced_bip +
    p1B_dist + p1B_angle + p2B_dist + p2B_angle + p3B_dist + p3B_angle + pSS_dist + pSS_angle +
    dteamATL + dteamAZ + dteamBAL + dteamBOS + dteamCHC + dteamCIN + dteamCLE + dteamCOL + dteamCWS +
    dteamDET + dteamHOU + dteamKC + dteamLAA + dteamLAD + dteamMIA + dteamMIL + dteamMIN + dteamNYM + dteamNYY +
    dteamOAK + dteamPHI + dteamPIT + dteamSD + dteamSEA + dteamSF + dteamSTL + dteamTB + dteamTEX + dteamTOR + dteamWSH +
    home_teamATL + home_teamAZ + home_teamBAL + home_teamBOS + home_teamCHC + home_teamCIN + home_teamCLE + home_teamCOL +
    home_teamCWS + home_teamDET + home_teamHOU + home_teamKC + home_teamLAA + home_teamLAD + home_teamMIA + home_teamMIL +
    home_teamMIN + home_teamNYM + home_teamNYY + home_teamOAK + home_teamPHI + home_teamPIT + home_teamSD + home_teamSEA +
    home_teamSF + home_teamSTL + home_teamTB + home_teamTEX + home_teamTOR + home_teamWSH - 1,
  data = test_R
)

dtest_R0 <- xgb.DMatrix(data = x_test_R, label = test_R$y_position)

depth_grid <- c(1, 2, 3)
num_class <- nlevels(train_R$fielderposition)

params <- list(
  booster = "gbtree", objective = "multi:softprob", num_class = num_class,
  eta = 0.3, max_depth = 2, gamma = 0, min_child_weight = 1,
  subsample = 1, colsample_bytree = 0.6, eval_metric = "mlogloss"
)

run_cv <- function(dtrain, params, max_depth, nrounds = 250, nfold = 5) {
  params_cv <- modifyList(params, list(max_depth = max_depth))
  cv <- xgb.cv(
    params = params_cv, data = dtrain, nrounds = nrounds, nfold = nfold,
    early_stopping_rounds = 20, maximize = FALSE, verbose = 0
  )
  tibble(
    max_depth = max_depth,
    best_nrounds = cv$best_iteration,
    best_score = min(cv$evaluation_log$test_mlogloss_mean, na.rm = TRUE)
  )
}

depth_grid <- c(1, 2, 3)

set.seed(1)
cv_res_R <- purrr::map_dfr(depth_grid, ~run_cv(dtrain = dtrain_R0, params = params, max_depth = .x))

best <- cv_res_R |> slice_min(best_score)

params_final_R <- modifyList(params, list(max_depth = best$max_depth))

set.seed(1)
model_R_respon <- xgb.train(
  params = params_final_R, data = dtrain_R0,
  nrounds = best$best_nrounds, verbose = 0
)

pred_prob_R <- predict(model_R_respon, dtest_R0)
pred_prob_R <- matrix(pred_prob_R, ncol = num_class, byrow = TRUE)
colnames(pred_prob_R) <- levels(train_R$fielderposition)

pred_class_R <- colnames(pred_prob_R)[max.col(pred_prob_R, ties.method = "first")]
pred_class_R <- factor(pred_class_R, levels = levels(train_R$fielderposition))

l2 <- confusionMatrix(pred_class_R, factor(test_R$fielderposition))
l2

f1_R <- round(mean(l2$byClass[, 7]), 3)
print(paste("RHB F1:", f1_R))

x_train_L <- model.matrix(
  ~p_throwsLHP + p_throwsRHP + launch_speed + launch_angle + is_foul_tag + is_bounced_bip +
    p1B_dist + p1B_angle + p2B_dist + p2B_angle + p3B_dist + p3B_angle + pSS_dist + pSS_angle +
    dteamATL + dteamAZ + dteamBAL + dteamBOS + dteamCHC + dteamCIN + dteamCLE + dteamCOL + dteamCWS +
    dteamDET + dteamHOU + dteamKC + dteamLAA + dteamLAD + dteamMIA + dteamMIL + dteamMIN + dteamNYM + dteamNYY +
    dteamOAK + dteamPHI + dteamPIT + dteamSD + dteamSEA + dteamSF + dteamSTL + dteamTB + dteamTEX + dteamTOR + dteamWSH +
    home_teamATL + home_teamAZ + home_teamBAL + home_teamBOS + home_teamCHC + home_teamCIN + home_teamCLE + home_teamCOL +
    home_teamCWS + home_teamDET + home_teamHOU + home_teamKC + home_teamLAA + home_teamLAD + home_teamMIA + home_teamMIL +
    home_teamMIN + home_teamNYM + home_teamNYY + home_teamOAK + home_teamPHI + home_teamPIT + home_teamSD + home_teamSEA +
    home_teamSF + home_teamSTL + home_teamTB + home_teamTEX + home_teamTOR + home_teamWSH - 1,
  data = train_L
)

dtrain_L0 <- xgb.DMatrix(data = x_train_L, label = train_L$y_position)

x_test_L <- model.matrix(
  ~p_throwsLHP + p_throwsRHP + launch_speed + launch_angle + is_foul_tag + is_bounced_bip +
    p1B_dist + p1B_angle + p2B_dist + p2B_angle + p3B_dist + p3B_angle + pSS_dist + pSS_angle +
    dteamATL + dteamAZ + dteamBAL + dteamBOS + dteamCHC + dteamCIN + dteamCLE + dteamCOL + dteamCWS +
    dteamDET + dteamHOU + dteamKC + dteamLAA + dteamLAD + dteamMIA + dteamMIL + dteamMIN + dteamNYM + dteamNYY +
    dteamOAK + dteamPHI + dteamPIT + dteamSD + dteamSEA + dteamSF + dteamSTL + dteamTB + dteamTEX + dteamTOR + dteamWSH +
    home_teamATL + home_teamAZ + home_teamBAL + home_teamBOS + home_teamCHC + home_teamCIN + home_teamCLE + home_teamCOL +
    home_teamCWS + home_teamDET + home_teamHOU + home_teamKC + home_teamLAA + home_teamLAD + home_teamMIA + home_teamMIL +
    home_teamMIN + home_teamNYM + home_teamNYY + home_teamOAK + home_teamPHI + home_teamPIT + home_teamSD + home_teamSEA +
    home_teamSF + home_teamSTL + home_teamTB + home_teamTEX + home_teamTOR + home_teamWSH - 1,
  data = test_L
)

dtest_L0 <- xgb.DMatrix(data = x_test_L, label = test_L$y_position)

set.seed(1)
cv_res_L <- purrr::map_dfr(depth_grid, ~run_cv(dtrain = dtrain_L0, params = params, max_depth = .x))

best <- cv_res_L |> slice_min(best_score)

params_final_L <- modifyList(params, list(max_depth = best$max_depth))

set.seed(1)
model_L_respon <- xgb.train(
  params = params_final_L, data = dtrain_L0,
  nrounds = best$best_nrounds, verbose = 0
)

pred_prob_L <- predict(model_L_respon, dtest_L0)
pred_prob_L <- matrix(pred_prob_L, ncol = num_class, byrow = TRUE)
colnames(pred_prob_L) <- levels(train_L$fielderposition)

pred_class_L <- colnames(pred_prob_L)[max.col(pred_prob_L, ties.method = "first")]
pred_class_L <- factor(pred_class_L, levels = levels(train_L$fielderposition))

l2 <- confusionMatrix(pred_class_L, factor(test_L$fielderposition))
l2

f1_L <- round(mean(l2$byClass[, 7]), 3)
print(paste("LHB F1:", f1_L))

save(model_R_respon, model_L_respon, file = here("data", "responsibility model.RData"))

###################################
  
  pred_respon_R <- matrix(predict(model_R_respon, dtrain_R0), ncol = num_class, byrow = TRUE) |>
    data.frame()
  colnames(pred_respon_R) <- levels(train_R$fielderposition)
  
  infielders <- c("1B", "2B", "3B", "SS")
  probs_if <- as.matrix(pred_respon_R[, infielders])
  hit_if <- probs_if >= 0.20
  max_idx <- max.col(probs_if, ties.method = "first")

  train_R$gate_infield <- purrr::map(
    seq_len(nrow(train_R)),
    \(i) {
      hi <- infielders[hit_if[i, ]]
      if (length(hi) > 0) hi else infielders[max_idx[i]]
    }
  )
  
  train_R <- train_R |>
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
      arm_fielder_3_eff = ifelse(map_lgl(gate_infield, ~"1B" %in% .x), arm_fielder_3, NA_real_),
      arm_fielder_4_eff = ifelse(map_lgl(gate_infield, ~"2B" %in% .x), arm_fielder_4, NA_real_),
      arm_fielder_5_eff = ifelse(map_lgl(gate_infield, ~"3B" %in% .x), arm_fielder_5, NA_real_),
      arm_fielder_6_eff = ifelse(map_lgl(gate_infield, ~"SS" %in% .x), arm_fielder_6, NA_real_),
      fielder3_fhtime_eff = ifelse((map_lgl(gate_infield, ~"1B" %in% .x) & is_bounced_bip), fielder3_fhtime, NA_real_),
      fielder4_fhtime_eff = ifelse((map_lgl(gate_infield, ~"2B" %in% .x) & is_bounced_bip), fielder4_fhtime, NA_real_),
      fielder5_fhtime_eff = ifelse((map_lgl(gate_infield, ~"3B" %in% .x) & is_bounced_bip), fielder5_fhtime, NA_real_),
      fielder6_fhtime_eff = ifelse((map_lgl(gate_infield, ~"SS" %in% .x) & is_bounced_bip), fielder6_fhtime, NA_real_),
      pSS_1B_thtime_eff = ifelse(is_bounced_bip, pSS_1B_thtime_eff, NA_real_),
      p2B_1B_thtime_eff = ifelse(is_bounced_bip, p2B_1B_thtime_eff, NA_real_),
      p3B_1B_thtime_eff = ifelse(is_bounced_bip, p3B_1B_thtime_eff, NA_real_),
      p1B_1B_thtime_eff = ifelse(is_bounced_bip, p1B_1B_thtime_eff, NA_real_)
    )
  
  pred_respon_R <- matrix(predict(model_R_respon, dtest_R0), ncol = num_class, byrow = TRUE) |>
    data.frame()
  colnames(pred_respon_R) <- levels(test_R$fielderposition)
  
  probs_if <- as.matrix(pred_respon_R[, infielders])
  hit_if <- probs_if >= 0.20
  max_idx <- max.col(probs_if, ties.method = "first")
  
  test_R$gate_infield <- purrr::map(
    seq_len(nrow(test_R)),
    \(i) {
      hi <- infielders[hit_if[i, ]]
      if (length(hi) > 0) hi else infielders[max_idx[i]]
    }
  )
  
  test_R <- test_R |>
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
      arm_fielder_3_eff = ifelse(map_lgl(gate_infield, ~"1B" %in% .x), arm_fielder_3, NA_real_),
      arm_fielder_4_eff = ifelse(map_lgl(gate_infield, ~"2B" %in% .x), arm_fielder_4, NA_real_),
      arm_fielder_5_eff = ifelse(map_lgl(gate_infield, ~"3B" %in% .x), arm_fielder_5, NA_real_),
      arm_fielder_6_eff = ifelse(map_lgl(gate_infield, ~"SS" %in% .x), arm_fielder_6, NA_real_),
      fielder3_fhtime_eff = ifelse((map_lgl(gate_infield, ~"1B" %in% .x) & is_bounced_bip), fielder3_fhtime, NA_real_),
      fielder4_fhtime_eff = ifelse((map_lgl(gate_infield, ~"2B" %in% .x) & is_bounced_bip), fielder4_fhtime, NA_real_),
      fielder5_fhtime_eff = ifelse((map_lgl(gate_infield, ~"3B" %in% .x) & is_bounced_bip), fielder5_fhtime, NA_real_),
      fielder6_fhtime_eff = ifelse((map_lgl(gate_infield, ~"SS" %in% .x) & is_bounced_bip), fielder6_fhtime, NA_real_),
      pSS_1B_thtime_eff = ifelse(is_bounced_bip, pSS_1B_thtime_eff, NA_real_),
      p2B_1B_thtime_eff = ifelse(is_bounced_bip, p2B_1B_thtime_eff, NA_real_),
      p3B_1B_thtime_eff = ifelse(is_bounced_bip, p3B_1B_thtime_eff, NA_real_),
      p1B_1B_thtime_eff = ifelse(is_bounced_bip, p1B_1B_thtime_eff, NA_real_)
    )
  
  pred_respon_L <- matrix(predict(model_L_respon, dtrain_L0), ncol = num_class, byrow = TRUE) |>
    data.frame()
  colnames(pred_respon_L) <- levels(train_L$fielderposition)
  
  probs_if <- as.matrix(pred_respon_L[, infielders])
  hit_if <- probs_if >= 0.20
  max_idx <- max.col(probs_if, ties.method = "first")
  
  train_L$gate_infield <- purrr::map(
    seq_len(nrow(train_L)),
    \(i) {
      hi <- infielders[hit_if[i, ]]
      if (length(hi) > 0) hi else infielders[max_idx[i]]
    }
  )
  
  train_L <- train_L |>
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
      arm_fielder_3_eff = ifelse(map_lgl(gate_infield, ~"1B" %in% .x), arm_fielder_3, NA_real_),
      arm_fielder_4_eff = ifelse(map_lgl(gate_infield, ~"2B" %in% .x), arm_fielder_4, NA_real_),
      arm_fielder_5_eff = ifelse(map_lgl(gate_infield, ~"3B" %in% .x), arm_fielder_5, NA_real_),
      arm_fielder_6_eff = ifelse(map_lgl(gate_infield, ~"SS" %in% .x), arm_fielder_6, NA_real_),
      fielder3_fhtime_eff = ifelse((map_lgl(gate_infield, ~"1B" %in% .x) & is_bounced_bip), fielder3_fhtime, NA_real_),
      fielder4_fhtime_eff = ifelse((map_lgl(gate_infield, ~"2B" %in% .x) & is_bounced_bip), fielder4_fhtime, NA_real_),
      fielder5_fhtime_eff = ifelse((map_lgl(gate_infield, ~"3B" %in% .x) & is_bounced_bip), fielder5_fhtime, NA_real_),
      fielder6_fhtime_eff = ifelse((map_lgl(gate_infield, ~"SS" %in% .x) & is_bounced_bip), fielder6_fhtime, NA_real_),
      pSS_1B_thtime_eff = ifelse(is_bounced_bip, pSS_1B_thtime_eff, NA_real_),
      p2B_1B_thtime_eff = ifelse(is_bounced_bip, p2B_1B_thtime_eff, NA_real_),
      p3B_1B_thtime_eff = ifelse(is_bounced_bip, p3B_1B_thtime_eff, NA_real_),
      p1B_1B_thtime_eff = ifelse(is_bounced_bip, p1B_1B_thtime_eff, NA_real_)
    )
  
  pred_respon_L <- matrix(predict(model_L_respon, dtest_L0), ncol = num_class, byrow = TRUE) |>
    data.frame()
  colnames(pred_respon_L) <- levels(test_L$fielderposition)
  
  probs_if <- as.matrix(pred_respon_L[, infielders])
  hit_if <- probs_if >= 0.20
  max_idx <- max.col(probs_if, ties.method = "first")
  
  test_L$gate_infield <- purrr::map(
    seq_len(nrow(test_L)),
    \(i) {
      hi <- infielders[hit_if[i, ]]
      if (length(hi) > 0) hi else infielders[max_idx[i]]
    }
  )
  
  test_L <- test_L |>
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
      arm_fielder_3_eff = ifelse(map_lgl(gate_infield, ~"1B" %in% .x), arm_fielder_3, NA_real_),
      arm_fielder_4_eff = ifelse(map_lgl(gate_infield, ~"2B" %in% .x), arm_fielder_4, NA_real_),
      arm_fielder_5_eff = ifelse(map_lgl(gate_infield, ~"3B" %in% .x), arm_fielder_5, NA_real_),
      arm_fielder_6_eff = ifelse(map_lgl(gate_infield, ~"SS" %in% .x), arm_fielder_6, NA_real_),
      fielder3_fhtime_eff = ifelse((map_lgl(gate_infield, ~"1B" %in% .x) & is_bounced_bip), fielder3_fhtime, NA_real_),
      fielder4_fhtime_eff = ifelse((map_lgl(gate_infield, ~"2B" %in% .x) & is_bounced_bip), fielder4_fhtime, NA_real_),
      fielder5_fhtime_eff = ifelse((map_lgl(gate_infield, ~"3B" %in% .x) & is_bounced_bip), fielder5_fhtime, NA_real_),
      fielder6_fhtime_eff = ifelse((map_lgl(gate_infield, ~"SS" %in% .x) & is_bounced_bip), fielder6_fhtime, NA_real_),
      pSS_1B_thtime_eff = ifelse(is_bounced_bip, pSS_1B_thtime_eff, NA_real_),
      p2B_1B_thtime_eff = ifelse(is_bounced_bip, p2B_1B_thtime_eff, NA_real_),
      p3B_1B_thtime_eff = ifelse(is_bounced_bip, p3B_1B_thtime_eff, NA_real_),
      p1B_1B_thtime_eff = ifelse(is_bounced_bip, p1B_1B_thtime_eff, NA_real_)
    )
  
  x_R_tra <- train_R |>
    select(
      p_throwsLHP, p_throwsRHP, launch_angle, launch_speed, hp_to_1b, is_foul_tag,
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
  
  x_R_te <- test_R |>
    select(
      p_throwsLHP, p_throwsRHP, launch_angle, launch_speed, hp_to_1b, is_foul_tag,
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
  
  x_L_tra <- train_L |>
    select(
      p_throwsLHP, p_throwsRHP, launch_angle, launch_speed, hp_to_1b, is_foul_tag,
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
  
  x_L_te <- test_L |>
    select(
      p_throwsLHP, p_throwsRHP, launch_angle, launch_speed, hp_to_1b, is_foul_tag,
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
  
  dtrain_R <- xgb.DMatrix(data = x_R_tra, label = train_R$y_result, missing = NA)
  dtrain_L <- xgb.DMatrix(data = x_L_tra, label = train_L$y_result, missing = NA)
  dtest_R <- xgb.DMatrix(data = x_R_te, label = test_R$y_result, missing = NA)
  dtest_L <- xgb.DMatrix(data = x_L_te, label = test_L$y_result, missing = NA)
  
  params_base <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eta = 0.3,
    gamma = 0,
    min_child_weight = 1,
    subsample = 1,
    colsample_bytree = 0.6,
    eval_metric = "logloss"
  )
  
  depth_grid <- c(1, 2, 3)
  
  run_cv_2 <- function(dtrain, params_base, max_depth, nrounds = 250, nfold = 5) {
    params_cv <- modifyList(params_base, list(max_depth = max_depth))
    cv <- xgb.cv(
      params = params_cv, data = dtrain, nrounds = nrounds, nfold = nfold,
      early_stopping_rounds = 20, maximize = FALSE, verbose = 0
    )
    tibble(
      max_depth = max_depth,
      best_nrounds = cv$best_iteration,
      best_score = min(cv$evaluation_log$test_logloss_mean)
    )
  }
  
  depth_grid <- c(1, 2, 3)
  
  set.seed(1)
  cv_train_R <- purrr::map_dfr(
    depth_grid,
    ~run_cv_2(dtrain = dtrain_R, params_base = params_base, max_depth = .x)
  )
  
  train_R_best_row <- cv_train_R |> slice_min(best_score)
  train_R_best_depth <- train_R_best_row$max_depth
  train_R_best_nrounds <- train_R_best_row$best_nrounds
  
  set.seed(1)
  cv_train_L <- purrr::map_dfr(
    depth_grid,
    ~run_cv_2(dtrain = dtrain_L, params_base = params_base, max_depth = .x)
  )
  
  train_L_best_row <- cv_train_L |> slice_min(best_score)
  train_L_best_depth <- train_L_best_row$max_depth
  train_L_best_nrounds <- train_L_best_row$best_nrounds
  
  params_final_R <- modifyList(
    params_base,
    list(max_depth = train_R_best_depth, objective = "binary:logistic")
  )
  
  set.seed(1)
  model_final_R <- xgb.train(
    params = params_final_R, data = dtrain_R,
    nrounds = train_R_best_nrounds, verbose = 0
  )
  
  params_final_L <- modifyList(
    params_base,
    list(max_depth = train_L_best_depth, objective = "binary:logistic")
  )
  
  set.seed(1)
  model_final_L <- xgb.train(
    params = params_final_L, data = dtrain_L,
    nrounds = train_L_best_nrounds, verbose = 0
  )
  
  pred_prob_R <- predict(model_final_R, newdata = dtest_R)
  
  library(pROC)
  
  roc_R <- roc(
    response = test_R$results,
    predictor = pred_prob_R,
    levels = c("Out", "Base hit"),
    direction = "<"
  )
  
  auc_R <- as.numeric(auc(roc_R))
  print(round(auc_R, 3))
  
  pred_prob_L <- predict(model_final_L, newdata = dtest_L)
  
  roc_L <- roc(
    response = test_L$results,
    predictor = pred_prob_L,
    levels = c("Out", "Base hit"),
    direction = "<"
  )
  
  auc_L <- as.numeric(auc(roc_L))
  print(round(auc_L, 3))
  
  test_R <- test_R |>
    mutate(p0 = pred_prob_R)
  
  test_L <- test_L |>
    mutate(p0 = pred_prob_L)
  
  all_result <- data.frame(aucr = round(auc_R, 3),aucl = round(auc_L, 3))
  
    
save(test_R, test_L, model_final_R, model_final_L, file = here("data", "Outcome prediction model.RData"))
