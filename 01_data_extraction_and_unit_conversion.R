library(tidyverse)
library(remotes)
library(baseballr)
library(here)
source(here::here("R", "load_functions.R"))
year <- c(2023:2025)


alldata <- data.frame()
for (yr in year){
  #year <- 2023
  load(here::here("data", paste0("MLB", yr, ".RData")))
  alldata <- bind_rows(all, alldata)
}

# Exclude games played during spring training
subdata <- alldata |>
  filter(game_type != "S")

# Extract variables used in the analysis
subdata <- subdata |>
  select(batter, stand, pitcher, p_throws,
         home_team, away_team, if_fielding_alignment, of_fielding_alignment, 
         on_3b, on_2b,  on_1b,balls, strikes, outs_when_up, sz_top, sz_bot,
         release_speed, pitch_type, plate_x, plate_z, pfx_x, pfx_z, 
         hit_distance_sc, hc_x, hc_y, launch_speed, launch_angle, 
         fielder_3, fielder_4, fielder_5, fielder_6 , fielder_7, fielder_8, fielder_9, inning_topbot,
         events, description,des,year) 

# Simplify result descriptions
subdata <- subdata |>
  mutate(
    simp_desc = case_when(
      description == "blocked_ball" ~ "ball",
      description == "foul_tip" ~ "foul",
      description == "swinging_strike_blocked" ~ "swinging_strike",
      description %in% c("unknown_strike", "hit_by_pitch", "pitchout") ~ "others",
      description %in% c("bunt_foul_tip", "foul_bunt", "missed_bunt") ~ "bunt",
      TRUE ~ description
    ),
    results = case_when(
      events %in% c("double_play", "field_out", "force_out", "grounded_into_double_play", "triple_play") ~ "Out",
      events %in% c("sac_bunt", "sac_fly", "sac_fly_double_play", "sac_bunt_double_play") ~ "Sacrifice",
      events %in% c("fielders_choice", "fielders_choice_out") ~ "fielders_choice",
      events %in% c("double", "home_run", "triple", "single") ~ "Base hit",
      events %in% c("Intent walk","intent_walk", "walk") ~ "Walk",
      events %in% c("strikeout", "strikeout_double_play") ~ "Strikeout",
      events %in% c("catcher_interf", "hit_by_pitch") ~ "Others",
      events == "" ~ "NoBIP",
      TRUE ~ events
    )
  ) |>
  filter(!events %in% c("home_run","truncated_pa","wild_pitch", "game_advisory", "stolen_base_2b", "stolen_base_3b", "caught_stealing_2b", "caught_stealing_3b",
                        "caught_stealing_home", "passed_ball", "pickoff_1b", "pickoff_2b", "pickoff_3b", "pickoff_caught_stealing_2b", 
                        "pickoff_caught_stealing_3b", "pickoff_error_3b", "pickoff_caught_stealing_home"),
         !results %in% c("Others", "Sacrifice", "Walk", "fielders_choice", "field_error"))

# Calculate spray angle from Statcast hit coordinates
ang <- rad2deg(
  atan2(subdata$hc_x - 125.42, 198.27 - subdata$hc_y))

subdata <- subdata |>
  mutate(sprayangle = ang) 

# Convert measurements from Statcast units to SI units
subdata <- subdata |> 
  mutate(launch_speed = launch_speed*0.44704, Distance = hit_distance_sc*0.3048, sprayangle = round(sprayangle*(-1))) |> #
  mutate(plate_x = plate_x*0.3048 , plate_z = plate_z*0.3048) |>
  mutate(release_speed = release_speed*0.44704, pfx_x = pfx_x*0.3048, pfx_z = pfx_z*0.3048) 

# Preserve the original values before adjusting for batter handedness
subdata$plate_x_raw <- subdata$plate_x
subdata$pfx_x_raw <- subdata$pfx_x
subdata$sprayangle_raw <- subdata$sprayangle

# Reverse the horizontal direction for left-handed batters
subdata$plate_x[subdata$stand == "L"] <- subdata$plate_x[subdata$stand == "L"] *(-1)
subdata$pfx_x[subdata$stand == "L"] <- subdata$pfx_x[subdata$stand == "L"] *(-1)
subdata$sprayangle[subdata$stand == "L"] <- subdata$sprayangle[subdata$stand == "L"] * (-1)

# Convert polar coordinates to Cartesian coordinates
subdata <- subdata |>
  mutate(llx_raw =  polar2orth_center45(Distance, sprayangle_raw+90)$x, lly_raw =  polar2orth_center45(Distance, sprayangle_raw+90)$y )  |>
  mutate(llx =  polar2orth_center45(Distance, sprayangle+90)$x, lly =  polar2orth_center45(Distance, sprayangle+90)$y )  |>
  mutate(sz_top = sz_top * 0.3048, sz_bot = sz_bot * 0.3048) 

save(subdata, file = here::here("data", "subdata_ed.RData"))


