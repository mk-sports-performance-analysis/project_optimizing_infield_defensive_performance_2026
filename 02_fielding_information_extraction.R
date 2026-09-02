
library(tidyverse)
library(here)
source(here("R", "load_functions.R"))
load(here("data", "subdata_ed.RData"))

# Define patterns for identifying fielding positions from play descriptions
wordlist_regex <- c(
  "out at home on the throw,\\s*",
  "out at\\s+(1st|2nd|3rd|home),\\s*",
  "out on the throw,\\s*",
  "caught stealing (1st|2nd|3rd) base,\\s*",
  "on a sacrifice fly to\\s*",
  "ground ball to\\s*",
  "line drive to\\s*",
  "fly ball to\\s*",
  "out sharply to\\s*",
  "out softly to\\s*",
  "out sharply,\\s*",
  "out softly,\\s*",
  "grounds out softly,\\s*",
  "grounds out,\\s*",
  "flies out,\\s*",
  "lines out,\\s*",
  "pops out,\\s*",
  "pops out to\\s*",
  "grounds out to\\s*",
  "lines out to\\s*",
  "flies out to\\s*",
  "pop up to\\s*",
  "pop to\\s*",
  " out to\\s*",
  "reaches on a fielder's choice out,\\s*",
  "a fielder's choice, fielded by\\s*",
  "into a force out,\\s*",
  "a throwing error by\\s*",
  "a fielding error by\\s*",
  "an interference error by\\s*",
  "catch error by\\s*",
  "a double play in foul territory,\\s*",
  "double play,\\s*",
  "a triple play,\\s*",
  "hit by batted ball,\\s*"
)

pos_map <- c(
  "pitcher" = "P",
  "catcher" = "C",
  "first baseman" = "1B",
  "second baseman" = "2B",
  "third baseman" = "3B",
  "shortstop" = "SS",
  "left fielder" = "LF",
  "center fielder" = "CF",
  "right fielder" = "RF"
)

pos_regex <- paste(names(pos_map), collapse = "|")

# Extract the fielding position from play descriptions
subdata <- subdata |>
  rowwise() |>
  mutate(
    hits = list(
      if (!is.na(events) && events != "") {
        wordlist_regex[
          sapply(wordlist_regex, function(w)
            str_detect(des, regex(w, ignore_case = TRUE))
          )
        ]
      } else character(0)
    ),
    hit = if (length(hits) == 0) {
      NA_character_
    } else {
      locs <- sapply(hits, function(w)
        str_locate(des, regex(w, ignore_case = TRUE))[1]
      )
      hits[which.min(locs)]
    },
    after = if (is.na(hit)) {
      NA_character_
    } else {
      loc_end <- str_locate(des, regex(hit, ignore_case = TRUE))[2]
      str_sub(des, loc_end + 1)
    },
    fielder_full = if (is.na(after)) NA_character_ else str_extract(after, pos_regex),
    fielderposition = ifelse(is.na(fielder_full), NA_character_, pos_map[fielder_full])
  ) |>
  select(-hits, -hit, -after) |>
  ungroup()

subdata <- subdata |>
  mutate(row_id = row_number())

d1 <- subdata |>
  filter(is.na(fielderposition)) |>
  mutate(
    fielderposition_new = case_when(
      des == "Foul"~ "Foul",
      events == "home_run" ~ "home_run",
      str_detect(des, "ground-rule double") ~ "GRD",
      str_detect(des, "fielder's choice") ~ "FC",
      str_detect(des, "fan interference") ~ "FI",
      str_detect(des, "error") ~ "fielder's error",
      TRUE ~ fielderposition))

subdata2 <- subdata |>
  left_join(d1 |>
              select(row_id, fielderposition_new), by = "row_id") |>
  mutate(fielderposition = coalesce(fielderposition ,fielderposition_new) ) |>
  select(-fielderposition_new)

# Define base state, pitch situation, and batted-ball characteristics
subdata3 <- subdata2 |>
  mutate(
    on_3b_flag = as.integer(!is.na(on_3b)),
    on_2b_flag = as.integer(!is.na(on_2b)),
    on_1b_flag = as.integer(!is.na(on_1b)),
    base_state = paste0(on_3b_flag, on_2b_flag, on_1b_flag),
    base_state_name = case_when(
      base_state == "000" ~ "Bases empty",
      base_state == "001" ~ "Runner on 1B",
      base_state == "010" ~ "Runner on 2B",
      base_state == "011" ~ "Runners on 2B & 1B",
      base_state == "100" ~ "Runner on 3B",
      base_state == "101" ~ "Runners on 3B & 1B",
      base_state == "110" ~ "Runners on 3B & 2B",
      base_state == "111" ~ "Bases loaded"
    ),
    bases = case_when(
      events == "single" ~ 1,
      events == "double" ~ 2,
      events == "triple" ~ 3,
      events == "home_run" ~ 4,
      TRUE ~ 0
    ),
    platoon = ifelse(p_throws == stand, "same", "opposite"),
    p_throws = ifelse(p_throws == "R", "RHP", "LHP"),
    launch_angle_cat = case_when(
      launch_angle < 10 ~ "GB",
      launch_angle < 25 ~ "LD",
      launch_angle < 50 ~ "FB",
      TRUE ~ "Pop-up"
    ),
    launch_angle_cat = factor(launch_angle_cat, levels = c("GB", "LD", "FB", "Pop-up"), ordered = TRUE),
    discate = ifelse(Distance > 60, "Outfield (>60m)", "Infield (<60m)"),
    vz = launch_speed * sin(deg2rad(launch_angle)),
    situation = paste(balls, strikes, sep = "_"),
    situation_cat = case_when(
      (balls == 0 & strikes >= 1) | (balls == 1 & strikes >= 2) | (balls == 2 & strikes == 2) ~ "Pitcher Favorable",
      (balls >= 1 & strikes == 0) | (balls >= 2 & strikes == 1) ~ "Batter Favorable",
      TRUE ~ "Even"
    ),
    situation_cat = factor(situation_cat, levels = c("Pitcher Favorable", "Even", "Batter Favorable"))
  ) |>
  select(-on_3b_flag, -on_2b_flag, -on_1b_flag, -on_3b, -on_2b, -on_1b, -base_state)

# Classify pitch types and define nine pitch-location zones
battedball <- subdata3 |>
  filter(
    base_state_name == "Bases empty",
    discate == "Infield (<60m)",
    !is.na(pfx_x),
    !is.na(launch_speed),
    !is.na(launch_angle),
    !is.na(sprayangle),
    !is.na(Distance)
  ) |>
  mutate(
    main_type = case_when(
      pitch_type %in% c("FF", "SI", "FC") ~ "Fastball",
      pitch_type %in% c("CH", "FS", "FO", "SC") ~ "Offspeed Group",
      pitch_type %in% c("CU", "KC", "CS") ~ "Curveball Group",
      pitch_type %in% c("SL", "ST", "SV") ~ "Slider Group",
      TRUE ~ NA_character_
    ),
    main_type = factor(main_type, levels = c("Fastball", "Offspeed Group", "Curveball Group", "Slider Group")),
    heightwide = sz_top - sz_bot,
    lowerthre = sz_bot + (sz_top - sz_bot) / 3,
    upperthre = sz_bot + 2 * (sz_top - sz_bot) / 3
  ) |>
  mutate(
    New_zone = case_when(
      plate_x < -((17 + 2.9) * 0.0254 / 3 / 2) & plate_z >= upperthre ~ 1L,
      plate_x >= -((17 + 2.9) * 0.0254 / 3 / 2) & plate_x < (17 + 2.9) * 0.0254 / 3 / 2 & plate_z >= upperthre ~ 2L,
      plate_x > (17 + 2.9) * 0.0254 / 3 / 2 & plate_z >= upperthre ~ 3L,
      plate_x < -((17 + 2.9) * 0.0254 / 3 / 2) & plate_z >= lowerthre & plate_z < upperthre ~ 4L,
      plate_x >= -((17 + 2.9) * 0.0254 / 3 / 2) & plate_x < (17 + 2.9) * 0.0254 / 3 / 2 & plate_z >= lowerthre & plate_z < upperthre ~ 5L,
      plate_x > (17 + 2.9) * 0.0254 / 3 / 2 & plate_z >= lowerthre & plate_z < upperthre ~ 6L,
      plate_x < -((17 + 2.9) * 0.0254 / 3 / 2) & plate_z < lowerthre ~ 7L,
      plate_x >= -((17 + 2.9) * 0.0254 / 3 / 2) & plate_x < (17 + 2.9) * 0.0254 / 3 / 2 & plate_z < lowerthre ~ 8L,
      plate_x > (17 + 2.9) * 0.0254 / 3 / 2 & plate_z < lowerthre ~ 9L,
      TRUE ~ NA_integer_
    ),
    New_zone_height = case_when(
        New_zone %in% 1:3 ~ "High",
        New_zone %in% 4:6 ~ "Middle",
        New_zone %in% 7:9 ~ "Low",
        TRUE ~ NA_character_
      ),
    New_zone_side = case_when(
        New_zone %in% c(1, 4, 7) ~ "Inner",
        New_zone %in% c(2, 5, 8) ~ "Middle",
        New_zone %in% c(3, 6, 9) ~ "Outer",
        TRUE ~ NA_character_
      ),
    New_zone_height = factor(New_zone_height, levels = c("Low", "Middle", "High")),
    New_zone_side = factor(New_zone_side, levels = c("Inner", "Middle", "Outer")),
    Areas = case_when(
      sprayangle_raw < 0 & stand == "R" ~ "Opposite field",
      sprayangle_raw < 0 & stand == "L" ~ "Pull side",
      sprayangle_raw >= 0 & stand == "R" ~ "Pull side",
      sprayangle_raw >= 0 & stand == "L" ~ "Opposite field",
      TRUE ~ NA_character_
    ),
    Areas = factor(Areas, levels = c("Pull side", "Opposite field"))) |>
   mutate(New_zone = factor(New_zone))
 
  
save(battedball, file = here::here("data", "battedball.RData"))

