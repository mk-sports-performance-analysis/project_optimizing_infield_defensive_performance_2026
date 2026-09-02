# project_optimizing_infield_defensive_performance_2026

This repository contains the R analysis pipeline that was used to examine whether the horizontal launch direction of batted balls varies depending on pitch location in U.S. Major League Baseball. In addition, this pipeline was designed to identify the optimal infield defensive formation that minimizes the probability of a base hit for each of nine pitch-location zones.

## Overview

The pipeline is organized into the following stages:

1. Data extraction & preprocessing – clean data, convert units, and define a 9-zone grid.
2. Descriptive analysis – examine whether pitch-zone height/side relates to batted ball launch direction.
3. Quantile regression
4. Modeling – train XGBoost models that (a) predict which fielder is responsible for a batted ball and (b) predict a batting result from batted-ball kinematics, fielder positioning, arm strength, and batter sprint speed.
5. Infield defensive formation optimization – use the trained models to identify the alignment that minimizes expected hit probability for each of the 9 pitch zones.
6. Robustness/discussion – worst-case scenario evaluation of the recommended shifts, the distribution of batted balls from low-zone pitches, and a check of the quantile-regression coefficient estimates.

## Repository Structure

All scripts are numbered in the order they are meant to be run. Each script loads the `.RData` output of the preceding step(s). 

| Step | Script |
|---|---|
| 1 | `01_data_extraction_and_unit_conversion.R` |  
| 2 | `02_feature_engineering_and_pitch_zone_grid.R` |
| 3 | `03_pitch_zone_grid_visualization.R` | 
| 4 | `04_pitch_location_vs_trajectory_and_spray_direction.R` | 
| 5 | `05_quantile_regression_of_spray_angle.R` |
| 6 | `06_fielder_responsibility_and_outcome_models.R` | 
| 7 | `07_infield_positioning_simulation_RHB.R` / `07_infield_positioning_simulation_LHB.R` |
| 8 | `08_optimized_infield_positioning_visualization.R` | 
| 9 | `09_batted_ball_distribution_in_low_zones.R` | 
| 10 | `10_worst_case_scenario_simulation.R` | 
| 11 | `11_quantile_regression_coefficient_bias_estimation.R` | 

## Data Source
All data listed above are owned by MLB Advanced Media, L.P. (MLB).
This repository distributes analysis code only; no data are included. Use of the data is subject to Baseball Savant / MLB's terms of use.

- https://baseballsavant.mlb.com/statcast_search
- https://baseballsavant.mlb.com/leaderboard/arm-strength
- https://baseballsavant.mlb.com/leaderboard/sprint_speed
- https://baseballsavant.mlb.com/visuals/fielder-positioning-all
- https://baseballsavant.mlb.com/visuals/fielder-positioning

## Data Requirements

This repository contains analysis code only. The following external inputs and must be obtained from the data source.

| File(s) | Source |
|---|---|
| `MLB2023.RData`, `MLB2024.RData`, `MLB2025.RData` | Statcast pitch-by-pitch data, one file per season|
| `sprint_speed_2023_2025.csv` | Baseball Savant Sprint Speed leaderboard |
| `arm_strength_2023.csv`, `arm_strength_2024.csv`, `arm_strength_2025.csv`, `arm_strength_all.csv` |Baseball Savant Arm Strength leaderboards |
| `positioning_data2023_2025.RData` | Baseball Savant fielder starting-position |

## Computational Environment

- R Environment: R 4.4.0
- R Package version:

```r
install.packages(c(
  "tidyverse",    # 2.0.0
  "remotes",      # 2.5.0
  "stringr",      # 1.5.1
  "ggtext",       # 0.1.2
  "ggplot2",      # 4.0.3
  "dplyr",        # 1.1.4
  "ggsignif",     # 0.6.4
  "viridis",      # 0.6.5
  "ggpattern",    # 1.3.1
  "shadowtext",   # 0.1.6
  "qgam",         # 2.0.0
  "DescTools",    # 0.99.60
  "xgboost",      # 3.2.1.1
  "caret",        # 7.0.1
  "pROC",         # 1.18.5
  "ggnewscale",   # 0.5.2
  "patchwork",    # 1.3.2
  "scales",       # 1.4.0
  "here"          # 1.0.2
))

# Installed from GitHub / CRAN (verified version 2.0.0)
remotes::install_github("BillPetti/baseballr")
```

## Recommended layout

```
repo-root/
├─ .here             
├─ R/                 
├─ data/             
├─ .gitignore
├─ README.md
└─ LICENSE
```

## Citation

If you use this code, please cite the associated publication:

```
[Add citation once the associated manuscript is published]
```

## License

This code is released under the **MIT License** (see `LICENSE`). 
