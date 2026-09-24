library(tidyverse)
library(xgboost)
library(patchwork)
library(pROC)
library(viridis)
library(grid)
library(here)
source(here("R", "load_functions.R"))
load(here("data", "dataset for sup 3.RData"))

roc_R <- roc(res_R$y_result, res_R$p0, direction = "<")
thresh_R <- coords(roc_R, "best", best.method = "youden", ret = "threshold")$threshold

roc_L <- roc(res_L$y_result, res_L$p0, direction = "<")
thresh_L <- coords(roc_L, "best", best.method = "youden", ret = "threshold")$threshold


res_R <- res_R |>
  mutate(
    predres = ifelse(p1 > thresh_R, 1, 0),
    predbase = ifelse(p0 > thresh_R, 1, 0),
    base_errors = case_when(
      y_result == 1 & predbase == 0 ~ "False negative",
      y_result == 0 & predbase == 1 ~ "False positive",
      y_result == predbase ~ "correct"
    ),
    impacts = case_when(
      predbase == 1 & predres == 0 ~ "Base hits prevented",
      predbase == 0 & predres == 1 ~ "Outs lost",
      predbase == 1 & predres == 1 ~ "No change（base hits）",
      predbase == 0 & predres == 0 ~ "No change（outs）"
    )
  )

res_L <- res_L |>
  mutate(
    predres = ifelse(p1 > roc_L, 1, 0),
    predbase = ifelse(p0 > roc_L, 1, 0),
    base_errors = case_when(
      y_result == 1 & predbase == 0 ~ "False negative",
      y_result == 0 & predbase == 1 ~ "False positive",
      y_result == predbase ~ "correct"
    ),
    impacts = case_when(
      predbase == 1 & predres == 0 ~ "Base hits prevented",
      predbase == 0 & predres == 1 ~ "Outs lost",
      predbase == 1 & predres == 1 ~ "No change（base hits）",
      predbase == 0 & predres == 0 ~ "No change（outs）"
    )
  )

zone_summaryR <- res_R |>
  count(New_zone, impacts) |>
  mutate(impacts = factor(impacts, levels = c(
    "Outs lost",
    "No change（base hits）",
    "No change（outs）",
    "Base hits prevented"
  ))) |>
  group_by(New_zone) |>
  mutate(ypos = cumsum(n))

zone_summaryL <- res_L |>
  count(New_zone, impacts) |>
  mutate(impacts = factor(impacts, levels = c(
    "Outs lost",
    "No change（base hits）",
    "No change（outs）",
    "Base hits prevented"
  ))) |>
  group_by(New_zone) |>
  mutate(ypos = cumsum(n))



###############################################################
res_R <- res_R |> mutate(effect=case_when(change<0 ~ "positive effect", change>0 ~ "negative effect", TRUE ~ "no change"))
res_L <- res_L |> mutate(effect=case_when(change<0 ~ "positive effect", change>0 ~ "negative effect", TRUE ~ "no change"))

resultr <- data.frame()
for(ii in 1:1000){
  base_r <- res_R[,c("New_zone","effect","p1")]
  updated_zones <- lapply(split(base_r,base_r$New_zone),function(sub_df){
    neg_indices <- which(sub_df$effect=="negative effect")
    sample_size <- round(length(neg_indices)*0.118)
    target_indices <- sample(neg_indices,size=sample_size)
    sub_df$p1[target_indices] <- sub_df$p1[target_indices]*2
    sub_df
  })
  base_r_updated <- do.call(rbind,updated_zones)
  means_by_zone <- aggregate(p1~New_zone,data=base_r_updated,FUN=mean)
  resultr <- rbind(resultr,data.frame(num=ii,New_zone=means_by_zone$New_zone,m1=means_by_zone$p1))
}
resultr$type <- "simulation"

Rresult <- resultr |>
  group_by(New_zone) |>
  summarise(`Expected base value (worst-case scenario)`=mean(m1),.groups="drop") |>
  left_join(dR3,by="New_zone") |>
  mutate(diffp4=`Expected base value (worst-case scenario)`-po)

resultl <- data.frame()
for(ii in 1:1000){
  base_l <- res_L[,c("New_zone","effect","p1")]
  updated_zones <- lapply(split(base_l,base_l$New_zone),function(sub_df){
    neg_indices <- which(sub_df$effect=="negative effect")
    sample_size <- round(length(neg_indices)*0.118)
    target_indices <- sample(neg_indices,size=sample_size)
    sub_df$p1[target_indices] <- sub_df$p1[target_indices]*2
    sub_df
  })
  base_l_updated <- do.call(rbind,updated_zones)
  means_by_zone <- aggregate(p1~New_zone,data=base_l_updated,FUN=mean)
  resultl <- rbind(resultl,data.frame(num=ii,New_zone=means_by_zone$New_zone,m1=means_by_zone$p1))
}
resultl$type <- "simulation"

Lresult <- resultl |>
  group_by(New_zone) |>
  summarise(`Expected base value (worst-case scenario)`=mean(m1),.groups="drop") |>
  left_join(dL3,by="New_zone") |>
  mutate(diffp4=`Expected base value (worst-case scenario)`-po)
