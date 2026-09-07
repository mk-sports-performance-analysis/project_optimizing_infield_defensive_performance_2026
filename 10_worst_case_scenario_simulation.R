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


windowsFonts(ASE_FONT = windowsFont("Arial"))

FIG_SIZE_MM <- 174
TEXT_MM <- 3
LINE_MM <- 0.3
POINT_MM <- 0.7
MARGIN_MM <- 0.5

theme_mm <- theme_classic() +
  theme(
    text = element_text(size = mm_text_theme(TEXT_MM), family = "ASE_FONT", colour = "black"),
    plot.title = element_text(hjust = 0, face = "bold", size = mm_text_theme(TEXT_MM), family = "ASE_FONT", colour = "black"),
    strip.background = element_blank(),
    strip.text.x = element_text(angle = 0, hjust = 0, face = "bold", size = mm_text_theme(TEXT_MM), family = "ASE_FONT", colour = "black")
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

impact_colors <- c(
  "Base hits prevented" = scales::alpha("indianred1", 0.9),
  "No change（base hits）" = scales::alpha("lightcyan", 0.7),
  "No change（outs）" = scales::alpha("thistle1", 0.7),
  "Outs lost" = scales::alpha("skyblue3", 0.9)
)

g1 <- ggplot(zone_summaryR, aes(factor(New_zone), n, fill = impacts)) +
  geom_col(col = "black") +
  geom_text(
    aes(y = ypos, label = ifelse(impacts %in% c("No change（base hits）", "No change（outs）"), "", n)),
    fontface = "bold", vjust = -0.3, size = TEXT_MM
  ) +
  scale_fill_manual("", values = impact_colors) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3500)) +
  theme_mm +
  xlab("Pitch zones") + ylab("Count [n]") + ggtitle("a. RHB") +
  theme(legend.background = element_rect(colour = "black", fill = "white", linewidth = mm_line(LINE_MM * 0.5)))

g2 <- ggplot(zone_summaryL, aes(factor(New_zone), n, fill = impacts)) +
  geom_col(col = "black") +
  geom_text(
    aes(y = ypos, label = ifelse(impacts %in% c("No change（base hits）", "No change（outs）"), "", n)),
    fontface = "bold", vjust = -0.3, size = TEXT_MM
  ) +
  scale_fill_manual("", values = impact_colors) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3500)) +
  theme_mm +
  xlab("Pitch zones") + ylab("Count [n]") + ggtitle("b. LHB") +
  theme(legend.background = element_rect(colour = "black", fill = "white", linewidth = mm_line(LINE_MM * 0.5)))

g3 <- (g1 | g2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

g3

ggsave(
  "supple4.tiff", plot = g3,
  width = 170, height = 90, units = "mm",
  dpi = 300, bg = "white", limitsize = FALSE,
  compression = "lzw", device = "tiff"
)

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

windowsFonts(ASE_FONT=windowsFont("Arial"))
family <- "ASE_FONT"
FIG_SIZE_MM <- 174
TEXT_MM <- 2.7
LINE_MM <- 0.3
POINT_MM <- 3
MARGIN_MM <- 0.5
windowsFonts(MEI=windowsFont("Arial"))

range_r <- resultr |> group_by(New_zone) |> summarise(xmin=min(m1),xmax=max(m1),.groups="drop")
range_l <- resultl |> group_by(New_zone) |> summarise(xmin=min(m1),xmax=max(m1),.groups="drop")

plot_r <- Rresult |> select(New_zone,pmean,po,`Expected base value (worst-case scenario)`) |> mutate(stand="RHB") |> left_join(range_r,by="New_zone")
plot_l <- Lresult |> select(New_zone,pmean,po,`Expected base value (worst-case scenario)`) |> mutate(stand="LHB") |> left_join(range_l,by="New_zone")
plot_dat <- bind_rows(plot_r,plot_l) |>
  mutate(New_zone=factor(New_zone,levels=sort(unique(New_zone),decreasing=TRUE)),
         stand=factor(stand,levels=c("RHB","LHB")))

shapes_map <- c("Worst-case scenario"=21,"Average positions"=24,"Optimal formation"=22)
col_worst <- "blue"
col1 <- viridis(2)
draw_key_worst <- function(data,params,size){
  grobTree(
    rectGrob(gp=gpar(fill="#9ecae1",col=NA)),
    pointsGrob(x=0.5,y=0.5,pch=21,size=unit(POINT_MM*0.7,"mm"),
               gp=gpar(fill=col_worst,col="black",lwd=1))
  )
}

g <- ggplot(plot_dat,aes(y=New_zone)) +
  geom_segment(aes(x=po,xend=pmean,yend=New_zone),linewidth=LINE_MM*2,colour="grey50") +
  geom_segment(aes(x=xmin,xend=xmax,yend=New_zone),linewidth=LINE_MM*13,colour="#9ecae1",alpha=0.8) +
  geom_point(aes(x=`Expected base value (worst-case scenario)`,shape="Worst-case scenario"),
             size=POINT_MM*0.7,fill=col_worst,colour="black",stroke=0.7,key_glyph=draw_key_worst) +
  geom_point(aes(x=po,shape="Average positions"),size=POINT_MM*0.7,fill=col1[1],colour="black",stroke=0.7) +
  geom_point(aes(x=pmean,shape="Optimal formation"),size=POINT_MM*0.7,fill=col1[2],colour="black",stroke=0.7) +
  facet_wrap(~stand,ncol=2,labeller=as_labeller(c(RHB="a. RHB",LHB="b. LHB"))) +
  scale_x_continuous(limits=c(0.17,0.33),breaks=seq(0.18,0.33,0.02),expand=c(0.01,0.01)) +
  scale_shape_manual(values=shapes_map) +
  labs(x="Fielding performance metrics",y="Pitch zone",shape="") +
  theme_mm +
  theme(
    axis.line=element_line(linewidth=LINE_MM),
    axis.ticks=element_line(linewidth=LINE_MM),
    panel.grid.major.x=element_line(linewidth=LINE_MM*0.5,colour="grey85"),
    panel.grid.major.y=element_line(linewidth=LINE_MM*0.5,colour="grey90"),
    panel.grid.minor=element_blank(),
    strip.background=element_blank(),
    strip.text=element_text(size=mm_text_theme(TEXT_MM),family="ASE_FONT",colour="black"),
    legend.position="bottom",
    legend.background=element_rect(colour="black",fill="white",linewidth=LINE_MM)
  )

ggsave("supple5.tiff",plot=g,width=170,height=80,units="mm",dpi=300,
       bg="white",limitsize=FALSE,compression="lzw",device="tiff")
