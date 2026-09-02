
mm_line <- function(mm) {mm / 25.4 * 72}
mm_margin <- function(mm){ mm / 25.4 * 72}
mm_text_theme <- function(mm){ mm / 25.4 * 72 }
mm_text <- function(mm){ mm / 25.4 * 72 }

deg2rad<-function(x) {
    angle<-x*pi/180
    return(angle)}

rad2deg<-function(x) {
  angle<-x/pi*180
  return(angle)}

polar2orth_center0<-function(x1,angle){
  x<-x1*sin(angle*pi/180)
  y<-x1*cos(angle*pi/180)
  return(data.frame(x,y))}

orth2polar_center0<-function(x,y){
  x1<-sqrt((x*x)+(y*y))
  angle<-rad2deg(atan(x/y))
  return(data.frame(x1,angle))}

polar2orth_center45<-function(x1,angle){
  x<-x1*cos(angle*pi/180)
  y<-x1*sin(angle*pi/180)
  return(data.frame(x,y))}

orth2polar_center45<-function(x,y){
  x1<-sqrt((x*x)+(y*y))
  angle<-rad2deg(atan2(y,x))
  return(data.frame(x1,angle))}


cramers_v <- function(tabdata) {
  chi <- chisq.test(tabdata)$statistic
  n <- sum(tabdata)
  k <- min(nrow(tabdata), ncol(tabdata))
  sqrt(chi / (n * (k - 1)))
}

onehot <- function(x, prefix = NULL) {
  lev <- sort(unique(x))
  M <- matrix(0, nrow = length(x), ncol = length(lev))
  if (is.null(prefix)) {
    colnames(M) <- lev
  } else {
    colnames(M) <- paste0(prefix, lev)
  }
  for (i in seq_along(lev)) {
    M[, i] <- as.integer(x == lev[i])
  }
  M[is.na(x), ] <- NA
  M
}

# Create ggplot layers for a baseball field coordinate system
baseball_field_layers <- function(
    max_dist = 60,
    dist_breaks = seq(20, max_dist, by = 10),
    angle_breaks = seq(-45, 45, by = 15),
    text_size = mm_text(TEXT_MM),
    line_width = mm_line(LINE_MM),
    family = ""
) {
  
  sideline <- data.frame(
    angle = c(45, 45, -45, -45),
    dist = rep(c(0, max_dist), 2),
    group = c(45, 45, -45, -45)
  )
  sideline$x <- polar2orth_center0(sideline$dist, sideline$angle)$x
  sideline$y <- polar2orth_center0(sideline$dist, sideline$angle)$y
  
  dline <- expand.grid(angle = seq(-45, 45, 1), dist = dist_breaks)
  dline$group <- dline$dist
  dline$x <- polar2orth_center0(dline$dist, dline$angle)$x
  dline$y <- polar2orth_center0(dline$dist, dline$angle)$y
  
  gridline1 <- expand.grid(angle = angle_breaks, dist = c(0, max_dist))
  gridline1$group <- gridline1$angle
  gridline1$x <- polar2orth_center0(gridline1$dist, gridline1$angle)$x
  gridline1$y <- polar2orth_center0(gridline1$dist, gridline1$angle)$y
  
  note1 <- data.frame(
    angle = angle_breaks,
    dist = max_dist + 5,
    label = paste0(rev(angle_breaks), "°")
  )
  note1$x <- polar2orth_center0(note1$dist, note1$angle)$x
  note1$y <- polar2orth_center0(note1$dist, note1$angle)$y
  
  offset <- 5
  note2 <- data.frame(dist = seq(10, max_dist, 10)) |>
    mutate(
      angle = 45 + asin(offset / dist) * 180 / pi,
      label = paste0(dist, " m")
    )
  note2$x <- polar2orth_center0(note2$dist, note2$angle)$x
  note2$y <- polar2orth_center0(note2$dist, note2$angle)$y - 1
  
  base_dist <- 27.432
  infield_poly <- data.frame(
    base = c("Home", "1B", "2B", "3B"),
    dist = c(0, base_dist, base_dist * sqrt(2), base_dist),
    angle = c(0, 45, 0, -45)
  )
  infield_poly$x <- polar2orth_center0(infield_poly$dist, infield_poly$angle)$x
  infield_poly$y <- polar2orth_center0(infield_poly$dist, infield_poly$angle)$y
  
   list(
    geom_line(data = dline, aes(x, y, group = group), color = "gray60", linewidth = line_width),
    geom_line(data = gridline1, aes(x, y, group = group), color = "gray60", linewidth = line_width),
    geom_line(data = sideline, aes(x, y, group = group), linewidth = line_width),
    geom_text(data = note1, aes(x, y, label = label), family = family, size = text_size),
    geom_text(data = note2, aes(x, y, label = label), family = family, size = text_size),
    geom_polygon(data = infield_poly, aes(x, y), fill = NA, color = "black", linewidth = line_width)
  )
}

clearConsole =  function(){
  cat( "\014" )
}

clearPlots = function () {
  if( dev.cur() > 1 ) dev.off()
}

clearWorkspace = function () {
  rm( list = ls( envir = globalenv() ), envir = globalenv() )
}

clearAll = function(){
  clearConsole()
  clearPlots()
  clearWorkspace()
}
