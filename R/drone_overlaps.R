# Functions originally created in drones/overlaps.qmd for calculating various aspects of drone flight.

get_gsd <- function(H_m, focal_mm, sensor_mm, image_px, h_units = 'm') {
  # make cm
  if (h_units == 'm') {H_m = H_m * 100} else if (h_units != 'cm') {stop("units not supported")}
  gsd <- (H_m * sensor_mm)/(focal_mm * image_px)
}

worst_gsd <- function(H_m, focal_mm, sensor_w, sensor_h, image_w, image_h, h_units = 'm') {
  gsd_w <- get_gsd(H_m, focal_mm, sensor_w, image_w)
  gsd_h <- get_gsd(H_m, focal_mm, sensor_h, image_h)
  
  gsd <- pmax(gsd_w, gsd_h)
}

ground_dist <- function(image_px, gsd_cmpx) {
  D <- image_px*gsd_cmpx/100
}

drone_dist <- function(ground_dist_m, overlap_prop) {
  d <- ground_dist_m * (1-overlap_prop)
}

photo_interval <- function(drone_dist_m, v_ms) {
  t <- drone_dist_m/v_ms
}

velocity <- function(drone_dist_m, p_s) {
  v <- drone_dist_m/p_s
}

drone_dist_from_gsd <- function(image_px, gsd_cmpx, overlap_prop) {
  D <- ground_dist(image_px, gsd_cmpx)
  dd <- drone_dist(D, overlap_prop)
}

photo_interval_from_gsd <- function(image_px, gsd_cmpx, overlap_prop, v_ms) {
  dd <- drone_dist_from_gsd(image_px, gsd_cmpx, overlap_prop)
  t <- photo_interval(dd, v_ms)
}

velocity_from_gsd <- function(image_px, gsd_cmpx, overlap_prop, p_s) {
  dd <- drone_dist_from_gsd(image_px, gsd_cmpx, overlap_prop)
  v <- velocity(dd, p_s)
}

ground_dist_H_o <- function(H_m, overlap_prop, 
                           focal_mm, sensor_mm, image_px, 
                           h_units = 'm') {
  gsd <- get_gsd(H_m, focal_mm, 
                 sensor_mm, 
                 image_px, 
                 h_units = 'm')
  
  gD <- ground_dist(image_px, gsd)
  
  return(gD)
}

drone_dist_H_o <- function(H_m, overlap_prop, 
                           focal_mm, sensor_mm, image_px, 
                           h_units = 'm') {
  gsd <- get_gsd(H_m, focal_mm, 
                 sensor_mm, 
                 image_px, 
                 h_units = 'm')
  
  gD <- ground_dist(image_px, gsd)
  
  dd <- drone_dist(gD, overlap_prop)
  
  return(dd)
}

v_H_o <- function(H_m, overlap_prop, p_s,
                  focal_mm, sensor_mm, image_px, 
                  h_units = 'm') {
  dd <- drone_dist_H_o(H_m, overlap_prop,
                       focal_mm, sensor_mm, image_px, 
                       h_units = 'm')
  v <- dd/p_s # could use velocity(dd/ps), but not worth it here.
  return(v)
}

t_H_o <- function(H_m, overlap_prop, v_ms,
                  focal_mm, sensor_mm, image_px, 
                  h_units = 'm') {
  dd <- drone_dist_H_o(H_m, overlap_prop,
                       focal_mm, sensor_mm, image_px, 
                       h_units = 'm')
  t <- dd/v_ms # could use velocity(dd/ps), but not worth it here.
  
  return(t)
}