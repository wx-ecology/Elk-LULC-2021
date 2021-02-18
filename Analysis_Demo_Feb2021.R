library(sf)
library(tidyverse)
library(raster)
library(ggplot2)
setwd("C:/Users/wenjing.xu/Google Drive/RESEARCH/Elk/GYE_ELK_LandUse")
target.crs <- st_crs("EPSG:32612")

# read range and corridor data
range <- read_sf(paste0(getwd(), "/data_clean/corridorRanges/rangeStopovers/all.shp")) %>% st_set_crs(target.crs)
corridor <- read_sf(paste0(getwd(), "/data_clean/corridorRanges/corridors/all.shp")) %>% st_set_crs(target.crs)

# protected status ------------
protected <- read_sf(paste0(getwd(), "/data_clean/Protected_Status/PROTECT_FULL_FINAL.shp")) 
range_protect <- st_join(range, protected)
