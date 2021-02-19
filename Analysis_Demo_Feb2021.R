library(sf)
library(tidyverse)
library(raster)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(USAboundaries) 
library(USAboundariesData)
setwd("C:/Users/wenjing.xu/Google Drive/RESEARCH/Elk/GYE_ELK_LandUse")
target.crs <- st_crs("EPSG:32612")
counties <- us_counties(states = c("Montana", "Wyoming", "Idaho"), resolution = "high") %>% st_transform(target.crs) 
area <- read_sf(paste0(getwd(), "/data/GYEsmallerExtent/GYE_Box_By__Range.shp")) %>% st_set_crs(target.crs)
counties <- st_join(counties, area, left = FALSE)

# read range and corridor data
range <- read_sf(paste0(getwd(), "/data_clean/corridorRanges/rangeStopovers/all.shp")) %>% 
  st_set_crs(target.crs) %>% 
  mutate (re_period = ifelse(period == "winter1" | period == "winter2", "winter", 
                       ifelse(period == "mig1stopover" | period == "mig2stopover", "stopover", "summer")))
range$area_km2 <- st_area(range)/1000000

corridor <- read_sf(paste0(getwd(), "/data_clean/corridorRanges/corridors/all.shp")) %>% 
  st_set_crs(target.crs) 
corridor$area_km2 <- st_area(corridor)/1000000
#corridor_summary <- range %>% group_by(herd, level) %>% summarise(area_km2 = as.numeric(sum(area_km2)))

## if we want area plot
# range_summary <- range %>% group_by(herd, re_period) %>% summarise(area_km2 = as.numeric(sum(area_km2)))
# range_summary_winter <- range_summary %>% filter (re_period == "winter") %>% arrange(area_km2) %>% 
#   mutate(herd = factor(herd, levels = herd)) 
# range_summary_winter$herd = with(range_summary_winter, reorder(herd, desc(area_km2)))
# range_summary_winter %>%
#   ggplot(aes(x = herd, y= area_km2)) +
#   geom_segment( aes(xend=herd, yend=0), size = 1.3, alpha=0.9) + 
# #  geom_point( size=4, color="orange") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  

# protected status - range ------------
protected <- read_sf(paste0(getwd(), "/data_clean/Protected_Status/PROTECT_FULL_FINAL.shp")) %>%
  mutate(DESIG_1 = ifelse(DESIG_1 == "National Park", "National_Park", 
                          ifelse(DESIG_1 == "Wilderness Study Area", "Wilderness_Study_Area", DESIG_1)))
range_protect <- st_join(range, protected) %>% 
  mutate (DESIG_1 = factor(DESIG_1, levels = c("National_Park", "Wilderness", "Wilderness_Study_Area", "Easement", "NOT_PROTECTED" )),
          re_period = factor(re_period, levels = c("winter", "stopover", "summer")))

range_protect$area_km2 <- as.numeric(st_area(range_protect)/1000000)
range_protect %>% 
  ggplot( aes(x = herd, y = area_km2, 
              fill = DESIG_1)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = c("#37B83F", "#6CDA96", "#90F3D4", "#E4E450", "#BBC8CA")) +
  facet_wrap(~re_period) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme(legend.position = "none")

range_summary <- range_protect %>%  st_set_geometry(NULL) %>% dplyr::select(-period) %>% group_by(herd, re_period, DESIG_1) %>%
  summarise(area_km2 = sum(area_km2)) %>%
  pivot_wider (id_cols = c(herd, re_period), names_from = DESIG_1, values_from = area_km2) 
range_summary[is.na(range_summary)] <- 0
range_summary <- range_summary %>% 
  mutate(perc_protected = sum(Easement, National_Park, Wilderness, Wilderness_Study_Area)/sum(NOT_PROTECTED, Easement, National_Park, Wilderness, Wilderness_Study_Area)) 

range_summary %>%
  ggplot(aes(x = herd, y= perc_protected, fill = re_period)) +
  geom_bar(stat="identity", position=position_dodge(), width=0.5) +
  scale_fill_manual(values=c('#8ff2e7','#E4E450', '#E5A6A6')) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

range_winter_geo <- left_join((range_summary_winter %>% dplyr::select(-area_km2)), (range_summary %>% filter(re_period == "winter")))
ggplot() +
  geom_sf(data = counties$geometry) +
  geom_sf(data = range_winter_geo, aes(fill = perc_protected), color = NA) +
  scale_fill_viridis(direction = -1)

# protected status - corridor ----------
corridor_protect <- st_join(corridor, protected)%>% 
  mutate(DESIG_1 = factor(DESIG_1, levels = c("National_Park", "Wilderness", "Wilderness_Study_Area", "Easement", "NOT_PROTECTED" )))
corridor_protect$area_km2 <- as.numeric(st_area(corridor_protect)/1000000)

corridor_protect %>% 
  ggplot( aes(x = herd, y = area_km2, 
              fill = DESIG_1)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = c("#37B83F", "#6CDA96", "#90F3D4", "#E4E450", "#BBC8CA")) +
  facet_wrap(~level) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme(legend.position = "none")
  
corridor_summary <- corridor_protect %>% st_set_geometry(NULL) %>% dplyr::select(-period) %>%
  group_by(herd, level, DESIG_1) %>%
  summarise(area_km2 = sum(area_km2))  %>%
  pivot_wider (id_cols = c(herd, level), names_from = DESIG_1, values_from = area_km2) 
corridor_summary[is.na(corridor_summary)] <- 0
corridor_summary <- corridor_summary  %>% 
  mutate(perc_protected = sum(Easement, National_Park, Wilderness, Wilderness_Study_Area)/sum(NOT_PROTECTED, Easement, National_Park, Wilderness, Wilderness_Study_Area)) 

corridor_summary %>%
  ggplot(aes(x = herd, y= perc_protected, fill = level)) +
  geom_bar(stat="identity", position=position_dodge(), width=0.5) +
  scale_fill_manual(values=c('#5CB93C', '#E3AD4F')) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

corridor_protect <- corridor_protect %>% dplyr::select(-period) %>% group_by(herd, level) %>% summarise(area_km2 = sum(area_km2))
corridor_mid_geo <- left_join((corridor_protect %>% filter(level == "mid") %>% dplyr::select(-area_km2)), (corridor_summary %>% filter(level == "mid")))

ggplot() +
  geom_sf(data = counties$geometry, alpha = 0.3) +
  geom_sf(data = corridor_mid_geo, aes(fill = perc_protected), color = NA) +
  scale_fill_viridis(direction = -1) +
  theme_minimal()
