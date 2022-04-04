library(sf)
library(dplyr)
library(ggplot2)
library(ggmap)

#pts_1 <- read_sf("kirk_data/GPS_points_day1_102320/GPS1_Ryan_ShellfishSurveyPoints_102320.shp")
#pts_2 <- read_sf("kirk_data/GPS_points_day1_102320/GPS2_Heidi_ShellfishSurveyPoints_102320.shp")

pts <- read_sf("rainsford_data/ShellfishSurvey_SamplePoints/ShellfishSurvey_SamplePoints.shp") %>%
  mutate(MusselBed = ifelse(is.na(MusselBed), "No", "Yes"),
         MusselBed = factor(MusselBed, levels = c("Yes", "No"))) %>%
  st_transform(crs = 4326)


rainsford <- get_stamenmap(bbox = st_bbox(pts) %>% as.vector + 
                             c(-0.001,-0.001, 0.001, 0.001),
                           zoom = 15)

ggmap(rainsford) + 
  geom_sf(data = pts %>% arrange(rev(MusselBed)), aes(color = MusselBed), inherit.aes = FALSE,
          alpha = 0.7) +
  scale_color_manual(values = c("orange", "darkBlue")) +
  #scale_color_viridis_d()+
  labs(color = "Mussel Bed?", x = "", y = "")

ggsave("figures/map.jpg", dpi = 500)


###

