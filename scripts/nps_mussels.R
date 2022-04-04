library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(sf)
library(tmap)

nps <- read_excel("nps_data/PhotoQuadrats_Cover_PROVISIONAL.xlsx") %>%
  filter(Site_Name=="Boston Harbor NRA") %>%
  mutate(year = year(Start_Date),
         month = month(Start_Date))

nps_mussels <- nps %>% filter(Spp_Code=="MUSSPP")

ggplot(nps_mussels,
       aes(x = year, y = Perc_Cover, group = Plot_Name)) +
  stat_summary() +
  stat_summary(fun = mean, geom = "line") +
  facet_wrap(vars(Loc_Name)) +
  theme_bw(base_size = 14) +
  labs(y = "Percent Cover of Mussels", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

max(nps_mussels$Perc_Cover, na.rm=TRUE)

nps_mussels

###
nps_loc <- read_excel("nps_data/Locations_PROVISIONAL.xlsx") %>%
  filter(Site_Name=="Boston Harbor NRA") %>%
  st_as_sf(coords = c("BM_UTM_East", "BM_UTM_North"),
           crs = "+proj=utm +zone=19") %>%
  st_transform(crs=4326)

tmap_mode("view")
tm_shape(nps_loc) +
  tm_symbols()
  
