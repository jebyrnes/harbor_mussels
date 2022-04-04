library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(ggmap)
library(ggridges)
library(patchwork)
library(readr)

plots <- dat <- read_csv("byrnes_data/intertidal_data.csv") 

dat <- dat %>%
  filter(`Species Code` == "MYED") %>%
  dplyr::select(Site, Quadrat, Height, 
                Square, Measurement) %>%
  group_by(Site, Quadrat, Height) %>%
  summarize(cover = sum(Measurement)) %>%
  ungroup() 

plots <- plots %>%
  select(Site, Quadrat, Height, Latitude, Longitude) %>%
  group_by(Site, Quadrat, Height, Latitude, Longitude) %>%
  slice(1L) %>%
  ungroup()

myed <- left_join(plots, dat) %>%
  replace_na(list(cover = 0)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  mutate(Height = factor(Height, levels = c("Low", "Mid", "High")))

  
harbor <- get_stamenmap(bbox = st_bbox(myed) %>% as.vector + 
                               c(-0.03,-0.03, 0.03, 0.03),
                             zoom = 14)
#BH Map
library(tigris)
coast <- readRDS("harbor_shapefile/boston_harbor.rds")

harbor_sf <- coast

harbor_map <- ggplot() +
  geom_sf(data = harbor_sf) +
  geom_sf(data = myed, aes(alpha = 0.5, color = Site, fill = Site), 
          inherit.aes = FALSE, size = 2) +
  theme_bw(base_size = 14) +
  guides(alpha = "none", color = "none", fill = "none") +
  labs(x = "", y = "") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = seq(-71.1, -70.8, length.out=4))+
  scale_y_continuous(breaks = seq(42.24, 42.4, length.out=4) %>% round(2)) +
  theme(panel.grid = element_blank())


zones <- myed %>%
  group_by(Site, Height) %>%
  summarize(beds = sum(cover>80),
            cover = mean(cover))

# Maps
ggmap(harbor) +
  geom_sf(data = zones, 
          aes(color = beds),
          inherit.aes = FALSE,
          size = 2) +
  scale_color_viridis_c()


ggmap(harbor) +
  geom_sf(data = zones, 
          aes(color = beds),
          inherit.aes = FALSE,
          size = 2) +
  scale_color_viridis_c()


# dist
dist_plot <- ggplot(myed) +
  geom_boxplot(aes(fill = Site, y = Height, x = cover), 
                      alpha = 0.4) +
  theme_bw(base_size = 14) +
  xlim(c(0,100)) +
  labs(x = "% Cover")+
  scale_fill_brewer(palette = "Dark2")

dist_plot_interval <- ggplot(myed) +
  ggdist::stat_pointinterval(aes(y = Height, x = cover, 
                                color = Site),  
                       position = position_dodge(width = 0.5)) +
  theme_bw(base_size = 14) +
  xlim(c(0,100)) +
  labs(x = "% Cover of Mussels")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2")

# put it together
harbor_map + dist_plot_interval +  plot_annotation(tag_levels = 'A')

ggsave("figures/map_intertidal.jpg", dpi = 800, height = 3, width = 7.5)


## Nucella
filt_count <- . %>%
  dplyr::select(Site, Quadrat, Height, 
                Square, Measurement) %>%
  group_by(Site, Quadrat, Height) %>%
  summarize(count = sum(Measurement)) %>%
  ungroup() 

dat %>%
  filter(`Species Code` == "NULA")%>%
  filt_count

#crabs 
dat %>%
  filter(`Species Code` == "CAMA")%>%
  filt_count

dat %>%
  filter(`Species Code` %in% c("CAIR", "CABO")) %>%
  filt_count

dat %>%
  filter(`Species Code` == "HESA")%>%
  filt_count
