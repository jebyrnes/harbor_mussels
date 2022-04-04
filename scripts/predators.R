library(googlesheets4)
library(ggplot2)
library(dplyr)
library(tidyr)

theme_set(theme_bw(base_size = 14))


subtidal <- read_sheet("https://docs.google.com/spreadsheets/d/17-SVkZlJ71wGVLLPe2W6TcIYieVlE3gDly0dCqXr9S4/edit#gid=619513821")

subtidal_long <- subtidal %>%
  pivot_longer(`0-5m`:`25-30m`) %>%
  replace_na(list(value = 0)) %>%
  mutate(num_per_sq_m = value/5) %>%
  filter(SP_Code %in% c("CAS", "TAAD", "LIEM", "HOAM", "LUHE", "ASFO"))
#cancer crab, cunner, spider crab, lobster, moon snail, sea star

ggplot(subtidal_long,
       aes(y = num_per_sq_m, x = Site)) +
  geom_boxplot() +
  geom_jitter(position = position_jitter(width = 0.2, height = 0),
              size = 3, alpha = 0.6) +
  labs(x = "", y = "# per sq. m.") +
  facet_wrap(vars(SP_Code), scale = "free_y")



ggplot(subtidal_long,
       aes(y = num_per_sq_m, x = Site, color = Site)) +
  ggdist::stat_dotsinterval() + coord_flip() +
  scale_color_brewer(palette = "Dark2")  +
  scale_fill_brewer(palette = "Dark2")  +
  labs(x="", y = "# per sq. m.")+
  facet_wrap(vars(SP_Code))

ggsave("figures/crabs.jpg", dpi = 800)
  

## Intertidal predators

plots <- dat <- read_csv("byrnes_data/intertidal_data.csv") 


plots <- plots %>%
  select(Site, Quadrat, Height, Latitude, Longitude) %>%
  group_by(Site, Quadrat, Height, Latitude, Longitude) %>%
  slice(1L) %>%
  ungroup()

# nucella
dat %>%
  filter(`Species Code` == "NULA") %>%
  dplyr::select(Site, Quadrat, Height, 
                Square, Measurement) %>%
  group_by(Site, Quadrat, Height) %>%
  summarize(cover = sum(Measurement)) %>%
  ungroup() 


dat %>%
  filter(`Species Code` == "ASFO") %>%
  dplyr::select(Site, Quadrat, Height, 
                Square, Measurement) %>%
  group_by(Site, Quadrat, Height) %>%
  summarize(cover = sum(Measurement)) %>%
  ungroup() 
