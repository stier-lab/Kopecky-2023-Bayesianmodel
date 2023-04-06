#----------------------------
## Spatial libraries
#----------------------------

library(ggplot2)
library(ggspatial)
library(tidyverse)
library(sf)


sp1 <- sf::st_read("data/spatial/Removal plot/2019/Plot11_2019_shape.shp") %>% st_as_sf()

sp2 <- sf::st_read("data/spatial/Removal plot/2022/Plot11_2022_shape.shp") %>% st_as_sf()

df <- sp1 %>%
  mutate(time = "t1") %>%
  bind_rows(sp2 %>% mutate(time = "t2")) %>%
  mutate(live_dead = ifelse(TL_Class == "Dead coral", "dead", "live"))


st_crs(df) <- "ENGCRS[\"Local Coordinates (m)\",\n    EDATUM[\"\"],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"Meter\",1]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"Meter\",1]]]"




buffer <- sp1 %>%
  mutate(time = "t1") %>%
  bind_rows(sp2 %>% mutate(time = "t2")) %>%
  mutate(live_dead = ifelse(TL_Class == "Dead coral", "dead", "live")) %>%
  filter(live_dead == "live") %>%
  st_buffer(dist = 0.01)



plot(df["live_dead"])

ggplot()+
  geom_sf(data = df)

