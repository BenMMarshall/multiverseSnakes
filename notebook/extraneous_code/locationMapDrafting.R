
library(ggplot2)
library(dplyr)
library(here)
library(sf)

library(rnaturalearth)
library(rnaturalearthhires)

core <- st_read(here("data", "SBR SHP", "SBRcore.shp"))
core <- st_transform(core, 4326)
buffer <- st_read(here("data", "SBR SHP", "Buffer zone.shp"))
transition <- st_read(here("data", "SBR SHP", "Transitional.shp"))

core <- (core %>%
                        select(name))
buffer <- (buffer %>%
                          select("name" = Name))
transition <- st_make_valid(transition %>%
                              select("name" = Name)) %>%
  st_cast("MULTIPOLYGON")

core$name <- "Core"
buffer$name <- "Buffer"
transition$name <- "Trans"

buffer <- st_zm(buffer, drop = TRUE, what = "ZM")
transition <- st_zm(transition, drop = TRUE, what = "ZM")

plot(transition)
# core <- st_buffer(core, 0.0)
# buffer <- st_buffer(buffer, 0.0)

SBRzones <- do.call(rbind, list(core,
        buffer,
        transition)
)
st_write(core, here("data", "SBRzones_core.geoJSON"),
         driver = "geoJSON", append = FALSE, , overwrite = TRUE,
         delete_dsn = TRUE)
st_write(buffer, here("data", "SBRzones_buffer.geoJSON"),
         driver = "geoJSON", append = FALSE, , overwrite = TRUE,
         delete_dsn = TRUE)
st_write(transition, here("data", "SBRzones_transition.geoJSON"),
         driver = "geoJSON", append = FALSE, , overwrite = TRUE,
         delete_dsn = TRUE)

zonesSF_core <- st_read(here("data", "SBRzones_core.geoJSON"))
zonesSF_buffer <- st_read(here("data", "SBRzones_buffer.geoJSON"))
zonesSF_transition <- st_read(here("data", "SBRzones_transition.geoJSON"))

roadsSF_SBR <- st_read(here("data", "SBR SHP", "Roads.shp"))
roadsSF_SBR <- st_transform(roadsSF_SBR, st_crs(4326))
roadsSF_SUT <- st_read(here("data", "SBR SHP", "Road_update25601102.shp"))
st_crs(roadsSF_SUT) <- st_crs(32648)
roadsSF_SUT <- st_transform(roadsSF_SUT, st_crs(4326))

st_write(roadsSF_SBR, here("data", "SBRroads.geoJSON"),
         driver = "geoJSON", append = FALSE, , overwrite = TRUE,
         delete_dsn = TRUE)
st_write(roadsSF_SUT, here("data", "SUTroads.geoJSON"),
         driver = "geoJSON", append = FALSE, , overwrite = TRUE,
         delete_dsn = TRUE)


# Prep above --------------------------------------------------------------------

targets::tar_load("movementData_OPHA_H1_binary")
targets::tar_load("movementData_PYBI_H1_binary")
targets::tar_load("movementData_BUCA_H1_binary")
targets::tar_load("movementData_BUFA_H1_binary")

targets::tar_source()

palList <- get_palette()

OPHAdata <- movementData_OPHA_H1_binary$movementData_sf
PYBIdata <- movementData_PYBI_H1_binary$movementData_sf
BUCAdata <- movementData_BUCA_H1_binary$movementData_sf
BUFAdata <- movementData_BUFA_H1_binary$movementData_sf

zonesSF_core <- st_read(here("data", "SBRzones_core.geoJSON"))
zonesSF_buffer <- st_read(here("data", "SBRzones_buffer.geoJSON"))
zonesSF_transition <- st_read(here("data", "SBRzones_transition.geoJSON"))

roadsSF_SBR <- st_read(here("data", "SBRroads.geoJSON"))
roadsSF_SUT <- st_read(here("data", "SUTroads.geoJSON"))

hypoOPHA <- st_read(here("data", "landuseOPHA.geoJSON"))
hypoPYBI <- st_read(here("data", "landusePYBI.geoJSON"))
hypoBUFA <- st_read(here("data", "landuseBUFA.geoJSON"))
hypoBUCA <- st_read(here("data", "landuseBUCA.geoJSON"))

ggplot() +
  geom_sf(data = hypoBUCA, aes(fill = habitat == "H1_Habitat"), alpha = 0.25,
          colour = NA) +
  geom_sf(data = roadsSF_SUT, alpha = 0.5) +
  geom_sf(data = BUCAdata, aes(colour = species), alpha = 0.5,
          size = 0.75) +
  coord_sf(
    xlim = range(BUCAdata$x), ylim = range(BUCAdata$y),
    expand = 0.1) +
  scale_fill_manual(values = c("white", unname(palList$corePalette[2]))) +
  scale_colour_manual(values = palList$speciesFullPalette) +
  theme_bw() +
  theme()

ggsave(here("notebook", "ext_images", "SUTMap.pdf"), width = 10, height = 10)

SBRmovementData <- do.call(rbind, list(OPHAdata, PYBIdata, BUFAdata))

ggplot() +
  geom_sf(data = hypoOPHA, aes(fill = habitat == "H1_Habitat"), alpha = 0.25,
          colour = NA) +
  geom_sf(data = hypoOPHA, aes(fill = habitat == "H2_Habitat"), alpha = 0.25,
          colour = NA) +
  geom_sf(data = hypoPYBI, aes(fill = habitat == "H1_Habitat"), alpha = 0.25,
          colour = NA) +
  geom_sf(data = hypoBUFA, aes(fill = habitat == "H1_Habitat"), alpha = 0.25,
          colour = NA) +
  geom_sf(data = roadsSF_SBR, alpha = 0.5) +
  geom_sf(data = SBRmovementData, aes(colour = species), alpha = 0.5,
          size = 0.25) +
  coord_sf(
    xlim = range(OPHAdata$x), ylim = range(OPHAdata$y),
    expand = 0.1) +
  scale_fill_manual(values = c("white", unname(palList$corePalette[2]))) +
  scale_colour_manual(values = palList$speciesFullPalette) +
  theme_bw() +
  theme()

ggsave(here("notebook", "ext_images", "SBRMap.pdf"), width = 10, height = 10)

ggplot() +
  geom_sf(data = hypoOPHA, aes(fill = habitat == "H1_Habitat"), alpha = 0.25,
          colour = NA) +
  geom_sf(data = hypoOPHA, aes(fill = habitat == "H2_Habitat"), alpha = 0.25,
          colour = NA) +
  geom_sf(data = hypoPYBI, aes(fill = habitat == "H1_Habitat"), alpha = 0.25,
          colour = NA) +
  geom_sf(data = hypoBUFA, aes(fill = habitat == "H1_Habitat"), alpha = 0.25,
          colour = NA) +
  geom_sf(data = roadsSF_SBR, alpha = 0.5) +
  geom_sf(data = SBRmovementData, aes(colour = species), alpha = 0.5,
          size = 0.75) +
  coord_sf(
    xlim = range(BUFAdata$x), ylim = range(BUFAdata$y),
    expand = 0.1) +
  scale_fill_manual(values = c("white", unname(palList$corePalette[2]))) +
  scale_colour_manual(values = palList$speciesFullPalette) +
  theme_bw() +
  theme()

ggsave(here("notebook", "ext_images", "SBRMap_BUFA.pdf"), width = 10, height = 10)


thaiAdmin <- ne_states(
  country = "Thailand",
  returnclass = "sf"
)
worldCoast <- ne_coastline(scale = "large", returnclass = "sf")
sfMap <- ne_countries(
  scale = "large",
  type = "countries",
  returnclass = "sf")


# SERS
# 14.50987551678215, 101.93098098152355

# SUT
# 14°52'45.3"N 102°01'11.8"E

studyLocation <- data.frame(
  long = c(101.93098098152355, 102.019944),
  lat = c(14.50987551678215, 14.87925),
  location = c("SERS", "SUT")) %>%
  st_as_sf(coords = c("long", "lat"), crs = st_crs(4326))

# worldData <- map_data("world")
# sfMap <- worldData %>%
#   st_as_sf(coords = c("long", "lat"), crs = st_crs(4326)) %>%
#   group_by(group) %>%
#   summarise(geometry = st_combine(geometry)) %>%
#   st_cast("POLYGON")

thaiAdmin$name

ggplot() +
  # geom_sf(data = worldCoast, fill = "grey", color = "black") +
  geom_sf(data = sfMap, fill = "grey", color = "black") +
  geom_sf(data = thaiAdmin %>% 
            filter(name == "Nakhon Ratchasima"), fill = "green") +
  geom_sf(data = thaiAdmin %>% 
            filter(name == "Bangkok Metropolis"), fill = "purple") +
  geom_sf(data = studyLocation, aes(colour = location)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_colour_manual(values = c("red", "blue")) +
  coord_sf(# crs = st_crs("+proj=moll +x_0=0 +y_0=0 +lat_0=0 +lon_0=133"),
    xlim = c(65, 140), ylim = c(-15, 40),
    expand = 0) +
  theme_bw() +
  theme()

ggsave(here("notebook", "ext_images", "WorldMap.pdf"), width = 10, height = 10)

