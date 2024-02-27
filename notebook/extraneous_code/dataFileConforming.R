
library(sf)
library(here)
library(ggplot2)
library(dplyr)
library(stringr)


# BUCA --------------------------------------------------------------------

shapefilesNames <- list.files(here("data", "BUCA", "Landuse Shapefiles"), pattern = ".shp$",
                              full.names = TRUE)

polyIn <- lapply(shapefilesNames, read_sf)

plot(polyIn[[1]])
polyIn[[1]]$landuse <- "Agriculture"
polyIn[[1]] <- polyIn[[1]] %>% 
  select(landuse, geometry)
plot(polyIn[[2]])
polyIn[[2]]$landuse <- "Buildings"
polyIn[[2]] <- polyIn[[2]] %>% 
  select(landuse, geometry)
plot(polyIn[[3]])
polyIn[[3]]$landuse <- "Settlement"
polyIn[[3]] <- polyIn[[3]] %>% 
  select(landuse, geometry)
# plot(polyIn[[4]])
plot(polyIn[[5]])
polyIn[[5]]$landuse <- "Forest"
polyIn[[5]] <- polyIn[[5]] %>% 
  select(landuse, geometry)
plot(polyIn[[6]])
polyIn[[6]]$landuse <- "Plantation"
polyIn[[6]] <- polyIn[[6]] %>% 
  select(landuse, geometry)
# plot(polyIn[[7]])
plot(polyIn[[8]])
polyIn[[8]]$landuse <- "Semi-natural"
polyIn[[8]] <- polyIn[[8]] %>% 
  select(landuse, geometry)
# plot(polyIn[[9]])
plot(polyIn[[10]])
polyIn[[10]]$landuse <- "Water"
polyIn[[10]] <- polyIn[[10]] %>% 
  select(landuse, geometry)

combinedLanduse <- do.call(rbind, polyIn[unlist(lapply(polyIn, function(x){any(str_detect(names(x), "landuse"))}))])

unique(combinedLanduse$landuse)

combinedLanduse <- combinedLanduse %>% 
  mutate(habitat = case_when(
    landuse %in% c("Buildings", "Natural", "Settlements", "Semi-natural", "Forest") ~ "H1_Habitat",
    TRUE ~ "Other Habitats"
  ))

combinedLanduse %>% 
  ggplot() +
  geom_sf(aes(fill = habitat))

st_write(combinedLanduse, here("data", "landuseBUCA.geoJSON"),
         driver = "geoJSON", append = FALSE)

movementBUCA <- read.csv(here("data", "BUCA", "BUCA_data_complete.csv"))

movementBUCA <- movementBUCA %>% 
  select(species, "id" = alternate_id, x, y, datetime, UTMzone, sex) %>% 
  mutate(datetime = as.POSIXct(datetime, format = "%m/%d/%Y %H:%M", tz = "Asia/Bangkok"))

movementBUCA$id <- 
  paste0(substr(movementBUCA$id, 1,4), "0", substr(movementBUCA$id,5,6))

movementBUCA <- movementBUCA %>% 
  select(species, id, sex, x, y, UTMzone, datetime)

write.csv(movementBUCA, file = here("data", "movementBUCA.csv"),
          row.names = FALSE)

# BUFA --------------------------------------------------------------------

shapefilesNames <- list.files(here("data", "BUFA", "Landuse Shapefiles"), pattern = ".shp$",
                              full.names = TRUE)

polyIn <- lapply(shapefilesNames, read_sf)

plot(polyIn[[1]])
polyIn[[1]]$landuse <- "Canal"
polyIn[[1]] <- polyIn[[1]] %>% 
  select(landuse, geometry)

plot(polyIn[[2]])
polyIn[[2]]$landuse <- "Cassava"
polyIn[[2]] <- polyIn[[2]] %>% 
  select(landuse, geometry)

# plot(polyIn[[3]])

plot(polyIn[[4]])
polyIn[[4]]$landuse <- "Empty"
polyIn[[4]] <- polyIn[[4]] %>% 
  select(landuse, geometry)

plot(polyIn[[5]])
polyIn[[5]]$landuse <- "MarginsDykes"
polyIn[[5]] <- polyIn[[5]] %>% 
  select(landuse, geometry)
# plot(polyIn[[6]])
# polyIn[[6]]

# plot(polyIn[[7]])
# plot(polyIn[[8]])
plot(polyIn[[9]])
polyIn[[9]]$landuse <- "Plantation"
polyIn[[9]] <- polyIn[[9]] %>% 
  select(landuse, geometry)

plot(polyIn[[10]])
polyIn[[10]]$landuse <- "Pond"
polyIn[[10]] <- polyIn[[10]] %>% 
  select(landuse, geometry)

plot(polyIn[[11]])
polyIn[[11]]$landuse <- "Rice"
polyIn[[11]] <- polyIn[[11]] %>% 
  select(landuse, geometry)

plot(polyIn[[12]])
polyIn[[12]]$landuse <- "Roads"
polyIn[[12]] <- polyIn[[12]] %>% 
  select(landuse, geometry)

plot(polyIn[[13]])
polyIn[[13]]$landuse <- "Settlement"
polyIn[[13]] <- polyIn[[13]] %>% 
  select(landuse, geometry)

# plot(polyIn[[14]])
plot(polyIn[[15]])
polyIn[[15]]$landuse <- "Sugarcane"
polyIn[[15]] <- polyIn[[15]] %>% 
  select(landuse, geometry)

plot(polyIn[[16]])
polyIn[[16]]$landuse <- "Vegetable"
polyIn[[16]] <- polyIn[[16]] %>% 
  select(landuse, geometry)

combinedLanduse <- do.call(rbind, polyIn[unlist(lapply(polyIn, function(x){any(str_detect(names(x), "landuse"))}))])

unique(combinedLanduse$landuse)

combinedLanduse <- combinedLanduse %>% 
  mutate(habitat = case_when(
    landuse %in% c("Canal", "MarginsDykes") ~ "H1_Habitat",
    TRUE ~ "Other Habitats"
  ))

combinedLanduse %>% 
  ggplot() +
  geom_sf(aes(fill = habitat))

st_write(combinedLanduse, here("data", "landuseBUFA.geoJSON"),
         driver = "geoJSON", append = FALSE)

movementBUFA <- read.csv(here("data", "BUFA", "BUFA_movebankdata_1204811528.csv"))

movementBUFA <- movementBUFA %>% 
  select("species" = individual_taxon_canonical_name, "id" = individual_local_identifier,
         location_long, location_lat, "datetime" = timestamp) %>% 
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Bangkok"))

sfBUFA <- st_as_sf(movementBUFA, coords = c("location_long","location_lat"),
                   crs = 4326)

sfBUFA <- st_transform(sfBUFA, 32647)
coordBUFA <- st_coordinates(sfBUFA)

movementBUFA$x <- coordBUFA[,1]
movementBUFA$y <- coordBUFA[,2]

movementBUFA <- movementBUFA %>% 
  select(-location_long, -location_lat) %>% 
  mutate(UTMzone = "47N",
         sex = case_when(
           id == "BUFA01" ~ "M",
           TRUE ~ "F"
         ))

movementBUFA$id <- 
  paste0(substr(movementBUFA$id, 1,4), "0", substr(movementBUFA$id,5,6))

movementBUFA <- movementBUFA %>% 
  select(species, id, sex, x, y, UTMzone, datetime)

write.csv(movementBUFA, file = here("data", "movementBUFA.csv"),
          row.names = FALSE)

# PYBI --------------------------------------------------------------------

shapefilesNames <- list.files(here("data", "PYBI", "Landuse Shapefiles"), pattern = ".shp$",
                              full.names = TRUE)

polyIn <- lapply(shapefilesNames, read_sf)

plot(polyIn[[3]])

combinedLanduse <- polyIn[[3]] %>% 
  select("landuse" = R_LU_DE)

unique(combinedLanduse$landuse)

combinedLanduse <- combinedLanduse %>% 
  mutate(habitat = case_when(
    landuse %in% c("Water_Bodies", "Other_SemiNatural_Areas") ~ "H1_Habitat",
    TRUE ~ "Other Habitats"
  ))

combinedLanduse %>% 
  ggplot() +
  geom_sf(aes(fill = habitat))

dir.create(here("data", "PYBI", "Polygons"))

st_write(combinedLanduse, here("data", "landusePYBI.geoJSON"),
         driver = "geoJSON", append = FALSE)

movementPYBI <- read.csv(here("data", "PYBI", "Burmese%20Python_movement%20data.csv"))

movementPYBI <- movementPYBI %>% 
  mutate(species = "Python bivittatus") %>% 
  select(species, id,
         x, y, datetime) %>% 
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "Asia/Bangkok"))

movementPYBI <- movementPYBI %>% 
  mutate(UTMzone = "47N",
         sex = case_when(
           id == "PYBI028" ~ "M",
           TRUE ~ "F"
         )) %>% 
  select(species, id, sex, x, y, UTMzone, datetime)

write.csv(movementPYBI, file = here("data", "movementPYBI.csv"),
          row.names = FALSE)

# OPHA --------------------------------------------------------------------

shapefilesNames <- list.files(here("data", "OPHA", "Landuse Shapefiles"), pattern = ".shp$",
                              full.names = TRUE)

polyIn <- lapply(shapefilesNames, read_sf)

plot(polyIn[[2]])

unique(polyIn[[2]]$R_LU_DE)

combinedLanduse <- polyIn[[2]] %>% 
  select("landuse" = R_LU_DE) %>% 
  mutate(habitat = case_when(
    landuse == "Other_SemiNatural_Areas" ~ "H1_Habitat",
    landuse %in% c("Disturbed_Forest", "Dense_Deciduous_Forest", "Dense_Evergreen_Forest") ~ "H2_Habitat",
    TRUE ~ "Other Habitats"
  ))

combinedLanduse %>% 
  ggplot() +
  geom_sf(aes(fill = habitat))

dir.create(here("data", "OPHA", "Polygons"))

st_write(combinedLanduse, here("data", "landuseOPHA.geoJSON"),
         driver = "geoJSON", append = FALSE)

# movementOPHA_1 <- read.csv(here("data", "OPHA", "OPHA_movebankdata_556564170.csv"))

# unique(movementOPHA_1$individual_local_identifier)

movementOPHA_2 <- read.csv(here("data", "OPHA", "OPHA_movebankdata_1093796277.csv"))

movementOPHA_2 <- movementOPHA_2 %>% 
  filter(!individual_local_identifier == "") %>% 
  select("species" = individual_taxon_canonical_name, 
         "id" = individual_local_identifier,
         location_long, location_lat,
         "datetime" = timestamp
         ) %>% 
  mutate(
    sex = case_when(
      str_detect(id, "F") ~ "F",
      str_detect(id, "M") ~ "M"
    )) %>%
  mutate(id = sub("^..", "OPHA", id)) %>%
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Bangkok"))

opha2IDs <- unique(movementOPHA_2$id)

movementOPHA_3 <- read.csv(here("data", "OPHA", "OPHA_movebankdata_1649411628.csv"))

movementOPHA_3 <- movementOPHA_3 %>% 
  mutate(species = "Ophiophagus hannah") %>% 
  select(species, 
         "id" = tag_local_identifier,
         location_long, location_lat,
         "datetime" = timestamp
  ) %>% 
  mutate(
    sex = case_when(
      str_detect(id, "F") ~ "F",
      str_detect(id, "M") ~ "M"
    )) %>%
  mutate(id = sub("^..", "OPHA", id)) %>%
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Bangkok")) %>% 
  filter(!id %in% opha2IDs)

opha3IDs <- unique(movementOPHA_3$id)

# key2 <- movementOPHA_2 %>% 
#   mutate(uniKey = paste0(id, datetime)) %>% 
#   pull(uniKey)
# 
# unique(movementOPHA_2$id)
# 
# key3 <- movementOPHA_3 %>% 
#   mutate(uniKey = paste0(id, datetime)) %>% 
#   pull(uniKey)
# 
# movementOPHA_3 %>% 
#   mutate(uniKey = paste0(id, datetime)) %>% 
#   filter(uniKey %in% key2)
# 
# unique(movementOPHA_3$id)
# 
# sum(key3 %in% key2)
# 
# in2not3 <- movementOPHA_2 %>% 
#   mutate(uniKey = paste0(id, datetime)) %>% 
#   filter(!uniKey %in% key3) %>% 
#   # head(n = 2000)
#   pull(datetime) %>% range()
# 
# movementOPHA_3 %>% 
#   # mutate(uniKey = paste0(id, datetime)) %>% 
#   # filter(!uniKey %in% key3) %>% 
#   filter(datetime > in2not3[1], datetime < in2not3[2],
#          id == "AM006")
#   # pull(datetime) %>% range()
# 
# movementOPHA_3 %>% 
#   filter(id == "OPHA006") %>% 
#   arrange(datetime)
# movementOPHA_2 %>% 
#   filter(id == "OPHA006") %>% 
#   arrange(datetime)
# 
# movementOPHA_2 %>% 
#   mutate(uniKey = paste0(id, datetime)) %>% 
#   left_join(movementOPHA_3 %>% 
#               mutate(uniKey = paste0(id, datetime)),
#             by = "uniKey") %>% 
#   filter(!is.na(species.y))
# 
# movementOPHA_2[movementOPHA_2$id == "",]
# 
ggplot() +
  geom_point(data = movementOPHA_2, aes(x = datetime, y = id),
             position = position_nudge(y = -0.1), colour = "red") +
  geom_point(data = movementOPHA_3, aes(x = datetime, y = id),
             position = position_nudge(y = 0.1), colour = "blue")
# 
#   ggplot(blankID) +
#   geom_point(data = movementOPHA_3, aes(x = location_long, y = location_lat,
#                                         colour = id)) +
#   geom_point(aes(x = location_long, y = location_lat),
#              colour = "red",
#              alpha = 1, shape = 4) +
#   coord_cartesian(xlim = range(blankID$location_long), ylim = range(blankID$location_lat))

movementOPHA_4 <- read.csv(here("data", "OPHA", "olddata.csv"))

unique(movementOPHA_4$Snake.ID)

movementOPHA_4 <- movementOPHA_4 %>% 
  mutate(species = "Ophiophagus hannah",
         datetime = paste(CONSOLDIATED.DATES, CONSOLIDATED.TIMES)) %>% 
  select(species, 
         "id" = Snake.ID,
         "x" = Easting, "y" = Northing,
         datetime
  ) %>% 
  mutate(
    sex = case_when(
      str_detect(id, "1|3|4|8|10|12") ~ "F",
      TRUE ~ "M"
    )) %>%
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Bangkok")) %>% 
  filter(!id %in% opha2IDs, !id %in% opha3IDs)

movementOPHA_4 <- movementOPHA_4 %>%
  arrange(datetime) %>% 
  tidyr::fill(x, y, .direction = "down")

ggplot() +
  geom_point(data = movementOPHA_4, aes(x = datetime, y = id),
             position = position_nudge(y = -0.1), colour = "green") +
  geom_point(data = movementOPHA_2, aes(x = datetime, y = id),
             position = position_nudge(y = -0.1), colour = "red") +
  geom_point(data = movementOPHA_3, aes(x = datetime, y = id),
             position = position_nudge(y = 0.1), colour = "blue")

newOPHAData <- rbind(movementOPHA_2, movementOPHA_3)

sfOPHA <- st_as_sf(newOPHAData, coords = c("location_long","location_lat"),
                   crs = 4326)

sfOPHA <- st_transform(sfOPHA, 32647)
coordOPHA <- st_coordinates(sfOPHA)

newOPHAData$x <- coordOPHA[,1]
newOPHAData$y <- coordOPHA[,2]

movementOPHA <- rbind(newOPHAData %>% 
        select(-location_long, -location_lat),
      movementOPHA_4) %>% 
  mutate(UTMzone = "47N") %>% 
  arrange(id, datetime) %>% 
  select(species, id, sex, x, y, UTMzone, datetime)

write.csv(movementOPHA, file = here("data", "movementOPHA.csv"),
          row.names = FALSE)
