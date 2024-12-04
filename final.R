setwd("C:/Users/momo_/OneDrive/Documents/GitHub/finalproject")
library(tidyverse)
library(sf)
library(janitor)
library(jsonlite)

zippath <- "C:/Users/momo_/OneDrive/Documents/GitHub/finalproject"
zip <- paste0(zippath, "/Boundaries - Neighborhoods.zip")
unzip(zip,exdir=zippath)
nhood_shape <- st_read(file.path(zippath, "/geo_export_adb12c01-c30d-4573-b741-90bfc6a3f43f.shp"))

#chicagoneighbourhoods
ggplot() +
  geom_sf(data = nhood_shape, aes(fill = "#69b3a2"), show.legend = FALSE) +
  theme_minimal() +
  labs(title = "Neighbourhoods In Chicago")

#importing vacant lots in Chicago using JSON directly from the API

chicago_data <- fromJSON("https://data.cityofchicago.org/resource/7nii-7srd.json") |>
  janitor::clean_names() |>
  rename(vacant_or_occupied = is_the_building_currently_vacant_or_occupied,
         who_occupies = any_people_using_property_homeless_childen_gangs)

community_area <- read.csv("CommAreas_20241204.csv") |>
  mutate(community_area = as.character(community_area)) |>
  rename(Community = COMMUNITY)

vacant_lots <- left_join(chicago_data, community_area, by = "community_area") |>
  filter(vacant_or_occupied == "Vacant", who_occupies == TRUE)|>
  select(c(Community, vacant_or_occupied, who_occupies, zip_code, latitude, longitude))

#importing affordable housing units 
affordable_housing <- read.csv("Affordable_Rental_Housing_Developments_20241204 (1).csv")


write.csv(actual_data, "C:/Users/momo_/OneDrive/Documents/GitHub/finalproject")

#checking crs before mapping vacant lots, affordable housing, and neighbourhood boundaries
st_crs(nhood_shape) == st_crs(vacant_lots)
st_crs(affordable_housing) == st_crs(nhood_shape)
st_crs(affordable_housing) == st_crs(vacant_lots)

#converting affordable housing and vacant lots to sf object
vacant_lots_sf <- st_as_sf(vacant_lots, coords = c("longitude", "latitude"), crs = 4326)
affordable_housing_sf <- st_as_sf(affordable_housing, coords = c("Longitude", "Latitude"), crs = 4326)

#Perform spatial join to associate affordable housing with neighborhoods
nhoods_affordable <- st_join(affordable_housing_sf, nhood_shape, join = st_intersects)

affordable_housing_count <- nhoods_affordable |>
  group_by(pri_neigh) |>
  summarise(affordable_housing_total = n()) |>
  mutate(affordable_housing_total = ifelse(is.na(affordable_housing_total), 0, affordable_housing_total))

# plot chicago neighbourhoods + affordable
affordable_housing_plot <- ggplot() +
    geom_sf(data = nhood_shape, size = 0.5) +  
    geom_sf(data = affordable_housing_count, aes(color = affordable_housing_total)) + 
    scale_color_gradient(low = 'lightpink', high = 'red') +
    theme_minimal() +
    labs(title = "Affordable Housing in Chicago Neighborhoods 2024",
         color = "Total Affordable Housing") +
    theme(legend.position = "right")

affordable_housing_plot


#Step 2: Add vacant lots to the graph
affordable_housing_plot +
  geom_sf(data = vacant_lots_sf, color = "blue", aes(shape = "Vacant Lots")) +
  scale_shape_manual(values = 15, name = NULL) +
  labs(title = "Affordable Housing Units and Vacant Lots in Chicago", 
       subtitle = "Latest Data as of July 2024")
