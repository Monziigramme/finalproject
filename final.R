setwd("C:/Users/momo_/OneDrive/Documents/GitHub/finalproject")
library(tidyverse)
library(sf)
library(janitor)
library(jsonlite)
library(rvest)
library(dplyr)

zippath <- "C:/Users/momo_/OneDrive/Documents/GitHub/finalproject"
zip <- paste0(zippath, "/Boundaries - Neighborhoods.zip")
unzip(zip,exdir=zippath)
nhood_shape <- st_read(file.path(zippath, "/geo_export_adb12c01-c30d-4573-b741-90bfc6a3f43f.shp"))

#chicagoneighbourhoods
ggplot() +
  geom_sf(data = nhood_shape, aes(fill = "#69b3a2"), show.legend = FALSE) +
  theme_minimal() +
  labs(title = "Neighbourhoods In Chicago")

#importing affordable housing units 
affordable_housing <- read.csv("Affordable_Rental_Housing_Developments_20241204 (1).csv") 
affordable_housing_clean <- affordable_housing|>
  janitor::clean_names()|>
  rename(Community = community_area_name,
         affordable_units_no = units) |>
  select(-c(property_name, address, phone_number, management_company))

#importing vacant lots in Chicago using JSON directly from the API

chicago_data <- fromJSON("https://data.cityofchicago.org/resource/7nii-7srd.json") |>
  janitor::clean_names() |>
  rename(vacant_or_occupied = is_the_building_currently_vacant_or_occupied,
         who_occupies = any_people_using_property_homeless_childen_gangs)

#the chicago dataset does not have the actual names of the communities. So,
#I will webscrape to get the tables 
url <- "https://en.wikipedia.org/wiki/Community_areas_in_Chicago"
response <- read_html(url)
table <- html_table(response, fill = TRUE)
View(table[[1]])

community_area <- table[[1]] |>
  select(c(No., Name)) |>
  mutate(No. = gsub("^0+", "", No.)) |>
  rename(community_area = No.,
         Community = Name)
community_area <- community_area[-c(1, 79), ]

#I will then merge vacant lots with the community areas to match their names
vacant_lots <- left_join(chicago_data, community_area, by = "community_area") |>
  filter(vacant_or_occupied == "Vacant", who_occupies == TRUE)|>
  group_by(Community) |>
  mutate(count_vacant = n()) |>
  select(c(Community, who_occupies, zip_code, latitude, longitude, count_vacant))

#checking crs before mapping vacant lots, affordable housing, and neighbourhood boundaries
st_crs(nhood_shape) == st_crs(vacant_lots)
st_crs(affordable_housing_clean) == st_crs(nhood_shape)
st_crs(affordable_housing_clean) == st_crs(vacant_lots)

#converting affordable housing and vacant lots to sf object
vacant_lots_sf <- st_as_sf(vacant_lots, coords = c("longitude", "latitude"), crs = 4326)
affordable_housing_sf <- st_as_sf(affordable_housing_clean, coords = c("Longitude", "Latitude"), crs = 4326)

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

ggsave("affordable_housing_plot.png", plot = affordable_housing_plot, width = 6, height = 4, dpi = 300)


#Step 2: Add vacant lots to the graph
vacant_lots_hist <- vacant_lots |>
  group_by(Community) |>
  head(15) |>
  ggplot(aes(x = reorder(Community, count), y = count_vacant)) +
  geom_histogram(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

vacant_lots_hist
ggsave("vacant_lots_hist.png", plot = vacant_lots_hist, width = 6, height = 4, dpi = 300)

#plot vacant lot on affordable housing plot
vacant_lot_housing_plot <- affordable_housing_plot +
  geom_sf(data = vacant_lots_sf, color = "lightblue", aes(shape = "Vacant Lots")) +
  scale_shape_manual(values = 15, name = NULL) +
  labs(title = "Affordable Housing Units and Vacant Lots in Chicago", 
       subtitle = "Latest Data as of July 2024")
 
vacant_lot_housing_plot

ggsave("vacant_lot_housing_plot.png", plot = vacant_lot_housing_plot, width = 6, height = 4, dpi = 300)


#Importing the socioeconomic dataset for communities to see the socioeconomic status 
socioec_data <- fromJSON("https://data.cityofchicago.org/resource/5kdt-irec.json") |>
  janitor::clean_names() |>
  rename(Community = community_area_name)

#merge vacant lot, affordable housing, and socieconomic data
clean_chicago_data <- socioec_data |>
  left_join(vacant_lots, by = "Community") |>
  left_join(affordable_housing_clean, by = "Community") |>
  janitor::clean_names()|>
  na.omit()

write.csv(clean_chicago_data, "C:/Users/momo_/OneDrive/Documents/GitHub/finalproject/clean_chicago_data.csv")

