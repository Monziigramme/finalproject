setwd("C:/Users/momo_/OneDrive/Documents/GitHub/finalproject")
library(tidyverse)
library(janitor)
library(jsonlite)
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


#importing vacant lots in Chicago using JSON directly from the API but allowing toggle
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

#Importing the socioeconomic dataset for communities to incorporate the socioeconomic status in the regression
socioec_data <- fromJSON("https://data.cityofchicago.org/resource/5kdt-irec.json") |>
  janitor::clean_names() |>
  rename(Community = community_area_name)

#merge vacant lot, affordable housing, and socioeconomic data
clean_chicago_data <- socioec_data |>
  left_join(vacant_lots, by = "Community") |>
  left_join(affordable_housing_clean, by = "Community") |>
  janitor::clean_names()|>
  select(-c(who_occupies, location, x_coordinate, y_coordinate, zip_code_y, zip_code_x, latitude_y,
            longitude_y, ca)) |>
  na.omit()

#grouping the types of properties
clean_chicago_data |>
  count(property_type, name = "count")

clean_chicago_data <- clean_chicago_data |>
  mutate(property_type = case_when(
    property_type %in% c("Senior", "Senior HUD 202", "Senior Supportive Living") ~ "Senior",
    property_type == "ARO" ~ "ARO",
    property_type %in% c("Artist Housing", "Artist Live/Work Space", "Artist/Family", 
                         "Artists & Families", "Multifamily/Artists") ~ "Artist",
    property_type == "Inter-generational" ~ "Inter-generational",
    property_type %in% c("Multifamily", "Mutifamily", "Multfamily") ~ "Multifamily",
    property_type == "People with Disabilities" ~ "People with Disabilities",
    property_type %in% c("Supportive", "Supportive Housing", "Supportive/HIV/AIDS", 
                         "Supportive/Kinship Families", "Supportive/Teenage Moms", 
                         "Supportive/Veterans", "Supportive/Youth/Kinship Families") ~ "Supportive Housing",
    TRUE ~ "Other"
  ))

clean_chicago_data |>
  count(property_type, name = "count")

summary(clean_chicago_data)
#Some of the data need to be transformed to the correct data type

# Convert columns to appropriate data types
clean_chicago_data <- clean_chicago_data |>
  mutate(
    community = as.factor(community),                          
    community_area_number = as.numeric(community_area_number), 
    property_type = as.factor(property_type),                  
    percent_of_housing_crowded = as.numeric(percent_of_housing_crowded), 
    percent_households_below_poverty = as.numeric(percent_households_below_poverty), 
    percent_aged_16_unemployed = as.numeric(percent_aged_16_unemployed),
    percent_aged_25_without_high_school_diploma = as.numeric(percent_aged_25_without_high_school_diploma), 
    percent_aged_under_18_or_over_64 = as.numeric(percent_aged_under_18_or_over_64), 
    per_capita_income = as.numeric(per_capita_income),         
    hardship_index = as.numeric(hardship_index),              
    count_vacant = as.numeric(count_vacant),
    latitude_x = as.numeric(latitude_x),                      
    longitude_x = as.numeric(longitude_x)                     
  )|>
  rename(latitude = latitude_x,
         longitude = longitude_x)

#Check the structure of the dataset after conversions
str(clean_chicago_data)



data_folder <- paste0(zippath, "/data")

write.csv(chicago_data, paste0(data_folder, "/chicago_data.csv"))
write.csv(affordable_housing, paste0(data_folder, "/affordable_housing.csv"))
write.csv(socioec_data, paste0(data_folder, "/socioec_data.csv"))
write.csv(vacant_lots, paste0(data, "/vacant_lots.csv"))
write.csv(affordable_housing_clean, paste0(data_folder, "/affordable_housing_clean.csv"))
write.csv(clean_chicago_data, paste0(data_folder, "/clean_chicago_data.csv")) #this clean data set is primarily for the regression analysis 





