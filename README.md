# finalproject

#packages required
library(tidyverse)
library(janitor)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(sf)
library(sp)
library(spatialreg)
library(stargazer)
library(stringr)
library(udpipe)
library(tidytext)
library(rvest)

#The structure of my directory is as follows: fisrt create the following directories: data and images.

#For data sources setup:
-Download "Boundaries - Neighborhoods.zip" from Chicago Data Portal
-Download "Affordable_Rental_Housing_Developments_20241204.csv"
-Place these files in your project root directory

#How i executed my code
1. Data Collection and Cleaning (01_data_cleaning.R)
Set working directory path
Import and clean neighborhood boundaries
Import affordable housing data
Import vacant lots data via API
Import socioeconomic data
Merge datasets
2. Spatial Analysis (02_spatial_analysis.R)
Create spatial objects
Generate maps of affordable housing
Generate maps of vacant lots
Create combined visualization
3. Statistical Analysis (03_regression_analysis.R)
Calculate housing need ratio
Run regression models
Generate correlation plots
4. Text Analysis (04_text_analysis.R)
Scrape homeless count reports
Perform sentiment analysis
Generate sentiment plots

#Required Path Modifications
Users must modify the following paths in the scripts:
In all scripts, update this path to your local directory
zippath <- "YOUR_PATH_HERE/finalproject"

#Output
The code will generate:
1. Cleaned datasets in /data
2. Visualizations in /images
3. Text analysis results in /HomelessReports

#Data sets used in this analysis from Chicago Data Portal
1. Affordable Housing Units by Community Area - method was import CSV
2. Boundaries Neighborhoods - method used was importing zip
3. Vacant and Abandoned Buildings Reported - methods used was API and Webscraping (from wikipedia)
4. Hardship Index by Community Area - methods used was API


#This is my code for the spatial regression. Unfortunately, it did not work even though I tried to transform the data into the specifications of a spatial regression. 
 I have to transform the data into polygons
chicago_polygons <- clean_chicago_data_sf |>
  group_by(community) |>
  summarize(count_vacant = first(count_vacant),
            percent_of_housing_crowded = first(percent_of_housing_crowded),
            affordable_units_no = first(affordable_units_no),
            geometry = st_combine(geometry)) |>
  st_make_valid() |>
  st_buffer(0)

chicago_weights <- chicago_polygons |>
  st_cast("MULTIPOLYGON") |>
  poly2nb(snap = 1e-3) |>  # Increased snap distance to 0.01
  nb2listw(zero.policy = TRUE)

# Spatial lag model
spatial_lag <- lagsarlm(count_vacant ~ percent_of_housing_crowded + affordable_units_no, 
                        data = chicago_polygons,
                        listw = chicago_weights)

# Spatial error model
spatial_error <- errorsarlm(count_vacant ~ percent_of_housing_crowded,
                            data = spatial_data,
                            listw = chicago_weights)

# Test for spatial autocorrelation
moran <- moran.test(spatial_data$count_vacant, 
                    listw = chicago_weights,
                    zero.policy = TRUE)




