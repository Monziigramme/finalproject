setwd("C:/Users/momo_/OneDrive/Documents/GitHub/finalproject")
library(tidyverse)
library(sf)
library(sp)
library(janitor)
library(jsonlite)
library(rvest)
library(dplyr)
library(spdep)
library(ggplot2)
library(spatialreg)
library(stargazer)
library(rvest)
library(stringr)
library(udpipe)
library(tidytext)




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

write.csv(clean_chicago_data, paste0(zippath, "/clean_chicago_data.csv")) #this clean dataset is primarily for the regression analysis 

#checking crs before mapping vacant lots, affordable housing, and neighbourhood boundaries; I will use the unmerged data sets to map the graphs because 
#the merged excludes areas with affordable housing and vacant lots due to NAs after merging. Moreover, the socioeconomic data does not have location points
#hence the NAs. 
st_crs(nhood_shape) == st_crs(vacant_lots)
st_crs(affordable_housing_clean) == st_crs(nhood_shape)
st_crs(affordable_housing_clean) == st_crs(vacant_lots)

#converting affordable housing and vacant lots to sf object
vacant_lots_sf <- st_as_sf(vacant_lots, coords = c("longitude", "latitude"), crs = 4326)
affordable_housing_sf <- st_as_sf(affordable_housing_clean, coords = c("longitude", "latitude"), crs = 4326)

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
  summarize(count_vacant = first(count_vacant)) |>
  arrange(desc(count_vacant)) |>
  head(15) |>
  ggplot(aes(x = reorder(Community, count_vacant), y = count_vacant)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Top 15 Communities with Vacant Lots in Chicago",
       x = "Communities",
       y = "Count")

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

#Step 3: I will run a spatial regression using socioeconomic data which helps me analyze the distribution of affordable housing developments and open lots across neighborhoods.
#I will first check the correlation matrix Select numeric columns of interest
numeric_columns <- clean_chicago_data[, c("hardship_index", "count_vacant", "affordable_units_no")]

# Calculate the correlation matrix
cor_matrix <- cor(numeric_columns, use = "complete.obs") # Excludes rows with NAs
print(cor_matrix)

# Save the correlation matrix as a CSV file
write.csv(cor_matrix, paste0(zippath, "/correlation_matrix.csv"), row.names = TRUE)

#I will then plot the correlation between affordable housing, vacant lots, and the hardship index
corr_plot_2 <- ggplot(clean_chicago_data, aes(x = hardship_index)) +
  # Plot for Count of Vacant Lots
  geom_point(aes(y = count_vacant), color = "blue") +
  geom_smooth(aes(y = count_vacant), method = "lm", color = "blue", se = TRUE) +
  # Plot for Affordable Units with transformation
  geom_point(aes(y = affordable_units_no / 10), color = "pink") +  # Scale down affordable units
  geom_smooth(aes(y = affordable_units_no / 10), method = "lm", color = "red", se = TRUE) +
  # Labels and second y-axis
  scale_y_continuous(
    name = "Count of Vacant Lots",
    sec.axis = sec_axis(~ . * 10, name = "Count of Affordable Units")
  ) +
  labs(
    title = "Hardship Index vs Count of Vacant Lots and Affordable Units",
    x = "Hardship Index"
  ) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "red"),
    axis.title.y.left = element_text(color = "blue")
  )
corr_plot_2

ggsave("corr_plot_2.png", plot = corr_plot_2, width = 6, height = 4, dpi = 300)


#for regression
clean_chicago_sf <- st_as_sf(clean_chicago_data, coords = c("longitude", "latitude"), crs = 4326)
clean_chicago_sf <- st_join(nhood_shape, clean_chicago_sf)

overcrowded_plot <- ggplot() +
  geom_sf(data = nhood_shape, size = 0.5) +  
  geom_sf(data = clean_chicago_data_sf, aes(color = percent_of_housing_crowded)) + 
  scale_color_gradient(low = 'orange', high = 'red') +
  theme_minimal() +
  labs(title = "Percent of Households Overcrowded in Chicago Neighborhoods 2024",
       color = "Total Percent") +
  theme(legend.position = "right")
overcrowded_plot

ggsave("overcrowded_plot.png", plot = overcrowded_plot, width = 6, height = 4, dpi = 300)

#I will create a ratio of overcrowding to affordable housing needs

clean_chicago_data <- clean_chicago_data |>
  mutate(housing_need_ratio = percent_of_housing_crowded/affordable_units_no)

#With this information, i will run a regression to with dependent variabel vacant lots and the needs ratio

vacant_reg <- lm(count_vacant ~ housing_need_ratio, data = clean_chicago_data)
summary(vacant_reg)

stargazer(vacant_reg, type = "text")
#then, I will control for other factors such as income, education, and age, which is captured in hardship index as well as property type
vacant_reg1 <- lm(count_vacant ~ housing_need_ratio + hardship_index + property_type, data = clean_chicago_data)
summary(vacant_reg1)

stargazer(vacant_reg1, type = "text")


#Text Analysis

url <- "https://www.chicago.gov/city/en/depts/fss/provdrs/emerg/svcs/PITcount.html"

#directory to save my links
output_dir <- paste0(zippath, "HomelessReports")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

#Then i need to scarpe the page using url
page <- read_html(url)

#Extract all links
links <- page %>%
  html_elements("a") %>%
  html_attr("href")

#Extract the text associated with links
titles <- page %>%
  html_elements("a") %>%
  html_text()

#Filter for links that contain "Homeless Point-In-Time Count and Survey Report" because some were filled with images and unable to be converted to text
filtered_links <- links[grepl("Homeless Point-In-Time Count and Survey Report", titles, ignore.case = TRUE)]
filtered_titles <- titles[grepl("Homeless Point-In-Time Count and Survey Report", titles, ignore.case = TRUE)]

#Ensure full URLs for relative links
base_url <- "https://www.chicago.gov"
full_links <- ifelse(grepl("^http", filtered_links), 
                     filtered_links, 
                     paste0(base_url, filtered_links))

# Function to extract text content from PDF files
pdf_to_text <- function(pdf_url, output_path) {
  temp_file <- tempfile(fileext = ".pdf")
  download.file(pdf_url, temp_file, mode = "wb")
  
  # Use pdftools to extract text
  library(pdftools)
  text_content <- pdf_text(temp_file)
  
  # Write to .txt file
  writeLines(text_content, con = output_path)
  
  unlink(temp_file)  # Remove temporary PDF file
}

# Download and save as .txt files
for (i in seq_along(full_links)) {
  # Create a safe file name
  file_name <- str_replace_all(filtered_titles[i], "[^a-zA-Z0-9]", "_")
  file_path <- file.path(output_dir, paste0(file_name, ".txt"))
  
  # Download and convert to text
  tryCatch({
    pdf_to_text(full_links[i], file_path)
    cat("Saved:", filtered_titles[i], "as .txt\n")
  }, error = function(e) {
    cat("Failed to process:", filtered_titles[i], "\n")
  })
}

articles <- list.files(output_dir, pattern = "_Homeless_Point_In_Time_Count_and_Survey_Report.txt")

#Loads text from all of the articles
article_text <- list()

for(i in 1:length(articles)) {
  article_text[[i]] <- read_delim(paste0(output_dir, "/", articles[i]),
                                  col_names = F, delim = ',,,,', col_types = cols())
}


article_text <- do.call(rbind, article_text) %>%
  as.data.frame()

chicago_udpipe <- udpipe(article_text$X1, "english") %>%
  filter(! upos %in% c("PUNCT", "SYM"))

#Removes stop words
chicago_udpipe <- chicago_udpipe %>%
  anti_join(stop_words, by = c("token" = "word"))

#Loads sentiment data
sentiment_nrc   <- get_sentiments("nrc") %>% rename(nrc = sentiment) %>% group_by(word) %>% slice(1) %>% ungroup() 
sentiment_afinn <- get_sentiments("afinn") %>% rename(afinn = value)
sentiment_bing  <- get_sentiments("bing") %>% rename(bing = sentiment)

#Merges udpipe with sentiment data
chicago_udpipe <- chicago_udpipe %>%
  left_join(sentiment_nrc, by = c('lemma' = 'word')) %>%
  left_join(sentiment_afinn, by = c('lemma' = 'word')) %>%
  left_join(sentiment_bing, by = c('lemma' = 'word'))

#overall sentiment
agg_sentiment_nrc <- chicago_udpipe %>%
  group_by(nrc) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(category = 'Overall')

agg_sentiment_afinn <- chicago_udpipe %>%
  group_by(afinn) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(category = 'Overall')

agg_sentiment_bing <- chicago_udpipe %>%
  group_by(bing) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(category = 'Overall')