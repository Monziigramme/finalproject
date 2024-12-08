library(tidyverse)
library(ggplot2)
library(sf)
library(sp)

zippath <- "C:/Users/momo_/OneDrive/Documents/GitHub/finalproject/"
nhood_shape <- st_read(file.path(zippath, "/geo_export_adb12c01-c30d-4573-b741-90bfc6a3f43f.shp"))
affordable_housing_clean <- read.csv(paste0(zippath, "/affordable_housing_clean.csv"))
vacant_lots <- read.csv(paste0(zippath, "/vacant_lots.csv"))


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

#plot vacant lot on affordable housing plot
vacant_lot_housing_plot <- affordable_housing_plot +
  geom_sf(data = vacant_lots_sf, color = "lightblue", aes(shape = "Vacant Lots")) +
  scale_shape_manual(values = 15, name = NULL) +
  labs(title = "Affordable Housing Units and Vacant Lots in Chicago", 
       subtitle = "Latest Data as of July 2024")

vacant_lot_housing_plot


ggsave(paste0(zippath, "/affordable_housing_plot.png"), plot = affordable_housing_plot, width = 6, height = 4, dpi = 300)
ggsave(paste0(zippath, "/vacant_lots_hist.png"), plot = vacant_lots_hist, width = 6, height = 4, dpi = 300)
ggsave(paste0(zippath, "vacant_lot_housing_plot.png"), plot = vacant_lot_housing_plot, width = 6, height = 4, dpi = 300)
