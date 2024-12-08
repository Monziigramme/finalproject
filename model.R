library(ggplot2)
library(spatialreg)
library(stargazer)
library(tidyverse)


zippath <- "C:/Users/momo_/OneDrive/Documents/GitHub/finalproject/"
clean_chicago_data <- read.csv(paste0(zippath, "/clean_chicago_data.csv"))

#Step 3: I will run a regression using socioeconomic data which helps me analyze the relationship between affordable housing developments and hardship across neighborhoods.

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

ggsave(paste0(zippath, "/corr_plot_2.png"), plot = corr_plot_2, width = 6, height = 4, dpi = 300)


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

ggsave(paste0(zippath, "/overcrowded_plot.png"), plot = overcrowded_plot, width = 6, height = 4, dpi = 300)

#I will create a ratio of overcrowding to affordable housing needs

clean_chicago_data <- clean_chicago_data |>
  mutate(housing_need_ratio = percent_of_housing_crowded/affordable_units_no)

#With this information, i will run a regression to with dependent variable vacant lots and the needs ratio

vacant_reg <- lm(count_vacant ~ housing_need_ratio, data = clean_chicago_data)
summary(vacant_reg)

stargazer(vacant_reg, type = "text", out = paste0(zippath, "/regression_1.txt"))

#then, I will control for other factors such as income, education, and age, which is captured in hardship index as well as property type
vacant_reg1 <- lm(count_vacant ~ housing_need_ratio + hardship_index + property_type, data = clean_chicago_data)
summary(vacant_reg1)

stargazer(vacant_reg1, type = "text", out = paste0(zippath, "/regression_2.txt"))