# finalproject



This is my code for the spatial regression. Unfortunately, it did not work even though I tried to transform the data into the specifications of a spatial regression. 
# I have to transform the data into polygons
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




