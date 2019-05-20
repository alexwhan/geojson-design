


saveRDS(boorowa_nest, "saved_senaps_data.RDS")
temp_df <- boorowa_nest %>% 
  filter(property == "temperature")

krig1 <- kriging::kriging(coordinates(temp_df$coords[[1]])[,"longitude"], 
                          coordinates(temp_df$coords[[1]])[, "latitude"], temp_df$coords[[1]]$interval_10min_mean, lags = 3)
image(krig1)

krig_df <- temp_df %>% 
  tidyr::unnest(coords) %>% 
  mutate(
    krig = list(kriging::kriging(coordinates(unlist(.$coords))[, "longitude"],
                                 coordinates(unlist(.$coords))[, "latitude"],
                                 .$coords$interval_10min_mean,
                                 lags = 3))
  )

## boundary
boundary <- geojsonio::geojson_read("C:/projects/software/senaps/model-stdlib/interpolate/boundary.json", parse = TRUE, what = "sp")
boundary@proj4string <- CRS('+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs')

test <- purrr::map(temp_df$coords, ~ kriging::kriging(coordinates(.x)[, "longitude"],
                                                      coordinates(.x)[, "latitude"],
                                                      .x$interval_10min_mean,
                                                      lags = 3#,
                                                      # polygons = list(data.frame(
                                                      #   boundary@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                      #   boundary@polygons[[1]]@Polygons[[1]]@coords[,2]
))#))

image(test[[1]], xlim = extendrange(boundary@polygons[[1]]@Polygons[[1]]@coords[,1]), 
      ylim = extendrange(boundary@polygons[[1]]@Polygons[[1]]@coords[,2]))
temp_df <- test[[1]]$map
coordinates(temp_df) <- ~ x + y
temp_df@proj4string <- CRS('+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs')
image(temp_df)
plot(raster::intersect(temp_df, boundary))

bbox <- bbox(boundary)
boundary.grid <- expand.grid(x = seq(from = bbox[1], to = bbox[3], by = 0.0005), y = seq(from = bbox[2], to = bbox[4], by = 0.0005))
coordinates(boundary.grid) <- ~x + y
gridded(boundary.grid) <- TRUE
boundary.grid@proj4string <- boundary@proj4string

#create SpatialPixels grid
boundary.grid.stepped <- boundary.grid[!is.na(over(boundary.grid, boundary)),]
plot(boundary.grid.stepped)
