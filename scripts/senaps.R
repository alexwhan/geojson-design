library(settings)
library(httr)
library(jsonlite)
library(RSenaps)
library(dplyr)
library(purrr)
library(automap)
library(spatstat)
library(raster)
library(ncdf4)

senaps_options(
    sensor_url = Sys.getenv("SENAPS_URL"),
    apikey = Sys.getenv("SENAPS_APIKEY")
)
source("scripts/make_netcdf.R")
source("scripts/krig_fun.R")
source("scripts/update_netcdf_slice.R")

boundary <- geojsonio::geojson_read("data/boundary.json", parse = TRUE, what = "sp")
platforms <- get_platforms(groups = "boorowa-temp-humidity-deployed")



deployments <- map_df(platforms, get_meta)

##coords for deployments

deployments.coords <- deployments %>% 
  dplyr::select(platform, latitude, longitude)
coordinates(deployments.coords) <- ~ longitude + latitude


# update_nc("data/humidity.nc", "temperature", lubridate::as_datetime("2019-05-15 07:51:00"), deployments)

