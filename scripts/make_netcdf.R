make_netcdf <- function(filename, krig_list, time, var_name) {
  # browser()
  mat_list <- map(krig_list, ~ matrix(.x@data@values, ncol = .x@ncols, nrow = .x@nrows, byrow = TRUE))
  latdim <- ncdim_def("latitude", "degrees_north", 
                      unique(coordinates(krig_list[[1]])[,"y"]))
  londim <- ncdim_def("longitude", "degrees_east", 
                      unique(coordinates(krig_list[[1]])[,"x"]))
  
  tunits <- "seconds since 1970-01-01 00:00:00.0 -0:00"
  timedim <- ncdim_def("time", tunits, as.numeric(time), unlim = TRUE)
  mat_array <- array(unlist(mat_list), c(krig_list[[1]]@ncols, krig_list[[1]]@nrows, length(mat_list)))

  ncvar <- ncvar_def(var_name, "var units", dim = list(latdim, londim, timedim), missval = NA)

  filecon <- nc_create(filename, ncvar)
  # filecon <- nc_open(filename, write = TRUE)
  ncvar_put(filecon, ncvar, mat_array)
  nc_close(filecon)
}

get_meta <- function(platform) {
  platform_dat <- RSenaps::get_platform(platform)
  streams <- list(platform_dat$streams)
  if(length(platform_dat$deployments) == 0) return(NULL)
  if(length(platform_dat$deployments) > 1) stop("One of the platforms has more than one deployment")
  location <- RSenaps::get_location(platform_dat$deployments[[1]]$location)
  
  deployment_start <- platform_dat$deployments[[1]]$validTime$start
  
  tibble::tibble(platform = platform,
                 deployment_start = deployment_start,
                 latitude = location$latitude,
                 longitude = location$longitude,
                 streams = streams)
}  

#df is deployment df
krig_from_deployments <- function(df, prop_string, start = NULL, end = NULL, limit = 1000, agg_period = "10 minutes", grid_points) {
  # browser()
  if(is.null(end)) end <- as.POSIXct(Sys.time(), tz = "UTC")
  boorowa_deployments <- df  %>% 
    filter(latitude > -40) %>% 
    tidyr::unnest(streams) %>% 
    filter(grepl(prop_string, streams)) %>% 
    rowwise %>% 
    mutate(stream_dat = list(get_stream(streams))) %>% 
    # browser()
    mutate(stream_obs = list(get_observations(streams, start = start, end = end, limit = limit)))  
  
  boorowa_deployment_dat <- boorowa_deployments %>% 
    tidyr::unnest(stream_obs) %>% 
    mutate(property = sub(".*\\.([a-z]+)$", "\\1", streams))
  
  earliest_period <- lubridate::ceiling_date(start, "10 minutes")
  
  aggregated_obs <- boorowa_deployment_dat %>% 
    filter(.$timestamp > earliest_period) %>% 
    mutate(agg_time = lubridate::ceiling_date(timestamp, agg_period)) %>% 
    group_by(platform, streams, property, agg_time, latitude, longitude) %>% 
    summarise(variable = mean(value))
  
  boorowa_nest <- aggregated_obs %>% 
    group_by(agg_time, property) %>% 
    tidyr::nest() %>% 
    rowwise %>% 
    mutate(coords = list(coord_fun(data)))
  
  krigs <- lapply(boorowa_nest$coords, function(x) {
    krig_fun(x, W = boundary, grid_points = grid_points)
  })
  return(list(krigs = krigs, agg_time = boorowa_nest$agg_time, property = boorowa_nest$property[1]))
  # output <- boorowa_nest %>% 
  #   ungroup %>% 
  #   mutate(krig = purrr::map(.$coords, krig_fun, W = boundary))
}

coord_fun <- function(df) {
sp::coordinates(df) <- ~ longitude + latitude
df <- df[-zerodist(df)[,1],]
return(df)
}

