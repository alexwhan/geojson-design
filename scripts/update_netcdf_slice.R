update_nc <- function(filename, varid = "temperature", update_time = NULL, deployments, grid_points) {
  nc <- nc_open(filename, write = TRUE)
  # browser()
  if(ncatt_get(nc, "time") != "seconds since 1970-01-01 00:00:00.0 -0:00") {
    warning("Time units in netcdf do not match POSIXct")
  }
  
  if(!inherits(update_time, "POSIXct")) stop("update_time should be POSIXct")
  
  if(is.null(update_time))
    update_time <- lubridate::ymd_hms(Sys.time(), tz = "UTC")
  if(!lubridate::tz(lubridate::ymd_hms(update_time)) %in% c("UTC", "GMT")) warning("update_time tz is not UTC, be careful...")
  update_time <- as.numeric(update_time)
  times <- ncvar_get(nc, "time")
  
  latest_time <- times[length(times)]
  periodicity <- unique(diff(times))
  if(length(periodicity) != 1) stop("The netcdf time var is not monotonic")
  
  if((update_time - latest_time) > periodicity) {
    krigs <- krig_from_deployments(deployments, varid, 
                                   start = lubridate::as_datetime(latest_time), 
                                   end = lubridate::as_datetime(update_time),
                                   grid_points = grid_points)
    
  } else stop("The netcdf is up to date")
  # browser()
  mat_list <- map(krigs$krigs, ~ matrix(.x@data@values, ncol = .x@ncols, nrow = .x@nrows, byrow = TRUE))
  mat_array <- array(unlist(mat_list), c(krigs$krigs[[1]]@ncols, krigs$krigs[[1]]@nrows, length(mat_list)))
  
  index_start <- length(times) + 1
  index_end <- length(times) + length(krigs$agg_time)
  
  ncvar_put(nc, "time", krigs$agg_time, start = index_start, length(krigs$agg_time))
  ncvar_put(nc = nc, 
            varid = varid, 
            vals = mat_array[,,1:length(mat_list)], 
            start = c(1, 1, index_start), 
            count = c(-1, -1, length(krigs$agg_time)))
  nc_close(nc)
}
