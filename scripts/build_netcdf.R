source('scripts/senaps.R')
krigs <- krig_from_deployments(deployments, prop_string = "temperature", 
                               start = lubridate::as_datetime("2019-05-20 01:00:00", tz = "UTC"), 
                               end = lubridate::as_datetime("2019-05-20 01:30:00", tz = "UTC"),
                               grid_points = 5000, agg_period = "10 minutes")
make_netcdf("data/temperature.nc", krigs$krigs, as.numeric(krigs$agg_time), krigs$property)
