source('scripts/senaps.R')
time <- Sys.time()
attributes(time)$tzone <- "UTC"
update_nc("data/temperature.nc", "temperature", time, deployments)
