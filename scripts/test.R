library(ncdf4)

humvar <- ncvar_def("humidity", "something", list(londim, latdim, timedim))

nc_out <- nc_create("data/test.nc", list(tempvar, humvar), force_v4 = TRUE)

ncvar_put(nc_out, tempvar, temp)
ncvar_put(nc_out, humvar, hum)

nc_out
nc_close(nc_out)

library(ncdf.tools)

mat <- matrix(r.m@data@values, ncol = r.m@ncols, nrow = r.m@nrows, byrow = TRUE)
mat2 <- matrix(rnorm(r.m@ncols * r.m@nrows), ncol = r.m@ncols, nrow = r.m@nrows)
ncdf.tools::createLatLongTime(file.name = "data/please9.nc", var.names = c("humidity"), lat.values = unique(coordinates(r.m)[,"y"]), long.values = unique(coordinates(r.m)[,"x"]),
                              time.values = boorowa_nest$interval_10min[1])
please <- nc_open("data/please9.nc", write = TRUE)
ncvar_put(please, humvar, mat)
ncvar_put(please, tempvar, mat2)
nc_close(please)

colnames(mat) <- paste0("X.", lat.values)

mat_fun <- function(raster) {
  mat <- matrix(raster@data@values, ncol = raster@ncols, nrow = raster@nrows, byrow = TRUE)
}

aargh <- boorowa_nest %>% 
  ungroup %>% 
  filter(property == "temperature") %>% 
  # filter(row_number() %in% c(1, 51, 101, 151, 201)) %>% 
  mutate(krig = purrr::map(.$coords, krig_fun, W = boundary)) %>% 
  mutate(krig_mat = purrr::map(.$krig, mat_fun))

nc_create("data/write_test.nc", tempvar_1)
bars_write <- nc_open("data/write_test.nc", write = TRUE)
ncvar_put(bars_write, "temperature", mat_array[,,1:200])

bars_write2 <- nc_open("data/write_test2.nc", write = TRUE)
ncatt_put(bars_write2, "temperature", "repository", "https://github.com", "text")
ncatt_put(bars_write2, "temperature", "doi", "https://doi.org/10.25919/5be5086a6fda1", "text")
ncvar_put(bars_write2, "time", aargh$interval_10min[201:295], start = 201, 95)
ncvar_put(bars_write2, "temperature", mat_array[,,201:295], start = c(1, 1, 201), c(-1, -1, 95))
nc_close(bars_write2)
#how to extend time dim?
d1 <- boorowa_nest[rep(4, times = 10),] %>% 
  ungroup %>% 
  filter(property == "humidity") %>% 
  # filter(row_number() %in% c(1, 51, 101, 151, 201)) %>% 
  mutate(krig = purrr::map(.$coords, krig_fun, W = boundary)) 

map_dbl(d1$krig, ~ .x[[1]]@ncols)
