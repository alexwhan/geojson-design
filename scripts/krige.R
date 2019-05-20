boundary <- geojsonio::geojson_read("C:/projects/software/senaps/model-stdlib/interpolate/boundary.json", parse = TRUE, what = "sp")
boundary@proj4string <- CRS('+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs')

#create bounding box grid
bbox <- bbox(boundary)
boundary.grid <- expand.grid(x = seq(from = bbox[1], to = bbox[3], by = 0.0005), y = seq(from = bbox[2], to = bbox[4], by = 0.0005))
coordinates(boundary.grid) <- ~x + y
gridded(boundary.grid) <- TRUE
boundary.grid@proj4string <- boundary@proj4string

#create SpatialPixels grid
boundary.grid.stepped <- boundary.grid[!is.na(over(boundary.grid, boundary)),]
plot(boundary.grid.stepped)

library(rgeos)
#cut grid with polygon to create SpatialPolygons grid
boundary.poly.grid <- as.SpatialPolygons.GridTopology(getGridTopology(boundary.grid), proj4string = CRS('+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs'))
boundary.grid.smooth <- gIntersection(boundary.poly.grid, boundary, byid=TRUE)
plot(boundary.grid.smooth)



library(rgdal)
library(tmap)

temp_df <- boorowa_nest$data[[100]] #%>% 
  # mutate(latitude_adj = latitude + rnorm(n(), 0, diff(range(latitude))/500),
  #        longitude_adj = longitude + rnorm(n(), 0, diff(range(longitude))/250))
# %>% 
#   dplyr::select(latitude, longitude, interval_10min_mean) %>% 
#   bind_rows(
#     tibble(latitude = runif(20, min(.$latitude), max(.$latitude)),
#            longitude = runif(20, min(.$longitude), max(.$longitude)),
#            interval_10min_mean = runif(20, min(.$interval_10min_mean), max(.$interval_10min_mean))
#   ))
coordinates(temp_df) <- ~ longitude + latitude
# Load precipitation data
z <- gzcon(url("http://colby.edu/~mgimond/Spatial/Data/precip.rds"))
P <- readRDS(z)
P <- temp_df
P <- P[-zerodist(P)[,1],]
P@proj4string <- CRS('+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs')

# Load Texas boudary map
z <- gzcon(url("http://colby.edu/~mgimond/Spatial/Data/texas.rds"))
W <- readRDS(z)
W <- boundary

# Replace point boundary extent with that of Texas
P@bbox <- W@bbox

tm_shape(W) + tm_polygons() +
  tm_shape(P) +
  tm_dots(col="interval_10min_mean", palette = "RdBu", auto.palette.mapping = FALSE,
          title="Sampled precipitation \n(in inches)", size=0.7) +
  tm_text("interval_10min_mean", just="left", xmod=.5, size = 0.7) +
  tm_legend(legend.outside=TRUE)

library(spatstat)  # Used for the dirichlet tessellation function
library(maptools)  # Used for conversion from SPDF to ppp
library(raster)    # Used to clip out thiessen polygons

# Create a tessellated surface
th  <-  as(dirichlet(as.ppp(P)), "SpatialPolygons")

# The dirichlet function does not carry over projection information
# requiring that this information be added manually
proj4string(th) <- proj4string(P)

# The tessellated surface does not store attribute information
# from the point data layer. We'll use the over() function (from the sp
# package) to join the point attributes to the tesselated surface via
# a spatial join. The over() function creates a dataframe that will need to
# be added to the `th` object thus creating a SpatialPolygonsDataFrame object
th.z     <- over(th, P, fn=mean)
th.spdf  <-  SpatialPolygonsDataFrame(th, th.z)

# Finally, we'll clip the tessellated  surface to the Texas boundaries
th.clp   <- raster::intersect(W,th.spdf)

# Map the data
tm_shape(th.clp) + 
  tm_polygons(col="interval_10min_mean", palette="RdBu", auto.palette.mapping=FALSE,
              title="Predicted precipitation \n(in inches)") +
  tm_legend(legend.outside=TRUE)

library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function

# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(P, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(grd) <- proj4string(P)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(interval_10min_mean ~ 1, P, newdata=grd, idp=2.0)

# Convert to raster object then clip to Texas
r       <- raster(P.idw)
r.m     <- mask(r, W)

# Plot
tm_shape(r.m) + 
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Predicted precipitation \n(in inches)") + 
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)


IDW.out <- vector(length = length(P))
for (i in 1:length(P)) {
  IDW.out[i] <- idw(interval_10min_mean ~ 1, P[-i,], P[i,], idp=2.0)$var1.pred
}

# Plot the differences
OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out ~ P$interval_10min_mean, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5))
abline(lm(IDW.out ~ P$interval_10min_mean), col="red", lw=2,lty=2)
abline(0,1)
par(OP)

# Implementation of a jackknife technique to estimate 
# a confidence interval at each unsampled point.

# Create the interpolated surface
img <- gstat::idw(interval_10min_mean~1, P, newdata=grd, idp=2.0)
n   <- length(P)
Zi  <- matrix(nrow = length(img$var1.pred), ncol = n)

# Remove a point then interpolate (do this n times for each point)
st <- stack()
for (i in 1:n){
  Z1 <- gstat::idw(interval_10min_mean~1, P[-i,], newdata=grd, idp=2.0)
  st <- addLayer(st,raster(Z1,layer=1))
  # Calculated pseudo-value Z at j
  Zi[,i] <- n * img$var1.pred - (n-1) * Z1$var1.pred
}

# Jackknife estimator of parameter Z at location j
Zj <- as.matrix(apply(Zi, 1, sum, na.rm=T) / n )

# Compute (Zi* - Zj)^2
c1 <- apply(Zi,2,'-',Zj)            # Compute the difference
c1 <- apply(c1^2, 1, sum, na.rm=T ) # Sum the square of the difference

# Compute the confidence interval
CI <- sqrt( 1/(n*(n-1)) * c1)

# Create (CI / interpolated value) raster
img.sig   <- img
img.sig$v <- CI /img$var1.pred 

# Clip the confidence raster to Texas
r <- raster(img.sig, layer="v")
r.m <- mask(r, W)

# Plot the map
tm_shape(r.m) + tm_raster(n=7,title="95% confidence interval \n(in inches)") +
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

f.2 <- as.formula(interval_10min_mean ~ X + Y + I(X*X)+I(Y*Y) + I(X*Y))

# Add X and Y to P
P$X <- coordinates(P)[,1]
P$Y <- coordinates(P)[,2]

# Define the 1st order polynomial equation
f.1 <- as.formula(interval_10min_mean ~ X + Y) 

# Add X and Y to P
P$X <- coordinates(P)[,1]
P$Y <- coordinates(P)[,2]

# Run the regression model
lm.1 <- lm( f.1, data=P)

# Use the regression model output to interpolate the surface
dat.1st <- SpatialGridDataFrame(grd, data.frame(var1.pred = predict(lm.1, newdata=grd))) 

# Clip the interpolated raster to Texas
r   <- raster(dat.1st)
r.m <- mask(r, W)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", auto.palette.mapping=FALSE, 
            title="Predicted precipitation \n(in inches)") +
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

f.1 <- as.formula(interval_10min_mean ~ X + Y) 

# Compute the sample variogram; note that the f.1 trend model is one of the
# parameters passed to variogram(). This tells the function to create the 
# variogram on the de-trended data.
var.smpl <- variogram(f.1, P, cloud = FALSE)

# Compute the variogram model by passing the nugget, sill and range values
# to fit.variogram() via the vgm() function.
dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill=1, model="Sph", range=0.1, nugget=0))

# The following plot allows us to assess the fit
plot(var.smpl, dat.fit)

# Define the trend model
f.1 <- as.formula(interval_10min_mean ~ X + Y) 

# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
dat.krg <- krige( f.1, P, grd, dat.fit)

# Convert kriged surface to a raster object for clipping
r <- raster(dat.krg)
r.m <- mask(r, W)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", auto.palette.mapping=FALSE, 
            title="Predicted precipitation \n(in inches)") +
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

