krig_fun <- function(P, W, grid_points = 5000) {
  # browser()
  # P <- P[-zerodist(P)[,1],]
  P@proj4string <- sp::CRS('+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs')
  
  # Load Texas boudary map
  
  # Replace point boundary extent with that of Texas
  P@bbox <- W@bbox
  
  # Create a tessellated surface
  th  <-  methods::as(spatstat::dirichlet(as.ppp(P)), "SpatialPolygons")
  
  # The spatstat::dirichlet function does not carry over projection information
  # requiring that this information be added manually
  sp::proj4string(th) <- sp::proj4string(P)
  
  # The tessellated surface does not store attribute information
  # from the point data layer. We'll use the over() function (from the sp
  # package) to join the point attributes to the tesselated surface via
  # a spatial join. The over() function creates a dataframe that will need to
  # be added to the `th` object thus creating a SpatialPolygonsDataFrame object
  th.z     <- sp::over(th, P, fn=mean)
  th.spdf  <-  sp::SpatialPolygonsDataFrame(th, th.z)
  
  # Finally, we'll clip the tessellated  surface to the Texas boundaries
  th.clp   <- raster::intersect(W,th.spdf)
  
  # Map the data
  # Create an empty grid where n is the total number of cells
  set.seed(123)
  grd              <- as.data.frame(spsample(P, "regular", n=grid_points))
  names(grd)       <- c("X", "Y")
  sp::coordinates(grd) <- c("X", "Y")
  sp::gridded(grd)     <- TRUE  # Create SpatialPixel object
  sp::fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  sp::proj4string(grd) <- sp::proj4string(P)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  P.idw <- gstat::idw(variable ~ 1, P, newdata=grd, idp=2.0)
  
  
  # Implementation of a jackknife technique to estimate 
  # a confidence interval at each unsampled point.
  
  # Create the interpolated surface
  img <- gstat::idw(variable~1, P, newdata=grd, idp=2.0)
  n   <- length(P)
  Zi  <- matrix(nrow = length(img$var1.pred), ncol = n)
  
  # Remove a point then interpolate (do this n times for each point)
  st <- raster::stack()
  for (i in 1:n){
    Z1 <- gstat::idw(variable~1, P[-i,], newdata=grd, idp=2.0)
    st <- raster::addLayer(st,raster(Z1,layer=1))
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
  
  # Add X and Y to P
  P$X <- coordinates(P)[,1]
  P$Y <- coordinates(P)[,2]
  
  # Define the 1st order polynomial equation
  f.1 <- as.formula(variable ~ X + Y) 
  
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
  
  f.1 <- as.formula(variable ~ X + Y) 
  
  # Compute the sample variogram; note that the f.1 trend model is one of the
  # parameters passed to variogram(). This tells the function to create the 
  # variogram on the de-trended data.
  var.smpl <- gstat::variogram(f.1, P, cloud = FALSE)
  
  # Compute the variogram model by passing the nugget, sill and range values
  # to fit.variogram() via the vgm() function.
  # browser()
  mod <- gstat::vgm(psill=1, model="Sph", range=0.1, nugget=0)
  # dat.fit  <- gstat::fit.variogram(object = var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                           # model = mod)
  # Define the trend model
  
  # Perform the krige interpolation (note the use of the variogram model
  # created in the earlier step)
  # browser()
  dat.krg <- gstat::krige(f.1, P, grd, mod)
  
  # Convert kriged surface to a raster object for clipping
  r <- raster(dat.krg)
  # browser()
  r.m <- mask(r, W)
  return(r.m)
}

