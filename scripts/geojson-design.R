library(geojsonio)
library(sp)
library(dplyr)
library(purrr)
library(readr)
library(trackeR)
library(XML)
library(sf)
poly <- geojsonio::geojson_read("data/polygon.json", method = "local")

cancoords <- readGPX("data/Waypoints_07-MAY-19_Alex_GDA94.gpx")

gpx.raw <- xmlTreeParse("data/Waypoints_07-MAY-19_Alex_GDA94.gpx", useInternalNodes = TRUE)
rootNode <- xmlRoot(gpx.raw)
gpx.rawlist <- xmlToList(gpx.raw)
coord_df <- map_df(gpx.rawlist[2:9],  ~ tibble(point = .x$extensions$label$label_text, lat = .x$.attrs[1], lon = .x$.attrs[2])) %>% 
  mutate_all(as.numeric)
  
polygon2 <- map(gpx.rawlist[6:9], ~ as.numeric(.x$.attrs)) 
polygon3 <- matrix(as.numeric(unlist(polygon2)), ncol = 2, byrow = TRUE)
polygon3.a <- cbind(polygon3[,2], polygon3[,1])
polygon4 <- list(rbind(polygon3.a, polygon3.a[1,])) %>% st_polygon()

geojson_write(polygon4, file = "data/canola_polygon.json")

canola_poly <- geojson_read("data/canola_polygon.json", method = "local")
furthest <- function(feature, axis = 1, reverse = FALSE) {
  coords <- feature$geometry$coordinates[[1]]
  coords_vec <- unlist(map(coords[1:(length(coords) - 1)], ~ .x[axis]))
  if(!reverse) return(which.min(coords_vec)) else which.max(coords_vec)
}

design <- read_csv("data/canola-design-uuid-banner.csv") %>% 
  select(id, ENTRY, ROW, RANGE) %>% 
  mutate(ROW = as.integer(ROW),
         RANGE = as.integer(RANGE))

design2 <- expand.grid(ROW = 1:3, RANGE = 1:4) %>% 
  as_tibble() %>% 
  mutate(ID = LETTERS[1:12], example_value = rnorm(12, mean = 5))
make_geojson <- function(design, poly, dim1_var = RUN, dim2_var = RANGE) {
  # browser()
  dim1_quo <- enquo(dim1_var)
  dim1_var <- deparse(substitute(dim1_var))
  dim2_quo <- enquo(dim2_var)
  dim2_var <- deparse(substitute(dim2_var))
  #check that dim1_var and dim2_var terms are in the design
  # stopifnot(all(c(dim1_var, dim2_var) %in% names(design)))
  
  #check that dim1_var and dim2_var terms are integers
  stopifnot(inherits(design[[dim1_var]], "integer"),
            inherits(design[[dim2_var]], "integer"))
  
  #check if the dim1_var and dim2_var terms give unique combinations
  stopifnot(nrow(design) == nrow(design[, c(dim1_var, dim2_var)]))
  
  ndim1_var <- max(design[[dim1_var]])
  ndim2_var <- max(design[[dim2_var]])
  
  #find the most extreme south, west, north points to calculate the deltas for dim1 and dim2
  south <- furthest(poly$features[[1]], 1)
  west <- furthest(poly$features[[1]], 2)
  north <- furthest(poly$features[[1]], 1, TRUE)

  coords <- poly$features[[1]]$geometry$coordinates[[1]]
  
  #calculate delta for each dim for x/y
  dim1x <- (coords[[west]][[1]] - coords[[south]][[1]]) / ndim1_var
  dim2x <- (coords[[north]][[1]] - coords[[west]][[1]]) / ndim2_var
  dim1y <- (coords[[west]][[2]] - coords[[south]][[2]]) / ndim1_var
  dim2y <- (coords[[north]][[2]] - coords[[west]][[2]]) / ndim2_var
  
  #all output coords are based on this point
  minx <- coords[[south]][[1]]
  miny <- coords[[south]][[2]]
  
  #A list used to make polygons
  corners <- list(
    c1 = c(1, 1),
    c2 = c(0, 1),
    c3 = c(0, 0),
    c4 = c(1, 0),
    c5 = c(1, 1)
  )
  # browser()
  design_sf <- design %>% 
    rowwise %>% 
    mutate(poly = list(make_poly(!!dim1_quo, !!dim2_quo, dim1x, dim2x, dim1y, dim2y, coords[[south]][[1]], coords[[south]][[2]], coords = corners))) %>% 
    sf::st_sf()
}

make_points <- function(dim1, dim2, dim1x, dim2x, dim1y, dim2y, refx, refy, moddim1 = 0, moddim2 = 0) {
  x <- (dim1 - moddim1) * dim1x + (dim2 - moddim2) * dim2x + refx
  y <- (dim1 - moddim1) * dim1y + (dim2 - moddim2) * dim2y + refy
  return(c(x = x, y = y))
}

make_poly <- function(dim1, dim2, dim1x, dim2x, dim1y, dim2y, refx, refy, coords = NULL) {
  stopifnot(!is.null(coords))
  coords_out <- map(coords, ~ make_points(dim1, dim2, dim1x, dim2x, dim1y, dim2y, refx, refy, moddim1 = .x[1], moddim2 = .x[2]))
  # browser()
  st_polygon(list(matrix(unlist(coords_out), ncol = 2, byrow = TRUE)))
}

canola_json <- make_geojson(design, canola_poly, RANGE, ROW)
geojson_write(canola_json, file = "data/canola-uuid.json")  

ex_json <- make_geojson(design2, poly, ROW, RANGE)
geojson_write(ex_json, file = "data/example.json")
